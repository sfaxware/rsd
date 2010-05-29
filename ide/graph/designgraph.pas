unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CodeCache, GraphComponents;

type
  TCGraphDesign = class(TScrollBox)
  private
    FMagnification: Real;
    procedure MouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    SimCodeBuffer: TCodeBuffer;
    SelectedBlock:TCGraphBlock;
    SelectedInputPort: TCGraphInputPort;
    SelectedOutputPort: TCGraphOutputPort;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CreateNewBlock: TCGraphBlock; virtual;
    function GetUpdatedDescription: string;
    function Load: Boolean;
    function Load(Path: string): Boolean;
    function Save: Boolean;
    procedure ConnectPorts(Sender: TObject);
    procedure DestroyBlock(var Block: TCGraphBlock);
    procedure SelectBlock(Sender: TObject);
  end;
  TScrollBox = class(TCGraphDesign);

implementation
uses
  Controls, Graphics, LFMTrees, CodeToolManager, CodeWriter,
  Magnifier;
                        
constructor TCGraphDesign.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //WriteLn('Created new TCGraphDesign class instance');
  OnMouseWheel := @MouseWheele;
  FMagnification := 1;
end;

destructor TCGraphDesign.Destroy;
begin
  inherited Destroy;
end;

procedure TCGraphDesign.ConnectPorts(Sender: TObject);
var
  Connector: TCGraphConnector;
begin
  Connector := TCGraphConnector.Create(Self);
  with Connector do begin
    Parent := Self;
    Connect(SelectedOutputPort, SelectedInputPort);
  end;
end;

function TCGraphDesign.CreateNewBlock:TCGraphBlock;
var
  BlockQuantity: integer = 0;
  R: TRect;
  w, h: Integer;
begin
  Result := TCGraphBlock.Create(Self);
  R := Result.OriginalBounds;
  with R do begin
    w := Right - Left;
    h := Bottom - Top;
    Left := Random(Width - w);
    Top := Random(Height - h);
    Right := Left + w;
    Bottom := Top + h;
  end;
  with Result do begin
    OriginalBounds := R;
    Parent := Self;
    BlockQuantity += 1;
    repeat
      try
        Name := 'Block' + IntToStr(BlockQuantity);
        break;
      except
        BlockQuantity += 1;
      end;
    until false;
    Caption := 'Block ' + IntToStr(BlockQuantity);
    OnClick := @SelectBlock;
    OnDblClick := Self.OnDblClick;
    PopupMenu := Self.PopupMenu;
    Selected := True;
  end;
end;

function TCGraphDesign.GetUpdatedDescription: string;
var
  Component: TComponent;
  i: Integer;
begin
  Result := 'object ' + Name + 'Simulator: TCustomDesign' + LineEnding;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphConnector then with Component as TCGraphConnector do begin
      Result += GetUpdatedDescription;
    end else if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      Result += GetUpdatedDescription;
    end;
  end;
  Result += 'end' + LineEnding;
end;

procedure TCGraphDesign.DestroyBlock(var Block: TCGraphBlock);
begin
  FreeAndNil(Block);
end;

procedure TCGraphDesign.MouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
  Control: TControl;
  m: Real;
  dx, dy, dm: Integer;
begin
  Handled := True;
  with MousePos do begin
    if WheelDelta > 0 then begin
      dm := 10;
    end else begin
      dm := -10;
    end;
    dx := x div dm;
    dy := y div dm;
  end;
  m := FMagnification + 1 / dm;
  with HorzScrollBar do begin
    Range := Round(Width * m);
  end;
  with VertScrollBar do begin
    Range := Round(Height * m);
  end;
  for i := 0 to ControlCount - 1 do begin
    Control := Controls[i];
    if Control is TCGraphBlock then with Control as TMagnifier do begin
      Magnify(m);
    end;
  end;
  FMagnification := m;
  ScrollBy(dx, dy);
end;

procedure TCGraphDesign.SelectBlock(Sender: TObject);
begin
  if Sender is TCGraphBlock then begin
     if Assigned(SelectedBlock) then
       SelectedBlock.Selected := False;
    SelectedBlock := Sender as TCGraphBlock;
    SelectedBlock.Selected := True;
  end;
end;

function TCGraphDesign.Load: boolean;
var
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
begin
  codeFile[ctSource] := DesignDir + '/' + Name + '.pas';
  codeFile[ctDescription] := DesignDir + '/' + Name + '.lfm';
  for CodeType := Low(CodeType) To High(CodeType) do begin
    //WriteLn(CodeFile[CodeType]);
    if Assigned(CodeBuffer[CodeType]) then
      CodeBuffer[CodeType].Reload
    else begin
      GetCodeBuffer(CodeFile[CodeType], cttNone,Self, CodeBuffer[CodeType]);
    end;
  end;
  Result := true;
end;

function TCGraphDesign.Load(Path: string): Boolean;
var
  DesignDescription: TLFMTree;
  BlockDescription: TLFMObjectNode;
  PortName: string;
  BlockName: string;
  p: Integer;
begin
  if Load() then with CodeToolBoss do begin
    //WriteLn('TCGraphDesign.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    //WriteLn('TCGraphDesign.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False) then
      Exit(False);
  end;
  //WriteLn('TCGraphDesign.Load : LFM created');
  BlockDescription := FindObjectProperty(nil, DesignDescription);
  while Assigned(BlockDescription) do begin
    //WriteLn('BlockDescription.TypeName = ', BlockDescription.TypeName);
    if BlockDescription.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(BlockDescription, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('OutputPortName = ', PortName);
      SelectedOutputPort := FindComponent(BlockName).FindComponent(PortName) as TCGraphOutputPort;
      PortName := GetPropertyValue(BlockDescription, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('InputPortName = ', PortName);
      SelectedInputPort := FindComponent(BlockName).FindComponent(PortName) as TCGraphInputPort;
      ConnectPorts(Self);
    end else begin
      if Assigned(SelectedBlock) then
        SelectedBlock.Selected := False;
      SelectedBlock := TCGraphBlock.Create(Self);
      with SelectedBlock do begin
        Parent := Self;
        Name := BlockDescription.Name;
        Load(DesignDescription, BlockDescription);
        OnClick := @SelectBlock;
        OnDblClick := Self.OnDblClick;
        PopupMenu := Self.PopupMenu;
      end;
      //WriteLn('++++++++++++++');
    end;
    BlockDescription := FindObjectProperty(BlockDescription, DesignDescription);
  end;
  DesignDescription.Free;
end;

function TCGraphDesign.Save: Boolean;
var
  Component: TComponent;
  i: Integer;
  CodeFileName: string;
  f: System.Text;
begin
  //WriteLn('FileName = ', DesignDir + '/' + Name + '.lfm');
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      Save;
    end; 
  end;
  CodeFileName := DesignDir + Name + '.lfm';
  GetCodeBuffer(CodeFileName, cttNone,Self, CodeBuffer[ctDescription]);
  CodeBuffer[ctDescription].Source := GetUpdatedDescription;
  Result := CodeBuffer[ctDescription].Save;
  CodeFileName := DesignDir + '/Simulate' + Name + '.pas';
  GetCodeBuffer(CodeFileName, cttSimulator, Self, SimCodeBuffer);
  Result := Result and SimCodeBuffer.Save;
  CodeFileName := DesignDir + '/' + Name + '.pas';
  GetCodeBuffer(CodeFileName, cttDesign, Self, CodeBuffer[ctSource]);
  UpdateUsedBlocks(Self, CodeBuffer[ctSource]);
  Result := Result and CodeBuffer[ctSource].Save;
end;

{procedure Register;
begin
  RegisterComponents('GraphDesign', [TCGraphDesign]);
end;

initialization
  RegisterClass(TCGraphDesign);}
end.
