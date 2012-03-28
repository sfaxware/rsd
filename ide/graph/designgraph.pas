unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CodeCache, GraphComponents;

type
  TCGraphDesign = class(TScrollBox, TIGraphDevice)
  private
    FMagnification: Real;
    FOnChildrenCreate: TNotifyEvent;
    procedure HandleMouseEnter(Sender: TObject);
    procedure HandleMouseLeave(Sender: TObject);
    procedure MouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    SimCodeBuffer: TCodeBuffer;
    PointedDevice : TCGraphDevice;
    SelectedBlock:TCGraphBlock;
    SelectedInputPort: TCGraphInputPort;
    SelectedOutputPort: TCGraphOutputPort;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cleanup;
    function AddNewBlock(BlockType: TCGraphBlockClass; DeviceName: string): TCGraphBlock; virtual;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function GetUpdatedDescription(Indent: string): string;
    function Load: Boolean;
    function Save: Boolean;
    procedure ConnectPorts(Sender: TObject);
    procedure DestroyBlock(var Block: TCGraphBlock);
    procedure SelectBlock(Sender: TObject);
    property OnChildrenCreate: TNotifyEvent read FOnChildrenCreate write FOnChildrenCreate;
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

procedure TCGraphDesign.Cleanup;
var
  CodeType: TCodeType;
begin
  while Assigned(Components[0]) do begin
    Components[0].Free;
  end;
  for CodeType := Low(CodeType) to High(CodeType) do begin
    if Assigned(CodeBuffer[CodeType]) then begin
      FreeAndNil(CodeBuffer[CodeType]);
    end;
  end;
    if Assigned(SimCodeBuffer)  then begin
      FreeAndNil(SimCodeBuffer);
    end;
    SelectedBlock := nil;
    SelectedInputPort := nil;
    SelectedOutputPort := nil;
end;

procedure TCGraphDesign.ConnectPorts(Sender: TObject);
var
  Connector: TCGraphConnector;
begin
  Connector := TCGraphConnector.Create(Self);
  with Connector do begin
    Parent := Self;
    Connect(SelectedOutputPort, SelectedInputPort);
    OnMouseEnter := @HandleMouseEnter;
    OnMouseLeave := @HandleMouseLeave;
  end;
  if Assigned(FOnChildrenCreate) then begin
    FOnChildrenCreate(Connector);
  end;
end;

function TCGraphDesign.AddNewBlock(BlockType: TCGraphBlockClass; DeviceName: string):TCGraphBlock;
var
  BlockQuantity: integer = 0;
  R: TRect;
  w, h: Integer;
begin
  Result := BlockType.Create(Self);
  if DeviceName <> '' then begin
    with Result do begin
      Name := DeviceName;
    end;
  end else begin
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
    end;
  end;
  with Result do begin
    Selected := True;
    OnClick := @SelectBlock;
    OnMouseEnter := @HandleMouseEnter;
    OnMouseLeave := @HandleMouseLeave;
  end;
  if Assigned(FOnChildrenCreate) then begin
    FOnChildrenCreate(Result);
  end;
end;

function TCGraphDesign.DeviceIdentifier: string;
begin
  Result := Name;
end;

function TCGraphDesign.DeviceType: string;
const
  Prefix = 'TCGraph';
begin
  Result := ClassName;
  if Pos(Prefix, Result) = 1 then begin
    Delete(Result, 1, Length(Prefix));
  end;
end;

function TCGraphDesign.GetUpdatedDescription(Indent: string): string;
var
  Component: TComponent;
  i: Integer;
begin
  Result := Indent + 'object ' + Name + 'Simulator: TCustomDesign' + LineEnding;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphConnector then with Component as TCGraphConnector do begin
      Result += GetUpdatedDescription(Indent + '  ');
    end else if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      Result += GetUpdatedDescription(Indent + '  ');
    end;
  end;
  Result += Indent + 'end' + LineEnding;
end;

procedure TCGraphDesign.DestroyBlock(var Block: TCGraphBlock);
begin
  FreeAndNil(Block);
end;

procedure TCGraphDesign.HandleMouseEnter(Sender: TObject);
begin
  PointedDevice := Sender as TCGraphDevice;
end;

procedure TCGraphDesign.HandleMouseLeave(Sender: TObject);
begin
  if Sender = PointedDevice then begin
    PointedDevice := nil;
  end;
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

function TCGraphDesign.Load: Boolean;
  function GetDeviceClass(DeviceName, DeviceType: string): TCGraphBlockClass;
  var
    CodeFile: string;
    ACodeBuffer: TCodeBuffer;
    AncestorClassName: string;
  begin
    CodeFile := DesignDir + DeviceName + '.pas';
    //codeFile[ctDescription] := DesignDir + BlockDescription.Name + '.lfm';
    ACodeBuffer := GetCodeBuffer(CodeFile, cttNone, nil);
    CodeToolBoss.FindFormAncestor(ACodeBuffer, DeviceType, AncestorClassName, True);
    WriteLn('DeviceName = ', DeviceName, ', DeviceType = ', DeviceType, ', AncestorClassName = ', AncestorClassName);
    if AncestorClassName = 'TBlock' then begin
      Result := TCGraphBlock;
    end else if AncestorClassName = 'TRandomSource' then begin
      Result := TCGraphSource;
    end else if AncestorClassName = 'TFileDumpProbe' then begin
      Result := TCGraphProbe;
    end;
  end;
var
  DesignDescription: TLFMTree;
  BlockDescription: TLFMObjectNode;
  PortName: string;
  BlockName: string;
  p: Integer;
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
  Component: TComponent;
begin
  Result := true;
  codeFile[ctSource] := DesignDir + Name + '.pas';
  codeFile[ctDescription] := DesignDir + Name + '.lfm';
  for CodeType := Low(CodeType) To High(CodeType) do begin
    if Assigned(CodeBuffer[CodeType]) then
      Result := Result and CodeBuffer[CodeType].Reload
    else begin
        CodeBuffer[CodeType] := GetCodeBuffer(CodeFile[CodeType], cttNone, Self);
      Result := Result and Assigned(CodeBuffer[CodeType]);
    end;
  end;
  if not Result then begin
    Exit(False);
  end;
  with CodeToolBoss do begin
    //WriteLn('TCGraphDesign.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    //WriteLn('TCGraphDesign.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False) then begin
      if not Assigned(DesignDescription) then begin
        Exit(False);
      end else begin
        WriteLn('Errors encountered while loading design');
      end;
    end;
  end;
  //WriteLn('TCGraphDesign.Load : LFM created');
  BlockDescription := FindObjectProperty(nil, DesignDescription);
  while Assigned(BlockDescription) do begin
    WriteLn('BlockDescription.TypeName = ', BlockDescription.TypeName);
    if BlockDescription.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(BlockDescription, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      WriteLn('BlockName = ', BlockName);
      PortName := Copy(PortName, p + 1, length(PortName));
      WriteLn('OutputPortName = ', PortName);
      Component := FindComponent(BlockName);
      WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      Component := Component.FindComponent(PortName);
      WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      SelectedOutputPort := Component as TCGraphOutputPort;
      PortName := GetPropertyValue(BlockDescription, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      WriteLn('InputPortName = ', PortName);
      Component := FindComponent(BlockName);
      WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      Component := Component.FindComponent(PortName);
      WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      SelectedInputPort := Component as TCGraphInputPort;
      ConnectPorts(Self);
    end else begin
      if Assigned(SelectedBlock) then
        SelectedBlock.Selected := False;
      SelectedBlock := AddNewBlock(GetDeviceClass(BlockDescription.Name, BlockDescription.TypeName), BlockDescription.Name);
      SelectedBlock.Load(DesignDescription, BlockDescription);
      //WriteLn('++++++++++++++');
    end;
    BlockDescription := FindObjectProperty(BlockDescription, DesignDescription);
  end;
end;

function TCGraphDesign.Save: Boolean;
var
  Component: TComponent;
  i: Integer;
  CodeFileName: string;
begin
  CodeFileName := DesignDir + Name + '.lfm';
  CodeBuffer[ctDescription] := GetCodeBuffer(CodeFileName, cttNone,Self);
  CodeBuffer[ctDescription].Source := GetUpdatedDescription('');
  Result := CodeBuffer[ctDescription].Save;
  CodeFileName := DesignDir + Name + '.pas';
  CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttDesign, Self);
  UpdateUsedBlocks(Self, CodeBuffer[ctSource]);
  Result := Result and CodeBuffer[ctSource].Save;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      Result := Result and Save;
    end; 
  end;
  CodeFileName := DesignDir + '/Simulate' + Name + '.pas';
  SimCodeBuffer := GetCodeBuffer(CodeFileName, cttSimulator, Self);
  Result := Result and SimCodeBuffer.Save;
end;

{procedure Register;
begin
  RegisterComponents('GraphDesign', [TCGraphDesign]);
end;

initialization
  RegisterClass(TCGraphDesign);}
end.
