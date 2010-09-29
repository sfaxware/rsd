unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CodeCache, GraphComponents;

type
  TDesign = class(TScrollBox, TIGraphDevice)
  private
    FMagnification: Real;
    FOnChildrenCreate: TNotifyEvent;
    procedure HandleMouseEnter(Sender: TObject);
    procedure HandleMouseLeave(Sender: TObject);
    procedure MouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    SimCodeBuffer: TCodeBuffer;
    PointedDevice : TDevice;
    SelectedBlock:TBlock;
    SelectedInputPort: TInputPort;
    SelectedOutputPort: TOutputPort;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cleanup;
    function AddNewBlock(ADeviceName, ADeviceType: string): TBlock; virtual;
    function AddNewConnector(ADeviceName, ADeviceType: string): TConnector; virtual;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceAncestorType: string;
    function DeviceDescription(Indent: string): string;
    function Load: Boolean;
    function Save: Boolean;
    procedure DeleteConnector(var Connector: TConnector);
    procedure DestroyBlock(var Block: TBlock);
    procedure SelectBlock(Sender: TObject);
    property OnChildrenCreate: TNotifyEvent read FOnChildrenCreate write FOnChildrenCreate;
  end;
  TScrollBox = class(TDesign);

implementation
uses
  Controls, Graphics, LFMTrees, CodeToolManager, CodeWriter,
  Magnifier, Configuration;
                        
constructor TDesign.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //WriteLn('Created new TDesign class instance');
  OnMouseWheel := @MouseWheele;
  FMagnification := 1;
end;

destructor TDesign.Destroy;
begin
  inherited Destroy;
end;

procedure TDesign.Cleanup;
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

function TDesign.AddNewBlock(ADeviceName, ADeviceType: string):TBlock;
var
  R: TRect;
  w, h: Integer;
begin
  Result := CreateBlock(ADeviceName, ADeviceType, Self);
  if ADeviceName = '' then begin
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
  if not Assigned(CodeBuffer[ctSource]) then begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttDesign, Self);
  end;
  CodeBuffer[ctSource].LockAutoDiskRevert;
  CodeToolBoss.AddUnitToMainUsesSection(CodeBuffer[ctSource], Result.DeviceIdentifier, '');
  CodeToolBoss.AddPublishedVariable(CodeBuffer[ctSource], DeviceType, Result.DeviceIdentifier, Result.DeviceType);
  CodeBuffer[ctSource].UnlockAutoDiskRevert;
end;

function TDesign.AddNewConnector(ADeviceName, ADeviceType: string): TConnector;
begin
  Result := CreateConnector(ADeviceName, ADeviceType, Self);
  with Result do begin
    Parent := Self;
    Connect(SelectedOutputPort, SelectedInputPort);
    OnMouseEnter := @HandleMouseEnter;
    OnMouseLeave := @HandleMouseLeave;
  end;
  if Assigned(FOnChildrenCreate) then begin
    FOnChildrenCreate(Result);
  end;
  if not Assigned(CodeBuffer[ctSource]) then begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttDesign, Self);
  end;
end;

function TDesign.DeviceIdentifier: string;
begin
  Result := Name;
end;

function TDesign.DeviceType: string;
begin
  Result := 'TCustomDesign';
end;

function TDesign.DeviceAncestorType: string;
begin
  Result := 'TDesign';
end;

function TDesign.DeviceDescription(Indent: string): string;
var
  Component: TComponent;
  i: Integer;
begin
  Result := Indent + 'object ' + Name + 'Simulator: TCustomDesign' + LineEnding;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TConnector then with Component as TConnector do begin
      Result += DeviceDescription(Indent + '  ');
    end else if Component is TBlock then with Component as TBlock do begin
      Result += DeviceDescription(Indent + '  ');
    end;
  end;
  Result += Indent + 'end' + LineEnding;
end;

procedure TDesign.DeleteConnector(var Connector: TConnector);
begin
  FreeAndNil(Connector);
end;

procedure TDesign.DestroyBlock(var Block: TBlock);
begin
  FreeAndNil(Block);
end;

procedure TDesign.HandleMouseEnter(Sender: TObject);
begin
  PointedDevice := Sender as TDevice;
end;

procedure TDesign.HandleMouseLeave(Sender: TObject);
begin
  if Sender = PointedDevice then begin
    PointedDevice.MouseLeaved(Sender);
    PointedDevice := nil;
  end;
end;

procedure TDesign.MouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var
  i: Integer;
  Control: TControl;
  m: Real;
  dx, dy, dm: Integer;
begin
  Handled := True;
  if ssCtrl in Shift then begin
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
      if Control is TBlock then with Control as TMagnifier do begin
        Magnify(m);
      end;
    end;
    FMagnification := m;
    ScrollBy(dx, dy);
  end;
end;

procedure TDesign.SelectBlock(Sender: TObject);
begin
  if Sender is TBlock then begin
     if Assigned(SelectedBlock) then
       SelectedBlock.Selected := False;
    SelectedBlock := Sender as TBlock;
    SelectedBlock.Selected := True;
  end;
end;

function TDesign.Load: Boolean;
var
  DesignDescription: TLFMTree;
  BlockDescription: TLFMObjectNode;
  PortName: string;
  BlockName: string;
  p: Integer;
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
  Device: TDevice;
begin
  Result := true;
  codeFile[ctSource] := SourceFileName(Name);
  codeFile[ctDescription] := ResourceFileName(Name);
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
    //WriteLn('TDesign.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    //WriteLn('TDesign.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False) then begin
      if not Assigned(DesignDescription) then begin
        Exit(False);
      end else begin
        WriteLn('Errors encountered while loading design');
      end;
    end;
  end;
  //WriteLn('TDesign.Load : LFM created');
  BlockDescription := FindObjectProperty(nil, DesignDescription);
  while Assigned(BlockDescription) do begin
    //WriteLn('BlockDescription.TypeName = ', BlockDescription.TypeName);
    if BlockDescription.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(BlockDescription, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      //WriteLn('BlockName = ', BlockName);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('OutputPortName = ', PortName);
      Device := FindComponent(BlockName) as TDevice;
      //WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      Device := Device.FindComponent(PortName) as TDevice;
      //WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      SelectedOutputPort := Device as TOutputPort;
      PortName := GetPropertyValue(BlockDescription, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('InputPortName = ', PortName);
      Device := FindComponent(BlockName) as TDevice;
      //WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      Device := Device.FindComponent(PortName) as TDevice;
      //WriteLn('Component.Name = ', Component.Name, ', Component.Type = ', Component.ClassName);
      SelectedInputPort := Device as TInputPort;
      Device := AddNewConnector(BlockDescription.Name, BlockDescription.TypeName);
      Device.Load(DesignDescription, BlockDescription);
    end else begin
      SelectBlock(AddNewBlock(BlockDescription.Name, BlockDescription.TypeName));
      SelectedBlock.Load(DesignDescription, BlockDescription);
      //WriteLn('++++++++++++++');
    end;
    BlockDescription := FindObjectProperty(BlockDescription, DesignDescription);
  end;
end;

function TDesign.Save: Boolean;
var
  Component: TComponent;
  i: Integer;
begin
  CodeBuffer[ctDescription] := GetCodeBuffer(cttDescription, Self);
  CodeBuffer[ctDescription].Source := DeviceDescription('');
  Result := CodeBuffer[ctDescription].Save;
  CodeBuffer[ctSource] := GetCodeBuffer(cttDesign, Self);
  Result := Result and CodeBuffer[ctSource].Save;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TBlock then with Component as TBlock do begin
      Result := Result and Save;
    end; 
  end;
  SimCodeBuffer := GetCodeBuffer(cttSimulator, Self);
  Result := Result and SimCodeBuffer.Save;
end;

{procedure Register;
begin
  RegisterComponents('GraphDesign', [TDesign]);
end;

initialization
  RegisterClass(TDesign);}
end.
