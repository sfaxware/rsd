unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CodeCache, GraphComponents;

type
  TDesign = class(TBlock)
  private
    FMagnification: Real;
    FMousePos: TPoint;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    function AddNewSubBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock; override;
    procedure HandleMouseEnter(Sender: TObject); override;
    procedure HandleMouseLeave(Sender: TObject); override;
  public
    SimCodeBuffer: TCodeBuffer;
    PointedDevice : TDevice;
    SelectedBlock:TBlock;
    SelectedInputPort: TInputPort;
    SelectedOutputPort: TOutputPort;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cleanup;
    function AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock; virtual;
    function AddNewConnector(ADeviceName, ADeviceType: string): TConnector; virtual;
    function DeviceAncestorUnitName: string;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceAncestorType: string;
    function DeviceDescription(Indent: string): string;
    function DeviceUnitName: string;
    function IsSelected: Boolean; inline;
    function Save: Boolean;
    procedure DeleteConnector(var Connector: TConnector);
    procedure DestroyBlock(var Block: TBlock);
    procedure SelectBlock(Sender: TObject);
    procedure SetSelected(ShowDesign: Boolean);
  end;

implementation
uses
  Controls, Graphics, LFMTrees, CodeToolManager, CodeWriter,
  Magnifier, Configuration;

var
  SelectedDesign: TDesign = nil;
                        
constructor TDesign.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint := nil;
  Name := 'Design';
  //WriteLn('Created new TDesign class instance');
  Parent := AOwner as TScrollBox;
  with Parent do begin
    OnMouseWheel := @HandleMouseWheele;
    OnMouseMove := @HandleMouseMove;
  end;
  FMagnification := 1;
  SetSelected(not Assigned(SelectedDesign));
end;

destructor TDesign.Destroy;
begin
  SetSelected(False);
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

function TDesign.AddNewSubBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock;
begin
  Result := AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType);
end;

function TDesign.AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock;
var
  R: TRect;
  w, h: Integer;
begin
  Result := CreateBlock(ADeviceName, ADeviceType, ADeviceAncestorType, Self);
  if ADeviceName = '' then begin
    R := Result.OriginalBounds;
    with R do begin
      w := Right - Left;
      h := Bottom - Top;
      Left := FMousePos.X;
      Top := FMousePos.Y;
      Right := Left + w;
      Bottom := Top + h;
    end;
    with Result do begin
      OriginalBounds := R;
      Parent := Self.Parent;
    end;
  end;
  with Result do begin
    Selected := True;
    OnClick := @SelectBlock;
    OnMouseEnter := @Self.HandleMouseEnter;
    OnMouseLeave := @Self.HandleMouseLeave;
  end;
  if Assigned(OnChildrenCreate) then begin
    OnChildrenCreate(Result);
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
  Result := inherited AddNewConnector(ADeviceName, ADeviceType, SelectedOutputPort, SelectedInputPort);
  with Result do begin
    OnMouseEnter := @Self.HandleMouseEnter;
    OnMouseLeave := @Self.HandleMouseLeave;
  end;
end;

function TDesign.DeviceAncestorUnitName: string;
begin
  Result := 'Designs';
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

function TDesign.DeviceUnitName: string;
begin
  Result := 'Designs';
end;

function TDesign.IsSelected: Boolean; inline;
begin
  Result := SelectedDesign = Self;
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

procedure TDesign.HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if Sender = Self then begin
    FMousePos.x := X;
    FMousePos.y := Y;
  end;
end;

procedure TDesign.HandleMouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
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
    with Parent as TScrollBox do begin
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

procedure TDesign.SetSelected(ShowDesign: Boolean);
var
  i: Integer;
begin
  if Assigned(SelectedDesign) then begin
    if ShowDesign then begin
      SelectedDesign.SetSelected(False);
      SelectedDesign := Self;
    end else if IsSelected then begin
      SelectedDesign := nil;
    end;
  end;
  for i := 0 to ComponentCount do with Components[i] do begin
    Visible := ShowDesign;
  end;
end;

function TDesign.Save: Boolean;
var
  Component: TComponent;
  i: Integer;
  NewFileName: string;
begin
  CodeBuffer[ctDescription] := GetCodeBuffer(cttDescription, Self);
  CodeBuffer[ctDescription].Source := DeviceDescription('');
  Result := CodeBuffer[ctDescription].Save;
  if Assigned(CodeBuffer[ctSource]) then begin
    NewFileName := SourceFileName(DeviceIdentifier);
    if CodeBuffer[ctSource].Filename <> NewFileName then begin
      CodeToolBoss.SaveBufferAs(CodeBuffer[ctSource], NewFileName, CodeBuffer[ctSource]);
    end;
  end else begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttDesign, Self);
  end;
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
