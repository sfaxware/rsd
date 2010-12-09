unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, CodeCache, GraphComponents;

type
  TInputPortRef = class(TInputPort, IOutputPort)
  private
    FInternalConnector: TConnector;
    function GetConnector: TConnector; override;
    procedure SetConnector(AConnector: TConnector); override;
  end;
  TOutputPortRef = class(TOutputPort, IInputPort)
  private
    FInternalConnector: TConnector;
    function GetConnector: TConnector; override;
    procedure SetConnector(AConnector: TConnector); override;
  end;
  TDesign = class(TBlock)
  private
    FMagnification: Real;
    FMousePos: TPoint;
    FOriginalBound: TRect;
    procedure HandleMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseWheele(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected
    function AddNewSubBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock; override;
    procedure HandleMouseEnter(Sender: TObject); override;
    procedure HandleMouseLeave(Sender: TObject); override;
    procedure SetControlsVisibility(Visibility: Boolean); override;
  public
    SimCodeBuffer: TCodeBuffer;
    PointedDevice : TDevice;
    SelectedBlock:TBlock;
    SelectedInputPort: IInputPort;
    SelectedOutputPort: IOutputPort;
    class function GetViewed: TDesign; inline;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Cleanup;
    function AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock; virtual;
    function AddNewConnector(ADeviceName, ADeviceType: string): TConnector; virtual;
    function DeviceCodeTemplateType: TCodeTemplateType; override;
    function DeviceDescription(Indent: string): string;
    function DeviceUnitName: string;
    function IsSelected: Boolean; inline;
    function Save: Boolean;
    procedure DeleteConnector(var Connector: TConnector);
    procedure DestroyBlock(var Block: TBlock);
    procedure SelectBlock(Sender: TObject);
    procedure SetViewed(ShowDesign: Boolean);
  end;

implementation

uses
  Controls, Graphics, LFMTrees, CodeToolManager, CodeWriter,
  Magnifier, Configuration, Types;

var
  SelectedDesign: TDesign = nil;

function TInputPortRef.GetConnector: TConnector;
begin
  if Owner is TDesign then with Owner as TDesign do begin
    if isSelected then begin
      Result := FInternalConnector;
    end else begin
      Result := FConnector;
    end;
  end;
end;

procedure TInputPortRef.SetConnector(AConnector: TConnector);
begin
  if Owner is TDesign then with Owner as TDesign do begin
    if isSelected then begin
      FInternalConnector := AConnector;
    end else begin
      FConnector := AConnector;
    end;
  end;
end;

function TOutputPortRef.GetConnector: TConnector;
begin
  if Owner is TDesign then with Owner as TDesign do begin
    if isSelected then begin
      Result := FInternalConnector;
    end else begin
      Result := FConnector;
    end;
  end;
end;

procedure TOutputPortRef.SetConnector(AConnector: TConnector);
begin
  if Owner is TDesign then with Owner as TDesign do begin
    if isSelected then begin
      FInternalConnector := AConnector;
    end else begin
      FConnector := AConnector;
    end;
  end;
end;

class function TDesign.GetViewed: TDesign;
begin
  Result := SelectedDesign;
end;

constructor TDesign.Create(AOwner: TComponent);
begin
  if AOwner is TScrollBox then begin
    {We are creating a top design}
    ResetDeviceQty;
  end;
  inherited Create(AOwner);
  //WriteLn('Created new TDesign class instance');
  FMagnification := 1;
  SetViewed(not Assigned(SelectedDesign));
  WriteLn('Owner = ', Owner.Name, ', Parent = ', Parent.Name, ', Visible = ', Visible);
end;

destructor TDesign.Destroy;
begin
  SetViewed(False);
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
  //WriteLn(Name, '.AddNewSubBlock(', ADeviceName, ', ', ADeviceType, ', ', ADeviceAncestorType, ')');
  Result := AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType);
end;

function TDesign.AddNewBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock;
var
  R: TRect;
  w, h: Integer;
begin
  CreateDevice(Result, ADeviceName, ADeviceType, ADeviceAncestorType, Self);
  if ADeviceName = '' then begin
    with Result do begin
      R := OriginalBounds;
      with R do begin
        w := Right - Left;
        h := Bottom - Top;
        Left := FMousePos.X;
        Top := FMousePos.Y;
        Right := Left + w;
        Bottom := Top + h;
      end;
      OriginalBounds := R;
      Parent := Self.Parent;
    end;
  end;
  with Result do begin
    Visible := IsSelected;
    Selected := IsSelected;
    OnClick := @SelectBlock;
    OnMouseEnter := @Self.HandleMouseEnter;
    OnMouseLeave := @Self.HandleMouseLeave;
  end;
  if Assigned(OnChildrenCreate) then begin
    OnChildrenCreate(Result);
  end;
  InsertDevice(Result, Self);
end;

function TDesign.AddNewConnector(ADeviceName, ADeviceType: string): TConnector;
begin
  Result := inherited AddNewConnector(ADeviceName, ADeviceType, SelectedOutputPort, SelectedInputPort);
  with Result do begin
    Visible := IsSelected;
    OnMouseEnter := @Self.HandleMouseEnter;
    OnMouseLeave := @Self.HandleMouseLeave;
  end;
end;

function TDesign.DeviceCodeTemplateType: TCodeTemplateType;
begin
  Result := cttDesign;
end;

function TDesign.DeviceDescription(Indent: string): string;
var
  Component: TComponent;
  i: Integer;
begin
  Result := Indent + 'object ' + Name + 'Simulator: ' + DeviceType + LineEnding;
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

function TDesign.IsSelected: Boolean;
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
  if Sender = Parent then begin
    FMousePos.x := X;
    FMousePos.y := Y;
    //WriteLn('FMousePos.X = ', FMousePos.X, ', FMousePos.Y = ', FMousePos.Y);
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

procedure TDesign.SetViewed(ShowDesign: Boolean);
var
  R: TRect;
begin
  //WriteLn(Name, '.SetViewed(ShowDesign = ', ShowDesign, ')');
  if ShowDesign then begin
    if Assigned(SelectedDesign) then begin
      SelectedDesign.SetViewed(False);
    end;
    SelectedDesign := Self;
    Visible := False;
    FOriginalBound := OriginalBounds;
    with Parent do begin
      OnMouseMove := @HandleMouseMove;
      OnMouseWheel := @HandleMouseWheele;
      R := BoundsRect;
    end;
    InflateRect(R, -2 * DefaultPortWidth, -2 * DefaultPortHeight);
    OriginalBounds := R;
  end else if IsSelected then begin
    SelectedDesign := nil;
    OriginalBounds := FOriginalBound;
  end;
  SetControlsVisibility(ShowDesign);
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

procedure TDesign.SetControlsVisibility(Visibility: Boolean);
var
  i: Integer;
  j: Integer;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TControl then with Component as TControl do begin
      Visible := Visibility;
      if Component is TDesign then with Component as TDesign do begin
        SetControlsVisibility(IsSelected);
        for j := 0 to ComponentCount - 1 do begin
          if Components[j] is TPort then begin
            TPort(Components[j]).Visible := Visibility;
          end;
        end;
        UpdatePortsBounds(TInputPort);
        UpdatePortsBounds(TOutputPort);
      end else if Component is TBlock then with Component as TBlock do begin
        SetControlsVisibility(Visibility);
      end;
    end;
  end;
end;

initialization
  TInputPort.RegisterDevice('TInputPortRef', 'Designs', []);
  TOutputPort.RegisterDevice('TOutputPortRef', 'Designs', []);
  TDesign.RegisterDevice('TDesign', 'Designs', []);
  TDesign.RegisterDevice('TTopDesign', 'Designs', []);
end.
