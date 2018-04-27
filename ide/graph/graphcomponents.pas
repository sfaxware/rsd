unit GraphComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Types, CodeCache, LFMTrees, Routing, Magnifier, ComCtrls;

const
  DefaultBlockWidth = 100;
  DefaultBlockHeight = 100;
  DefaultPortWidth = 10;
  DefaultPortHeight = 10;
  MinPortSpacing = 10;

type
  TCodeType = (ctSource, ctDescription);
  TCodeTemplateType = (cttNone, cttDescription, cttSimulator, cttDesign, cttBlock, cttSource, cttProbe);
  IDevice = interface
    function DeviceAncestorType: string;
    function DeviceAncestorUnitName: string;
    function DeviceDescription(Indent: string): string;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceUnitName: string;
    function GetOriginalBounds: TRect;
    procedure SetOriginalBounds(B: TRect);
    property OriginalBounds: TRect read GetOriginalBounds write SetOriginalBounds;
  end;
  TDevicePropertyType = TLFMValueType;
  TDeviceProperty = string;
  TDevicePropertyInfo = record
    PropName: string;
    PropType: TDevicePropertyType;
    DefaultValue: TDeviceProperty;
  end;
  TDevice = class(TMagnifier, IDevice)
  private
    FDeviceType: string;
    FDeviceId: Integer;
    FProperties: array of TDeviceProperty;
  protected
    function GetPropertyIndex(const PropName: string): Integer;
    function GetProperty(PropIndex: Integer): TDeviceProperty; virtual;
    function GetProperty(const PropName: string): TDeviceProperty;
    function GetPropName(PropIndex: Integer): string;
    function GetPropQty: Integer;
    function GetPropType(PropIndex: Integer): string;
    function SetProperty(PropIndex: Integer; PropVal: TDeviceProperty): Boolean; virtual;
    function SetProperty(const PropName: string; PropVal: TDeviceProperty): Boolean;
    procedure SetAncestorType(const AncestorType: string);
    procedure SetPropVal(PropIndex: Integer; PropVal: TDeviceProperty);
    procedure DoPaint(Sender: TObject); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; DeviceName, DeviceType, DeviceAncestorType: string);
    class procedure RegisterDevice(DeviceType: string; DeviceUnitName: string; DeviceProperties: array of TDevicePropertyInfo);
    function DeviceAncestorUnitName: string;
    function DeviceCodeTemplateType: TCodeTemplateType; virtual; abstract;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceAncestorType: string;
    function DeviceDescription(Indent: string): string;
    function DevicePropertiesDescription(Indent: string): string; virtual;
    function DeviceUnitName: string; virtual;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; virtual;
    procedure MouseLeaved(Sender: TObject);
    property PropQty: Integer read GetPropQty;
    property PropName[PropIndex: Integer]: string read GetPropName;
    property PropType[PropIndex: Integer]: string read GetPropType;
    property PropVal[PropIndex: Integer]: TDeviceProperty read GetProperty write SetPropVal;
  end;
  TDeviceClass = class of TDevice;
  TConnector = class;
  IPort = interface(IDevice)
    function GetConnector: TConnector;
    function OwnerDevice: TDevice;
    function Unplug(AConnector: TConnector): Boolean;
    procedure SetConnector(AConnector: TConnector);
    property Connector: TConnector read GetConnector write SetConnector;
  end;
  IInputPort = interface(IPort)
  end;
  IOutputPort = interface(IPort)
  end;
  TPort = class(TDevice, IPort)
  protected
    FConnector: TConnector;
    function GetConnector: TConnector; virtual;
    function Unplug(AConnector: TConnector): Boolean; virtual;
    procedure HandleMouseEnterLeaveEvents(Sender: TObject); virtual;
    procedure DoPaint(Sender: TObject); override;
    procedure SetConnector(AConnector: TConnector); virtual;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); virtual; abstract;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
    function OwnerDevice: TDevice;
    property Connector: TConnector read GetConnector write SetConnector;
  end;
  TInputPort = class(TPort, IInputPort)
  protected
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TOutputPort = class(TPort, IOutputPort)
  protected
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TPortType = class of TPort;
  TConnector = class(TDevice)
  private
    FInputPort: IInputPort;
    FOutputPort: IOutputPort;
    FPoints: TRoute;
  protected
    function GetPortIdentifier(Port: IPort): string;
    procedure DoPaint(Sender: TObject); override;
    procedure SetInputPort(Value: IInputPort);
    procedure SetOutputPort(Value: IOutputPort);
    procedure UpdatePoints; virtual;
  public
    destructor Destroy; override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
    procedure Connect(AOutputPort: IOutputPort; AInputPort: IInputPort);
  published
    property InputPort: IInputPort read FInputPort write SetInputPort;
    property OutputPort: IOutputPort read FoutputPort write SetOutputPort;
  end;
  TBlock = class(TDevice)
  private
    _MousePos: TPoint;
    FInputComponentCount: Integer;
    FOutputComponentCount: Integer;
    FOnChildrenCreate: TNotifyEvent;
    procedure StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  protected
    FSelected: Boolean;
    function AddNewSubBlock(ADeviceName, ADeviceType, ADeviceAncestorType: string): TBlock; virtual; abstract;
    function BlockBoundsDescription(Indent: string): string; virtual;
    procedure HandleMouseEnter(Sender: TObject); virtual; abstract;
    procedure HandleMouseLeave(Sender: TObject); virtual; abstract;
    procedure SetControlsVisibility(Visibility: Boolean); virtual;
    procedure SetSelected(AValue: Boolean);
    procedure DoPaint(Sender: TObject); override;
    procedure UpdatePortsBounds(PortType: TPortType);
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    TreeNode: TTreeNode;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function AddNewConnector(ADeviceName, ADeviceType: string; AOutputPort: IOutputPort; AInputPort: IInputPort): TConnector;
    function AddNewPort(ADeviceName, ADeviceType: string): TPort; virtual;
    function DeviceCodeTemplateType: TCodeTemplateType; override;
    function DevicePropertiesDescription(Indent: string): string; override;
    function DeviceUnitName: string; override;
    function Load: boolean;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
    function Save: boolean;
    procedure Magnify(m: Real); override;
    property OnChildrenCreate: TNotifyEvent read FOnChildrenCreate write FOnChildrenCreate;
  published
    property Selected: Boolean read FSelected write SetSelected;
    property InputComponentCount: Integer read FInputComponentCount;
    property OutputComponentCount: Integer read FOutputComponentCount;
  end;
  TBlockClass = class of TBlock;
  TSource = class(TBlock)
  protected
    procedure DoPaint(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(ADeviceName, ADeviceType: string): TPort; override;
    function DevicePropertiesDescription(Indent: string): string; override;
  end;
  TProbe = class(TBlock)
  protected
    procedure DoPaint(Sender: TObject); override;
  public
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(ADeviceName, ADeviceType: string): TPort; override;
    function DevicePropertiesDescription(Indent: string): string; override;
  end;

function CreateDevice(out Device: TDevice; DeviceName, DeviceType, DeviceAncestorType: string; AOwner: TComponent): Boolean;
function CreateInputPort(DeviceName, DeviceType: string; AOwner: TComponent): TInputPort;
function CreateOutputPort(DeviceName, DeviceType: string; AOwner: TComponent): TOutputPort;
function CreateConnector(DeviceName, DeviceType: string; AOwner: TComponent): TConnector;
function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): TDeviceProperty;
function FindObjectProperty(ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
function FindObjectProperty(PropertyPath: string; ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
function DeviceProperty(const PropName: string; PropType: TDevicePropertyType; DefaultValue: TDeviceProperty): TDevicePropertyInfo;

implementation
uses
  DesignGraph, CodeToolManager, CodeWriter, Configuration;

type
  TDeviceInfo = record
    TypeName: string;
    TypeClass: TDeviceClass;
    UnitName: string;
    Properties: array of TDevicePropertyInfo;
  end;

const
  DevicePropertyType: array[TDevicePropertyType] of string = ('', 'Integer', 'Real', 'String', 'Symbol', 'Set', 'List', 'Collection', 'Binary');

var
  Devices: array of TDeviceInfo;
  PointedDevice: TDevice;

function DeviceProperty(const PropName: string; PropType: TDevicePropertyType; DefaultValue: TDeviceProperty): TDevicePropertyInfo;
begin
  Result.PropName := PropName;
  Result.PropType := PropType;
  Result.DefaultValue := DefaultValue;
end;

function GetDeviceId(DeviceType: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  if Pos('T', DeviceType) = 1 then begin
    for i := Low(Devices) to High(Devices) do with Devices[i] do begin
      if DeviceType = TypeName then begin
        Exit(i);
      end;
    end;
  end;
end;

function GetDeviceClass(Id: Integer): TDeviceClass;
begin
  if(Id >= Low(Devices)) and (Id <= High(Devices)) then begin
    Result := Devices[Id].TypeClass;
  end else begin
    Result := nil;
  end;
end;

function GetDeviceClass(DeviceType: string): TDeviceClass;
var
  i: Integer;
begin
  i := GetDeviceId(DeviceType);
  Result := GetDeviceClass(i);
end;

function CreateDevice(out Device: TDevice; DeviceName, DeviceType, DeviceAncestorType: string; AOwner: TComponent): Boolean;
var
  DeviceId: Integer;
  DeviceClass: TDeviceClass;
  HintMessage: string;
begin
  //WriteLn('DeviceName = ', DeviceName, ', DeviceType = ', DeviceType, ', DeviceAncestorType = ', DeviceAncestorType);
  IncrementDeviceQty;
  GuessNewDeviceNameAndType(DeviceName, DeviceType, DeviceAncestorType);
  //WriteLn('DeviceName = ', DeviceName, ', DeviceType = ', DeviceType, ', DeviceAncestorType = ', DeviceAncestorType);
  DeviceId := GetDeviceId(DeviceType);
  if DeviceId < 0 then begin
    DeviceId := GetDeviceId(DeviceAncestorType);
  end;
  DeviceClass := GetDeviceClass(DeviceId);
  if Assigned(DeviceClass) then begin
    Device := DeviceClass.Create(AOwner, DeviceName, DeviceType, DeviceAncestorType);
  end else begin
    Device := nil;
  end;
  Result := Assigned(Device);
  if Result then with Device do begin
    HintMessage := DeviceName + ' is ' + DeviceType;
    if DeviceName <> DeviceAncestorType then begin
      HintMessage += LineEnding + ' which is ' + DeviceAncestorType;
    end;
    Hint := HintMessage;
    ShowHint := Device is TBlock;
  end;
end;

function CreateInputPort(DeviceName, DeviceType: string; AOwner: TComponent): TInputPort;
begin
  CreateDevice(TDevice(Result), DeviceName, DeviceType, 'TInputPort', AOwner);
end;

function CreateOutputPort(DeviceName, DeviceType: string; AOwner: TComponent): TOutputPort;
begin
  CreateDevice(TDevice(Result), DeviceName, DeviceType, 'TOutputPort', AOwner);
end;

function CreateConnector(DeviceName, DeviceType: string; AOwner: TComponent): TConnector;
begin
  if DeviceType = '' then begin
    DeviceType := 'TConnector';
  end;
  CreateDevice(TDevice(Result), DeviceName, DeviceType, 'TConnector', AOwner);
end;

function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): TDeviceProperty;
var
  PropertyNode: TLFMPropertyNode;
  ValueNode: TLFMTreeNode;
  c: char;
  p: integer;
  Digits: set of Char;
begin
  while Assigned(ContextNode) do with ContextNode do begin
    PropertyName := Name + '.' + PropertyName;
    ContextNode := Parent as TLFMObjectNode;
  end;
  Result := '';
  //WriteLn('GetPropertyValue: PropertyName = ', PropertyName);
  PropertyNode := Self.FindProperty(PropertyName, nil);
  if not Assigned(PropertyNode) then begin
    Exit('');
  end;
  ValueNode := PropertyNode.Next;
  //WriteLn('GetPropertyValue : PropertyName = ', PropertyName, ', PropertyType = ', Integer(ValueNode.TheType));
  if ValueNode.TheType = lfmnValue then with ValueNode as TLFMValueNode do begin
    case ValueType of
      lfmvString: Result := ReadString;
      lfmvInteger: begin
        p := StartPos;
        c := Tree.LFMBuffer.Source[p];
        Digits := [];
        case c of
          '0'..'9': Digits := ['0'..'9'];
          '$': begin
            Digits := ['0'..'9', 'A'..'F', 'a'..'f'];
            Result += c;
            p += 1;
            c:= Tree.LFMBuffer.Source[p];
          end;
        else
          WriteLn('Invalid integer start digit "', c, '"');
        end;
        while c in Digits do begin
          Result += c;
          p += 1;
          c := Tree.LFMBuffer.Source[p];
        end;
      end;
      lfmvSymbol: begin
        p := StartPos;
        c := Tree.LFMBuffer.Source[p];
        while c in ['0'..'9', 'A'..'Z', 'a'..'z', '_', '.'] do begin
          Result += c;
          p += 1;
          c := Tree.LFMBuffer.Source[p]
        end;
      end;
    else
      WriteLn('GetPropertyValue : Unsupported node type "', Integer(ValueType), '"');
    end;
  end;
  //WriteLn('GetPropertyValue(', PropertyName, ') = "', Result, '"');
end;

function FindObjectProperty(ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
var
  Node: TLFMTreeNode;
begin
  if Assigned(ContextNode) then
    Node := ContextNode.NextSibling
  else
    Node := Self.Root.FirstChild;
  while Node <> nil do begin
    if Node is TLFMObjectNode then begin
      Result := Node as TLFMObjectNode;
      //WriteLn('Name = ', Result.Name, ', Type = ', Result.TypeName);
      Exit;
    end;
    Node := Node.NextSibling;
  end;
  Result:=nil;
end;

function FindObjectProperty(PropertyPath: string; ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
begin
  repeat
    Result := FindObjectProperty(ContextNode, Self);
    ContextNode := Result;
  until (Result = nil) or (SysUtils.CompareText(Result.Name, PropertyPath) = 0);
end;

function Translate(Points: TRoute; dx, dy: Integer): TRoute;
var
  l: Integer;
begin
  l := Length(Points);
  SetLength(Result, l);
  while l > 0 do begin
    l -= 1;
    Result[l].x := Points[l].x + dx;
    Result[l].y := Points[l].y + dy;
  end;
end;

constructor TDevice.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnPaint := @DoPaint;
  if FDeviceType = '' then begin
    FDeviceType := 'T' + Name;
  end;
  if DeviceAncestorType = '' then begin
    SetAncestorType(ClassName);
  end;
end;

constructor TDevice.Create(AOwner: TComponent; DeviceName, DeviceType, DeviceAncestorType: string);
begin
  if DeviceName <> '' then begin
    Name := DeviceName;
  end;
  if DeviceType <> '' then begin
    FDeviceType := DeviceType;
  end;
  if DeviceAncestorType <> '' then begin
    SetAncestorType(DeviceAncestorType);
  end;
  Create(AOwner);
end;

class procedure TDevice.RegisterDevice(DeviceType: string; DeviceUnitName: string; DeviceProperties: array of TDevicePropertyInfo);
var
  l: Integer;
begin
  l := Length(Devices);
  SetLength(Devices, l + 1);
  with Devices[l] do begin
    TypeName := DeviceType;
    TypeClass := TDeviceClass(ClassType);
    UnitName := DeviceUnitName;
    l := Length(DeviceProperties);
    SetLength(Properties, l);
    while l > 0 do begin
      l -= 1;
      Properties[l] := DeviceProperties[l];
    end;
  end;
end;

function TDevice.GetPropertyIndex(const PropName: string): Integer;
var
  PropIndex: Integer;
begin
  with Devices[FDeviceId] do begin
    for PropIndex := Low(Properties) to High(Properties) do begin
      if PropName = Properties[PropIndex].PropName then begin
        Exit(PropIndex);
      end;
    end;
  end;
  Result := -1;
end;

function TDevice.GetProperty(PropIndex: Integer): TDeviceProperty;
begin
  if(PropIndex >= Low(FProperties)) and (PropIndex <= High(FProperties))then begin
    Result := FProperties[PropIndex];
  end else begin
    Result := '';
  end;
end;

function TDevice.GetProperty(const PropName: string): TDeviceProperty;
var
  PropIndex: Integer;
begin
  PropIndex := GetPropertyIndex(PropName);
  Result := GetProperty(PropIndex);
end;

function TDevice.GetPropName(PropIndex: Integer): string;
begin
  with Devices[FDeviceId] do begin
    if(PropIndex >= Low(Properties)) and (PropIndex <= High(Properties)) then begin
      Result := Properties[PropIndex].PropName;
    end else begin
      Result := '';
    end;
  end;
end;

function TDevice.GetPropQty: Integer;
begin
  Result := Length(FProperties);
end;

function TDevice.GetPropType(PropIndex: Integer): string;
begin
  with Devices[FDeviceId] do begin
    if(PropIndex >= Low(Properties)) and (PropIndex <= High(Properties)) then begin
      Result := DevicePropertyType[Properties[PropIndex].PropType];
    end else begin
      Result := '';
    end;
  end;
end;

function TDevice.SetProperty(PropIndex: Integer; PropVal: TDeviceProperty): Boolean;
begin
  Result := (PropIndex >= Low(FProperties)) and (PropIndex <= High(FProperties));
  if Result then begin
    FProperties[PropIndex] := PropVal;
  end;
end;

function TDevice.SetProperty(const PropName: string; PropVal: TDeviceProperty): Boolean;
var
  PropIndex: Integer;
begin
  PropIndex := GetPropertyIndex(PropName);
  Result := SetProperty(PropIndex, PropVal);
end;

procedure TDevice.SetAncestorType(const AncestorType: string);
var
  PropIndex: Integer;
begin
  FDeviceId := GetDeviceId(AncestorType);
  with Devices[FDeviceId] do begin
    SetLength(FProperties, Length(Properties));
    for PropIndex := Low(Properties) to High(Properties) do begin
      SetProperty(PropIndex, Properties[PropIndex].DefaultValue);
    end;
  end;
end;

procedure TDevice.SetPropVal(PropIndex: Integer; PropVal: TDeviceProperty);
begin
  SetProperty(PropIndex, PropVal);
end;

function TDevice.DeviceAncestorUnitName: string;
begin
  Result := Devices[FDeviceId].UnitName;
end;

function TDevice.DeviceIdentifier: string;
begin
  Result := Name;
end;

function TDevice.DeviceType: string;
begin
  Result := FDeviceType;
end;

function TDevice.DeviceAncestorType: string;
begin
  Result := Devices[FDeviceId].TypeName;
end;

function TDevice.DeviceDescription(Indent: string): string;
begin
  Result := Indent + 'object ' + Name + ': ' +  FDeviceType + LineEnding;
  Result += DevicePropertiesDescription(Indent);
  Result += Indent + 'end' + LineEnding;
end;

function TDevice.DevicePropertiesDescription(Indent: string): string;
var
  i: Integer;
  PropValue: TDeviceProperty;
begin
  with Devices[FDeviceId] do begin
    for i := Low(Properties) to High(Properties) do begin;
      case Properties[i].PropType of
        lfmvString: PropValue := '''' + GetProperty(i) + '''';
      else
        PropValue := GetProperty(i);
      end;
      Result += Indent + '  ' + Properties[i].PropName + ' = ' + PropValue + LineEnding;
    end;
  end;
end;

function TDevice.DeviceUnitName: string;
begin
  Result := Devices[FDeviceId].UnitName;
end;

function TDevice.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  i: Integer;
  PropValue: TDeviceProperty;
begin
  Result := True;
  with Devices[FDeviceId] do begin
    for i := Low(Properties) to High(Properties) do with Properties[i] do begin;
      PropValue := GetPropertyValue(ContextNode, PropName, DesignDescription);
      Result := Result and SetProperty(i, PropValue);
    end;
  end;
end;

procedure TDevice.MouseLeaved(Sender: TObject);
begin
  PointedDevice := nil;
end;

constructor TPort.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UpdateBounds(-1, -1);
  OnMouseEnter := @HandleMouseEnterLeaveEvents;
  OnMouseLeave := @HandleMouseEnterLeaveEvents;
  Parent := TDevice(AOwner).Parent;
end;

procedure TPort.HandleMouseEnterLeaveEvents(Sender: TObject);
begin
  //WriteLn('MouseEntered = ', MouseEntered);
  Repaint;
end;

procedure TPort.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  Triangle: array[0..2] of TPoint;
begin
  PaintRect := ClientRect;
  Color := clBlack;
  with Canvas do begin
    with Brush do begin
      Style := bsSolid;
      Color := clWhite;
      Rectangle(PaintRect);
      If not Enabled then
        Color := clBtnShadow
      else if MouseEntered then
        Color := clYellow
      else
        Color:= clBlack;
    end;
  end;
  with PaintRect do begin
    Triangle[0] := Point(Left, Top + Height div 2);
    Triangle[1] := Point(Left + Width, Top);
    Triangle[2] := Point(Left + Width, Top + Height - 1);
  end;
  with Canvas do begin
    Polygon(Triangle);
  end;
end;

function TPort.GetConnector: TConnector;
begin
  Result := FConnector;
end;

function TPort.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  Path: string;
begin
  Path := '';
  while Assigned(ContextNode) do with ContextNode do begin
    Path := Name + '.' + Path;
    ContextNode := Parent as TLFMObjectNode;
  end;
  //WriteLn('TPort : Path = ', Path);
//  Left := GetPropertyValue(DesignDescription, Path + 'Left'));
//  Top := GetPropertyValue(DesignDescription, Path + 'Top'));
  Color := clBlack;
//  Caption := GetPropertyValue(DesignDescription, Path + 'Caption');
  Result := True;
end;

function TPort.OwnerDevice: TDevice;
begin
  Result := Owner as TDevice;
end;

function TPort.Unplug(AConnector: TConnector): Boolean;
begin
  Result := FConnector = AConnector;
  if Result then begin
    FConnector := nil;
  end;
end;

procedure TPort.SetConnector(AConnector: TConnector);
begin
  FConnector := AConnector;
end;

procedure TPort.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TBlock then with AComponent as TBlock do begin
    ValidateInsert(Self);
  end;
end;

procedure TInputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  R: TRect;
begin
  with Owner as TBlock do begin
     R := OriginalBounds;
   if Interval <= 0 then begin
      Interval := (R.Bottom - R.Top) div FInputComponentCount;
    end;
    if idx < 0 then begin
      idx := FInputComponentCount - 1;
    end;
  end;
  //WriteLn('idx = ', idx, ' PortTop = ', PortTop, ' PortLeft = ', PortLeft);
  with R do begin
    Left := Right;
    Top := Top + Idx * Interval + Interval div 2 - DefaultPortHeight div 2;
    Right := Left + DefaultPortWidth;
    Bottom := Top + DefaultPortHeight;
    //WriteLn('loaded bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  end;
  OriginalBounds := R;
  if Assigned(Connector) then with Connector do begin
    UpdatePoints;
  end;
end;

procedure TOutputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  R: TRect;
begin
  with Owner as TBlock do begin
    R := OriginalBounds;
    if Interval <= 0 then begin
      Interval := (R.Bottom - R.Top) div FOutputComponentCount;
    end;
    if Idx < 0 then begin
      Idx := FOutputComponentCount - 1;
    end;
  end;
  //WriteLn('idx = ', idx, ' PortTop = ', PortTop, ' PortLeft = ', PortLeft);
  with R do begin
    Left := Left - DefaultPortWidth;
    Top := Top + Idx * Interval + Interval div 2 - DefaultPortHeight div 2;
    Right := Left + DefaultPortWidth;
    Bottom := Top + DefaultPortHeight;
    //WriteLn('loaded bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  end;
  OriginalBounds := R;
  if Assigned(Connector) then with Connector do begin
    UpdatePoints;
  end;
end;

destructor TConnector.Destroy;
begin
  if Assigned(FOutputPort) then begin
    FOutputPort.Unplug(Self);
    FOutputPort := nil;
  end;
  if Assigned(FInputPort) then begin
    FInputPort.Unplug(Self);
    FInputPort := nil;
  end;
  inherited Destroy;
end;

function TConnector.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
begin
  Result := inherited;
end;

procedure TConnector.Connect(AOutputPort: IOutputPort; AInputPort: IInputPort);
begin
  OutputPort := AOutputPort;
  InputPort := AInputPort;
end;

function TConnector.GetPortIdentifier(Port: IPort): string;
var
  PortOwnerDevice: TDevice;
begin
  PortOwnerDevice := Port.OwnerDevice;
  if Owner = PortOwnerDevice then begin
    Result := ''
  end else begin
    Result := PortOwnerDevice.DeviceIdentifier + '.';
  end;
  Result += Port.DeviceIdentifier;
end;

Procedure TConnector.DoPaint(Sender: TObject);
begin
  //WriteLn('TConnector.Paint Self = ', Name, '(', Left,', ', Top,', ', Width,', ', Height, ')');
  if Length(FPoints) > 0 then with Canvas do begin
    {if FSelected then begin
      Color := clBlack;
      Brush.Color := clGray;
      InflateRect(PaintRect, -2, -2);
    end;}
    If not Enabled then
      Color := clBtnShadow
    else
      Color:= clGreen;
    Polyline(Translate(FPoints, -Left, -Top));
  end;
end;

procedure TConnector.SetInputPort(Value: IInputPort);
begin
  FInputPort := Value;
  SetProperty('InputPort', GetPortIdentifier(Value));
  FInputPort.Connector := Self;
  if Assigned(FOutputPort) then begin
    UpdatePoints;
  end;
end;

procedure TConnector.SetOutputPort(Value: IOutputPort);
begin
  FOutputPort := Value;
  SetProperty('OutputPort', GetPortIdentifier(Value));
  FOutputPort.Connector := Self;
  if Assigned(FInputPort) then begin
    UpdatePoints;
  end;
end;

procedure TConnector.UpdatePoints;
var
  P1, P2: TPoint;
begin
  P1 := RectCenter(FOutputPort.OriginalBounds);
  P2 := RectCenter(FInputPort.OriginalBounds);
  if Route(P1, P2, nil, FPoints) then begin
    //WriteLn('OutputPort = (', FPoints[0].x, ', ', FPoints[0].y, ' ), InputPort = (', FPoints[3].x, ', ', FPoints[3].y, ' )');
    BoundsRect := Bounds(FPoints);
    //WriteLn('Left = ', Left, ', Top = ', Top, ', Width = ', Width, ', Height = ', Height);
  end;
end;

constructor TBlock.Create(AOwner:TComponent);
var
  R: TRect;
begin
  inherited Create(AOwner);
  Caption := GetDeviceRandomCaption('Block ');
  FInputComponentCount := 0;
  FOutputComponentCount := 0;
  with R do begin
    Left := 0;
    Top := 0;
    Right := DefaultBlockWidth;
    Bottom := DefaultBlockHeight;
  end;
  OriginalBounds := R;
  FSelected := False;
  if AOwner is TWinControl then begin
    Parent := TWinControl(AOwner);
  end else begin
    Parent := TDevice(AOwner).Parent
  end;
  OnMouseDown := @StartMove;
  OnMouseUp := @EndMove;
  OnMouseMove := @Move;
  OnMouseLeave := @MouseLeaved;
  Canvas.Brush.Color := clRed;
end;

destructor TBlock.Destroy;
begin
  inherited Destroy;
end;

function TBlock.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  R: TRect;
begin
  Result := Load() and inherited;
  Caption := GetPropertyValue(ContextNode, 'Caption', DesignDescription);
  TreeNode.Text := Caption;
  with R do begin
    Left := StrToInt(GetPropertyValue(ContextNode, 'Left', DesignDescription));
    Top := StrToInt(GetPropertyValue(ContextNode, 'Top', DesignDescription));
    Right := Left + StrToInt(GetPropertyValue(ContextNode, 'Width', DesignDescription));
    Bottom := Top + StrToInt(GetPropertyValue(ContextNode, 'Height', DesignDescription));
    //WriteLn('loaded bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  end;
  Canvas.Brush.Color := StrToInt(GetPropertyValue(ContextNode, 'Color', DesignDescription));
  OriginalBounds := R;
end;

function TBlock.Load: boolean;
var
  DesignDescription: TLFMTree;
  DeviceDescriptionNode: TLFMObjectNode;
  PortName: string;
  BlockName: string;
  p: Integer;
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
  Device: TDevice;
  Port: TPort;
  InputPort: IInputPort;
  OutputPort: IOutputPort;
  Block: TBlock;
  DeviceClass: TDeviceClass;
begin
  Result := true;
  codeFile[ctSource] := SourceFileName(Name);
  codeFile[ctDescription] := ReSourceFileName(Name);
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
    //WriteLn('TBlock.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    //WriteLn('TBlock.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False, False) then begin
      if not Assigned(DesignDescription) then begin
        Exit(False);
      end else begin
        WriteLn('Errors encountered while loading design');
      end;
    end;
  end;
  //WriteLn('TBlock.Load : LFM created');
  DeviceDescriptionNode := FindObjectProperty(nil, DesignDescription);
  Port := nil;
  while Assigned(DeviceDescriptionNode) do begin
    //WriteLn('DeviceDescriptionNode.TypeName = ', DeviceDescriptionNode.TypeName);
    DeviceClass := GetDeviceClass(DeviceDescriptionNode.TypeName);
    if Assigned(DeviceClass) and DeviceClass.InheritsFrom(TPort) then begin
      Port := AddNewPort(DeviceDescriptionNode.Name, DeviceDescriptionNode.TypeName);
    end else if DeviceDescriptionNode.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(DeviceDescriptionNode, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      //WriteLn('BlockName = ', BlockName);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('OutputPortName = ', PortName);
      if p = 0 then begin
        Device := Self;
      end else begin
        Device := FindComponent(BlockName) as TDevice;
      end;
      //WriteLn('Block.Name = ', Device.DeviceIdentifier, ', Block.Type = ', Device.DeviceType);
      Device := Device.FindComponent(PortName) as TDevice;
      //WriteLn('OutputPort.Name = ', Device.DeviceIdentifier, ', OutputPort.Type = ', Device.DeviceType);
      if Device is TOutputPort then begin
        OutputPort := Device as TOutputPort;
      end else if Device is TInputPortRef then begin
        OutputPort := Device as TInputPortRef;
      end;
      PortName := GetPropertyValue(DeviceDescriptionNode, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      //WriteLn('BlockName = ', BlockName);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('InputPortName = ', PortName);
      if p = 0 then begin
        Device := Self;
      end else begin
        Device := FindComponent(BlockName) as TDevice;
      end;
      //WriteLn('Block.Name = ', Device.DeviceIdentifier, ', Block.Type = ', Device.DeviceType);
      Device := Device.FindComponent(PortName) as TDevice;
      //WriteLn('InputPort.Name = ', Device.DeviceIdentifier, ', InputPort.Type = ', Device.DeviceType);
      //WriteLn('InputPortName = ', PortName);
      if Device is TInputPort then begin
        InputPort := Device as TInputPort;
      end else if Device is TOutputPortRef then begin
        InputPort := Device as TOutputPortRef;
      end;
      Device := AddNewConnector(DeviceDescriptionNode.Name, DeviceDescriptionNode.TypeName, OutputPort, InputPort);
      Device.Load(DesignDescription, DeviceDescriptionNode);
    end else begin
      Block := AddNewSubBlock(DeviceDescriptionNode.Name, DeviceDescriptionNode.TypeName, '');
      with Block do begin
        Load(DesignDescription, DeviceDescriptionNode);
      end;
      //WriteLn('++++++++++++++');
    end;
    if Assigned(Port) then with Port do begin
      Parent := Self.Parent;
      Load(DesignDescription, DeviceDescriptionNode);
      Port := nil;
    end;
    DeviceDescriptionNode := FindObjectProperty(DeviceDescriptionNode, DesignDescription);
  end;
end;

function TBlock.Save: boolean;
var
  Component: TComponent;
  i: Integer;
  NewFileName: string;
begin
  CodeBuffer[ctDescription] := GetCodeBuffer(cttDescription,Self);
  CodeBuffer[ctDescription].Source := DeviceDescription('');
  Result := CodeBuffer[ctDescription].Save;
  if Assigned(CodeBuffer[ctSource]) then begin
    NewFileName := SourceFileName(DeviceIdentifier);
    if CodeBuffer[ctSource].Filename <> NewFileName then begin
      CodeToolBoss.SaveBufferAs(CodeBuffer[ctSource], NewFileName, CodeBuffer[ctSource]);
    end;
  end else begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttBlock, Self);
  end;
  Result := Result and CodeBuffer[ctSource].Save;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TBlock then with Component as TBlock do begin
      Result := Result and Save;
    end;
  end;
end;

procedure TBlock.Magnify(m: Real);
begin
  inherited Magnify(m);
  UpdatePortsBounds(TInputPort);
  UpdatePortsBounds(TOutputPort);
end;

function TBlock.AddNewConnector(ADeviceName, ADeviceType: string; AOutputPort: IOutputPort; AInputPort: IInputPort): TConnector;
begin
  Result := CreateConnector(ADeviceName, ADeviceType, Self);
  with Result do begin
    Parent := Self.Parent;
    Connect(AOutputPort, AInputPort);
  end;
  if Assigned(FOnChildrenCreate) then begin
    FOnChildrenCreate(Result);
  end;
  if not Assigned(CodeBuffer[ctSource]) then begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttDesign, Self);
  end;
end;

function TBlock.AddNewPort(ADeviceName, ADeviceType: string): TPort;
begin
  if CreateDevice(TDevice(Result), ADeviceName, ADeviceType, '', Self) then begin
    if Assigned(FOnChildrenCreate) then begin
      FOnChildrenCreate(Result);
    end;
    InsertDevice(Result, Self);
  end;
end;

function TBlock.BlockBoundsDescription(Indent: string): string;
begin
  with OriginalBounds do begin
    Result := Indent + '  Left = ' + IntToStr(Left) + LineEnding +
              Indent + '  Top = ' + IntToStr(Top) + LineEnding +
              Indent + '  Width = ' + IntToStr(Right - Left) + LineEnding +
              Indent + '  Height = ' + IntToStr(Bottom - Top) + LineEnding;
  end;
end;

function TBlock.DeviceCodeTemplateType: TCodeTemplateType;
begin
  Result := cttBlock;
end;

function TBlock.DevicePropertiesDescription(Indent: string): string;
var
  i: Integer;
begin
  Result := '';
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TDevice do begin
      Result += DeviceDescription(Indent + '  ');
    end;
  end else begin
    Result += Indent + '  Caption = ''' + Caption + '''' + LineEnding;
    Result += inherited;
    Result += Indent + '  Color = $' + HexStr(Canvas.Brush.Color, 8) + LineEnding;
    Result += BlockBoundsDescription(Indent);
  end;
end;

function TBlock.DeviceUnitName: string;
begin
  Result := Copy(FDeviceType, 2, Length(FDeviceType) - 1);
end;

procedure TBlock.StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:begin
      PointedDevice := Self;
      _MousePos.x := X + Left;
      _MousePos.y := Y + Top;
    end;
    mbRight:begin
      PopupMenu.PopUp;
    end;
  end;
end;

procedure TBlock.EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
  case Button of
    mbLeft:begin
      PointedDevice := nil;
    end;
  end;
end;

procedure TBlock.Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
  R: TRect;
begin
  if Sender <> Self then begin
    PointedDevice := nil;
  end;
  if PointedDevice = Self then begin
    X += Left;
    Y += Top;
    dx := X - _MousePos.x;
    dy := Y - _MousePos.y;
    _MousePos.x := X;
    _MousePos.y := Y;
    R := OriginalBounds;
    with R do begin
      Left += dx;
      Top += dy;
      Right += dx;
      Bottom += dy;
    end;
    OriginalBounds := R;
    UpdatePortsBounds(TInputPort);
    UpdatePortsBounds(TOutputPort);
  end;
end;

procedure TBlock.SetControlsVisibility(Visibility: Boolean);
var
  i: Integer;
  Component: TComponent;
begin
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TControl then with Component as TControl do begin
      Visible := Visibility;
      if Component is TBlock then with Component as TBlock do begin
        SetControlsVisibility(Visibility);
      end;
    end;
  end;
end;

procedure TBlock.SetSelected(AValue: Boolean);
begin
  if FSelected <> AValue then begin
    FSelected := AValue;
    if Assigned(TreeNode) then begin
      TreeNode.Selected := AValue;
    end;
    Refresh;
  end
end;

procedure TBlock.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  TXTStyle : TTextStyle;
begin
  //WriteLn('TBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  //with PaintRect do WriteLn('TBlock.Paint PaintRect=', Left,', ', Top,', ', Right,', ', Bottom);
  with Canvas do begin
    if FSelected then begin
      Pen.Color := clBlack;
      Rectangle(PaintRect);
      InflateRect(PaintRect, -2, -2);
    end;
    If not Enabled then
      Brush.Color := clBtnShadow;
    Rectangle(PaintRect);
    if Self is TDesign then with PaintRect do begin
      Line(Left, Top, Right, Bottom);
      Line(Left, Bottom, Right, Top);
    end;
    if Caption <> '' then begin
      TXTStyle := Canvas.TextStyle;
      with TXTStyle do begin
        Opaque := False;
        Clipping := True;
        ShowPrefix := False;
        Alignment := taCenter;
        Layout := tlCenter;
      end;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
      Font.Color := Self.Font.Color;
      TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
    end;
  end;
end;

procedure TBlock.UpdatePortsBounds(PortType: TPortType);
var
  i: Integer;
  Idx: Integer;
  dy: Integer;
  Component: TComponent;
  R: TRect;
begin
  R := OriginalBounds;
  if (PortType = TInputPort) and (FInputComponentCount > 0) then
    dy := (R.Bottom - R.Top) div FInputComponentCount
  else if (PortType = TOutputPort) and (FOutputComponentCount > 0) then
    dy := (R.Bottom - R.Top) div FOutputComponentCount
  else
    Exit;
  Idx := 0;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is PortType then with Component as PortType do begin
      UpdateBounds(Idx, dy);
      Idx += 1;
    end;
  end;
end;

procedure TBlock.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TInputPort then begin
    FInputComponentCount += 1;
    UpdatePortsBounds(TInputPort);
  end else if AComponent is TOutputPort then begin
    FOutputComponentCount += 1;
    UpdatePortsBounds(TOutputPort);
  end;
end;

constructor TSource.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AddNewPort('', 'TOutputPort');
end;

procedure TSource.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  TXTStyle : TTextStyle;
begin
  //WriteLn('TBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  //with PaintRect do WriteLn('TBlock.Paint PaintRect=', Left,', ', Top,', ', Right,', ', Bottom);
  with Canvas do begin
    if FSelected then begin
      Pen.Color := clBlack;
      Ellipse(PaintRect);
      InflateRect(PaintRect, -2, -2);
    end;
    If not Enabled then
      Brush.Color := clBtnShadow;
    Ellipse(PaintRect);
    if Caption <> '' then begin
      TXTStyle := Canvas.TextStyle;
      with TXTStyle do begin
        Opaque := False;
        Clipping := True;
        ShowPrefix := False;
        Alignment := taCenter;
        Layout := tlCenter;
      end;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
      Font.Color := Self.Font.Color;
      TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
    end;
  end;
end;

function TSource.AddNewPort(ADeviceName, ADeviceType: string): TPort;
var
  c: TComponent;
begin
  Result := nil;
  if ADeviceName = '' then begin
    ADeviceName := 'Output';
  end;
  c := FindComponent(ADeviceName);
  if Assigned(c) and (c is TOutputPort) then begin
    Result := c as TOutputPort;
  end;
  if not Assigned(Result) then begin
    if CreateDevice(TDevice(Result), ADeviceName, ADeviceType, '', Self) then begin
      if Assigned(FOnChildrenCreate) then begin
        FOnChildrenCreate(Result);
      end;
    end;
  end;
end;

function TSource.DevicePropertiesDescription(Indent: string): string;
var
  i: Integer;
begin
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TPort do begin
      Result += DeviceDescription(Indent + '  ');
    end;
  end else begin
    Result := inherited;
  end;
end;

constructor TProbe.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AddNewPort('', 'TInputPort');
end;

procedure TProbe.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  TXTStyle : TTextStyle;
begin
  //WriteLn('TBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  //with PaintRect do WriteLn('TBlock.Paint PaintRect=', Left,', ', Top,', ', Right,', ', Bottom);
  with Canvas do begin
    if FSelected then begin
      Pen.Color := clBlack;
      Ellipse(PaintRect);
      InflateRect(PaintRect, -2, -2);
    end;
    If not Enabled then
      Brush.Color := clBtnShadow;
    Ellipse(PaintRect);
    if Caption <> '' then begin
      TXTStyle := Canvas.TextStyle;
      with TXTStyle do begin
        Opaque := False;
        Clipping := True;
        ShowPrefix := False;
        Alignment := taCenter;
        Layout := tlCenter;
      end;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
      Font.Color := Self.Font.Color;
      TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
    end;
  end;
end;

function TProbe.AddNewPort(ADeviceName, ADeviceType: string): TPort;
var
  c: TComponent;
begin
  Result := nil;
  if ADeviceName = '' then begin
    ADeviceName := 'Input';
  end;
  c := FindComponent(ADeviceName);
  if Assigned(c) and (c is TInputPort) then begin
    Result := c as TInputPort;
  end;
  if not Assigned(Result) then begin
    if CreateDevice(TDevice(Result), ADeviceName, ADeviceType, '', Self) then begin
      if Assigned(FOnChildrenCreate) then begin
        FOnChildrenCreate(Result);
      end;
    end;
  end;
end;

function TProbe.DevicePropertiesDescription(Indent: string): string;
var
  i: Integer;
begin
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TPort do begin
      Result += DeviceDescription(Indent + '  ');
    end;
  end else with OriginalBounds do begin
    Result := inherited;
  end;
end;

initialization
  TInputPort.RegisterDevice('TInputPort', 'Blocks', []);
  TOutputPort.RegisterDevice('TOutputPort', 'Blocks', []);
  TConnector.RegisterDevice('TConnector', 'Blocks', [DeviceProperty('OutputPort', lfmvSymbol, ''), DeviceProperty('InputPort', lfmvSymbol, ''), DeviceProperty('Depth', lfmvInteger, '127')]);
  TBlock.RegisterDevice('TBlock', 'Blocks', []);
  TSource.RegisterDevice('TRandomSource', 'Sources', [DeviceProperty('InitialSeed', lfmvInteger, '1234'), DeviceProperty('Amplitude', lfmvInteger, '255')]);
  TSource.RegisterDevice('TFileReadSource', 'Sources', [DeviceProperty('FileName', lfmvString, '')]);
  TProbe.RegisterDevice('TFileDumpProbe', 'Probes', [DeviceProperty('FileName', lfmvString, ''), DeviceProperty('SampleQty', lfmvInteger, IntToStr(MaxInt))]);
end.
