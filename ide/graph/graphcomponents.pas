unit GraphComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Types, CodeCache, LFMTrees, Routing, Magnifier;

const
  DefaultBlockWidth = 100;
  DefaultBlockHeight = 100;
  DefaultPortWidth = 10;
  DefaultPortHeight = 10;
  MinPortSpacing = 10;

type
  TCodeType = (ctSource, ctDescription);
  TCodeTemplateType = (cttNone, cttDescription, cttSimulator, cttDesign, cttBlock, cttSource, cttProbe);
  TIGraphDevice = interface
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceAncestorType: string;
    function DeviceDescription(Indent: string): string;
  end;
  TDevice = class(TMagnifier, TIGraphDevice)
  private
    FOnCreate: TNotifyEvent;
    FDeviceType: string;
    FDeviceAncestorType: string;
  protected
    procedure SetName(const Value: TComponentName); override;
    procedure DoPaint(Sender: TObject); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    function DeviceIdentifier: string;
    function DeviceType: string;
    function DeviceAncestorType: string;
    function DeviceDescription(Indent: string): string; virtual;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; virtual; abstract;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
  end;
  TDeviceClass = class of TDevice;
  TConnector = class;
  TPort = class(TDevice)
  private
    FConnector: TConnector;
  protected
    procedure HandleMouseEnterLeaveEvents(Sender: TObject); virtual;
    procedure DoPaint(Sender: TObject); override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); virtual; abstract;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function DeviceDescription(Indent: string): string; override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
  end;
  TInputPort = class(TPort)
  protected
    procedure DoPaint(Sender: TObject); override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  TOutputPort = class(TPort)
  protected
    procedure DoPaint(Sender: TObject); override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;
  TPortType = class of TPort;
  TConnector = class(TDevice)
  private
    FInputPort: TInputPort;
    FOutputPort: TOutputPort;
    FPoints: TRoute;
  protected
    procedure DoPaint(Sender: TObject); override;
    procedure SetInputPort(Value: TInputPort);
    procedure SetOutputPort(Value: TOutputPort);
    procedure UpdatePoints; virtual;
  public
    Depth: Integer;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DeviceDescription(Indent: string): string; override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
    procedure Connect(AOutputPort: TOutputPort; AInputPort: TInputPort);
  published
    property InputPort: TInputPort read FInputPort write SetInputPort;
    property OutputPort: TOutputPort read FoutputPort write SetOutputPort;
  end;
  TBlock = class(TDevice)
  private
    _MouseDown: Boolean;
    _MousePos: TPoint;
    FInputComponentCount: Integer;
    FOutputComponentCount: Integer;
    FOnChildrenCreate: TNotifyEvent;
    procedure StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeaved(Sender: TObject);
  protected
    FSelected: Boolean;
    procedure SetSeleted(AValue: Boolean);
    procedure DoPaint(Sender: TObject); override;
    procedure UpdatePortsBounds(PortType: TPortType);
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function AddNewPort(PortType: TPortType): TPort; virtual;
    function DeviceDescription(Indent: string): string; override;
    function Load: boolean;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
    function Save: boolean;
    procedure Magnify(m: Real); override;
    property OnChildrenCreate: TNotifyEvent read FOnChildrenCreate write FOnChildrenCreate;
  published
    property Selected: Boolean read FSelected write SetSeleted;
    property InputComponentCount: Integer read FInputComponentCount;
    property OutputComponentCount: Integer read FOutputComponentCount;
  end;
  TBlockClass = class of TBlock;
  TSource = class(TBlock)
  protected
    procedure DoPaint(Sender: TObject); override;
  public
    InitialSeed: Integer;
    Amplitude: Integer;
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(PortType: TPortType): TPort; override;
    function DeviceDescription(Indent: string): string; override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
  end;
  TProbe = class(TBlock)
  protected
    procedure DoPaint(Sender: TObject); override;
  public
    FileName: string;
    SampleQty: Integer;
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(PortType: TPortType): TPort; override;
    function DeviceDescription(Indent: string): string; override;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean; override;
  end;

function CreateDevice(DeviceName, DeviceType: string; AncestorType: TDeviceClass; AOwner: TComponent): TDevice;
function CreateBlock(DeviceName, DeviceType: string; AOwner: TComponent): TBlock;
function CreateInputPort(DeviceName, DeviceType: string; AOwner: TComponent): TInputPort;
function CreateOutputPort(DeviceName, DeviceType: string; AOwner: TComponent): TOutputPort;
function CreateConnector(DeviceName, DeviceType: string; AOwner: TComponent): TConnector;
function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): string;
function FindObjectProperty(ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
function FindObjectProperty(PropertyPath: string; ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;

implementation
uses
  DesignGraph, CodeToolManager, CodeWriter, Configuration;

function GetDeviceClass(DeviceType: string): TDeviceClass;
begin
  if Pos('T', DeviceType) <> 1 then begin
    Result := nil;
  end else if DeviceType = 'TInputPort' then begin
    Result := TInputPort;
  end else if DeviceType = 'TOutputPort' then begin
    Result := TOutputPort;
  end else if DeviceType = 'TPort' then begin
    Result := TPort;
  end else if DeviceType = 'TConnector' then begin
    Result := TConnector;
  end else if DeviceType = 'TBlock' then begin
    Result := TBlock;
  end else if DeviceType = 'TRandomSource' then begin
    Result := TSource;
  end else if DeviceType = 'TFileDumpProbe' then begin
    Result := TProbe;
  end else begin
    Result := nil;
  end;
end;

function CreateDevice(DeviceName, DeviceType: string; AncestorType: TDeviceClass; AOwner: TComponent): TDevice;
var
  DeviceClass: TDeviceClass;
  CodeFile: string;
  ACodeBuffer: TCodeBuffer;
  DeviceAncestorType: string;
begin
  if DeviceName = '' then begin
    if DeviceType = '' then begin
      DeviceType := AncestorType.ClassName;
      if Pos('T', DeviceType) = 1 then begin
        DeviceType := 'T' + Copy(DeviceType, 8, Length(DeviceType));
      end
    end;
    DeviceAncestorType := DeviceType;
    if Pos('T', DeviceType) = 1 then begin
      DeviceName := Copy(DeviceType, 2, Length(DeviceType));
    end else begin
      DeviceName := 'A' + DeviceType;
    end;
    DeviceName += IntToStr(AOwner.ComponentCount + 1);
    DeviceType := 'T' + DeviceName;
  end else begin
    if DeviceType = '' then begin
      DeviceType := 'T' + DeviceName;
    end else begin
      CodeFile := SourceFileName(DeviceName);
      //codeFile[ctDescription] := DesignDir + BlockDescription.Name + '.lfm';
      ACodeBuffer := GetCodeBuffer(CodeFile, cttNone, nil);
      if Assigned(ACodeBuffer) then begin
        CodeToolBoss.FindFormAncestor(ACodeBuffer, DeviceType, DeviceAncestorType, True);
      end;
      //WriteLn('DeviceName = ', DeviceName, ', DeviceType = ', DeviceType, ', DeviceAncestorType = ', DeviceAncestorType);
    end;
  end;
  DeviceClass := GetDeviceClass(DeviceType);
  if not Assigned(DeviceClass) then begin
    DeviceClass := GetDeviceClass(DeviceAncestorType);
  end;
  if Assigned(DeviceClass) then begin
    Result := DeviceClass.Create(AOwner);
    if Assigned(Result) then begin
      if not (Result is AncestorType) then begin
        FreeAndNil(Result);
      end;
    end;
    if not Assigned(Result) then begin
      Result := AncestorType.Create(AOwner);
    end;
    if DeviceName <> '' then begin
      Result.Name := DeviceName;
    end;
    if DeviceType <> '' then begin
      Result.FDeviceType := DeviceType;
    end;
    if DeviceAncestorType <> '' then begin
      Result.FDeviceAncestorType := DeviceAncestorType;
    end;
  end else begin
    Result := nil;
  end;
end;

function CreateBlock(DeviceName, DeviceType: string; AOwner: TComponent): TBlock;
begin
  Result := CreateDevice(DeviceName, DeviceType, TBlock, AOwner) as TBlock;
end;

function CreateInputPort(DeviceName, DeviceType: string; AOwner: TComponent): TInputPort;
begin
  Result := CreateDevice(DeviceName, DeviceType, TInputPort, AOwner) as TInputPort;
end;

function CreateOutputPort(DeviceName, DeviceType: string; AOwner: TComponent): TOutputPort;
begin
  Result := CreateDevice(DeviceName, DeviceType, TOutputPort, AOwner) as TOutputPort;
end;

function CreateConnector(DeviceName, DeviceType: string; AOwner: TComponent): TConnector;
begin
  Result := CreateDevice(DeviceName, DeviceType, TConnector, AOwner) as TConnector;
end;

function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): string;
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
  ValueNode := PropertyNode.Next;
  //WriteLn('GetPropertyValue : PropertyName = ', PropertyName, ', PropertyType = ', Integer(ValueNode.TheType));
  if ValueNode.TheType = lfmnValue then with ValueNode as TLFMValueNode do
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
  if Assigned(AOwner) then begin
    if AOwner is TDevice then with AOwner as TDevice do begin
      Self.FOnCreate := FOnCreate;
    end else if AOwner is TDesign then with AOwner as TDesign do begin
      Self.FOnCreate := FOnCreate;
    end;
  end;
  OnPaint := @DoPaint;
end;

procedure TDevice.SetName(const Value: TComponentName);
var
  i: Integer;
  AName: TComponentName;
begin
  i := 0;
  AName := Value;
  repeat
    try
      inherited SetName(AName);
      i := MaxInt;
    except
      AName := Value + IntToStr(i);
      i += 1;
    end;
  until i = MaxInt;
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
  Result := FDeviceAncestorType;
end;

function TDevice.DeviceDescription(Indent: string): string;
begin
  Result := Indent + 'object ' + Name + ': ' +  FDeviceType + LineEnding +
    Indent + 'end' + LineEnding;
end;

constructor TPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
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
//  Left := StrToInt(GetPropertyValue(DesignDescription, Path + 'Left'));
//  Top := StrToInt(GetPropertyValue(DesignDescription, Path + 'Top'));
  Color := clBlack;
//  Caption := GetPropertyValue(DesignDescription, Path + 'Caption');
  Result := True;
end;

function TPort.DeviceDescription(Indent: string): string;
var
  PortType: string;
begin
  if Self is TInputPort then
    PortType := 'Input'
  else if Self is TOutputPort then
    PortType := 'Output'
  else
    PortType := '';
  Result := Indent + 'object ' + Name + ': T' + PortType + 'Port' + LineEnding +
    Indent + 'end' + LineEnding;
end;

procedure TPort.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TBlock then with AComponent as TBlock do begin
    ValidateInsert(Self);
  end;
end;

constructor TInputPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Name := 'Port';
  FDeviceType := 'TInputPort';
  FDeviceAncestorType := 'TInputPort';
  //WriteLn('AOwner.Name = ', AOwner.Name, 'Name = ', Name);
end;

procedure TInputPort.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  Triangle: array[0..2] of TPoint;
begin
  inherited DoPaint(Sender);
  PaintRect := ClientRect;
  with PaintRect do begin
    Triangle[0] := Point(Left, Top + Height div 2);
    Triangle[1] := Point(Left + Width, Top);
    Triangle[2] := Point(Left + Width, Top + Height - 1);
  end;
  with Canvas do begin
    Polygon(Triangle);
  end;
end;

procedure TInputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
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
  if Assigned(FConnector) then with FConnector do begin
    UpdatePoints;
  end;
end;

constructor TOutputPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  Name := 'Port';
  FDeviceType := 'TOutputPort';
  FDeviceAncestorType := 'TOutputPort';
  //WriteLn('AOwner.Name = ', AOwner.Name, 'Name = ', Name);
end;

procedure TOutputPort.DoPaint(Sender: TObject);
var
  PaintRect: TRect;
  Triangle: array[0..2] of TPoint;
begin
  inherited DoPaint(Sender);
  PaintRect := ClientRect;
  with PaintRect do begin
    Triangle[0] := Point(Left, Top + Height div 2);
    Triangle[1] := Point(Left + Width, Top);
    Triangle[2] := Point(Left + Width, Top + Height - 1);
  end;
  with Canvas do begin
    Polygon(Triangle);
  end;
end;

procedure TOutputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
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
  if Assigned(FConnector) then with FConnector do begin
    UpdatePoints;
  end;
end;

constructor TConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'Connector';
  FDeviceType := 'T' + Name;
  FDeviceAncestorType := 'TConnector';
  Depth := 127;
end;

destructor TConnector.Destroy;
begin
  if Assigned(FOutputPort) then begin
    FOutputPort.FConnector := nil;
    FOutputPort := nil;
  end;
  if Assigned(FInputPort) then begin
    FInputPort.FConnector := nil;
    FInputPort := nil;
  end;
  inherited Destroy;
end;

function TConnector.DeviceDescription(Indent: string): string;
begin
  Result := Indent + 'object ' + Name + ': TConnector' + LineEnding +
    Indent + '  OutputPort = ' + OutputPort.Owner.Name + '.' + OutputPort.Name + LineEnding +
    Indent + '  InputPort = ' + InputPort.Owner.Name + '.' + InputPort.Name + LineEnding +
    Indent + '  Depth = ' + IntToStr(Depth) + LineEnding +
    Indent + 'end' + LineEnding;
end;

function TConnector.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
begin
  Depth := StrToInt(GetPropertyValue(ContextNode, 'Depth', DesignDescription));
  Result := True;
end;

procedure TConnector.Connect(AOutputPort: TOutputPort; AInputPort: TInputPort);
begin
  OutputPort := AOutputPort;
  InputPort := AInputPort;
end;

Procedure TConnector.DoPaint(Sender: TObject);
begin
  //WriteLn('TConnector.Paint Self=', Left,', ', Top,', ', Width,', ', Height);
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

procedure TConnector.SetInputPort(Value: TInputPort);
begin
  FInputPort := Value;
  FInputPort.FConnector := Self;
  if Assigned(FOutputPort) then begin
    UpdatePoints;
  end;
end;

procedure TConnector.SetOutputPort(Value: TOutputPort);
begin
  FOutputPort := Value;
  FOutputPort.FConnector := Self;
  if Assigned(FInputPort) then begin
    UpdatePoints;
  end;
end;

procedure TConnector.UpdatePoints;
var
  P1, P2: TPoint;
begin
  P1 := RectCenter(FOutputPort.BoundsRect);
  P2 := RectCenter(FInputPort.BoundsRect);
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
  Name := 'Block';
  FDeviceType := 'T' + Name;
  FDeviceAncestorType := 'TBlock';
  Caption := 'Block ' + IntToStr(Owner.ComponentCount);
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
  Result := Load();
  with R do begin
    Left := StrToInt(GetPropertyValue(ContextNode, 'Left', DesignDescription));
    Top := StrToInt(GetPropertyValue(ContextNode, 'Top', DesignDescription));
    Right := Left + StrToInt(GetPropertyValue(ContextNode, 'Width', DesignDescription));;
    Bottom := Top + StrToInt(GetPropertyValue(ContextNode, 'Height', DesignDescription));
    //WriteLn('loaded bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  end;
  OriginalBounds := R;
  Canvas.Brush.Color := StrToInt(GetPropertyValue(ContextNode, 'Color', DesignDescription));
  Caption := GetPropertyValue(ContextNode, 'DeviceName', DesignDescription);
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
  ChildNode: TLFMTreeNode;
  Port: TPort;
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
    WriteLn('Exiting');
    Exit(False);
  end;
  with CodeToolBoss do begin
    WriteLn('TBlock.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    WriteLn('TBlock.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False) then begin
      if not Assigned(DesignDescription) then begin
        Exit(False);
      end else begin
        WriteLn('Errors encountered while loading design');
      end;
    end;
  end;
  WriteLn('TBlock.Load : LFM created');
  DeviceDescriptionNode := FindObjectProperty(nil, DesignDescription);
  Port := nil;
  while Assigned(DeviceDescriptionNode) do begin
    //WriteLn('DeviceDescription.TypeName = ', DeviceDescription.TypeName);
    if DeviceDescriptionNode.TypeName = 'TOutputPort' then begin
      Port := AddNewPort(TOutputPort);
      Port.Name := DeviceDescriptionNode.Name;
    end else if DeviceDescriptionNode.TypeName = 'TInputPort' then begin
      Port := AddNewPort(TInputPort);
      Port.Name := DeviceDescriptionNode.Name;
    end else if DeviceDescriptionNode.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(DeviceDescriptionNode, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('OutputPortName = ', PortName);
      PortName := GetPropertyValue(DeviceDescriptionNode, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      //WriteLn('InputPortName = ', PortName);
    end else begin
      {if Assigned(SelectedBlock) then
        SelectedBlock.Selected := False;
      SelectedBlock := TBlock.Create(Self);
      with SelectedBlock do begin
        Parent := Self;
        Name := BlockDescription.Name;
        Load(DesignDescription, BlockDescription);
        OnClick := @SelectBlock;
        OnDblClick := Self.OnDblClick;
        PopupMenu := Self.PopupMenu;
      end;}
      WriteLn('++++++++++++++');
    end;
    if Assigned(Port) then with Port do begin
      Parent := Self.Parent;
      Load(DesignDescription, DeviceDescriptionNode);
      OnDblClick := Self.OnDblClick;
      Port := nil;
    end;
    DeviceDescriptionNode := FindObjectProperty(DeviceDescriptionNode, DesignDescription);
  end;
  Selected := True;
end;

function TBlock.Save: boolean;
var
  Component: TComponent;
  i: Integer;
begin
  CodeBuffer[ctDescription] := GetCodeBuffer(cttDescription,Self);
  CodeBuffer[ctDescription].Source := DeviceDescription('');
  Result := CodeBuffer[ctDescription].Save;
  CodeBuffer[ctSource] := GetCodeBuffer(cttBlock, Self);
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

function TBlock.AddNewPort(PortType: TPortType): TPort;
begin
  Result := PortType.Create(Self);
  if not Assigned(CodeBuffer[ctSource]) then begin
    CodeBuffer[ctSource] := GetCodeBuffer(cttBlock, Self);
  end;
  if Assigned(FOnChildrenCreate) then begin
    FOnChildrenCreate(Result);
  end;
  CodeBuffer[ctSource].LockAutoDiskRevert;
  CodeToolBoss.AddUnitToMainUsesSection(CodeBuffer[ctSource], Result.DeviceIdentifier, '');
  CodeToolBoss.AddPublishedVariable(CodeBuffer[ctSource], DeviceType, Result.DeviceIdentifier, Result.DeviceType);
  CodeBuffer[ctSource].UnlockAutoDiskRevert;
end;

function TBlock.DeviceDescription(Indent: string): string;
var
  i: Integer;
begin
  Result := Indent + 'object ' + Name + ': T' + Name + LineEnding;
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TDevice do begin
      Result += DeviceDescription(Indent + '  ');
    end;
  end else with OriginalBounds do begin
    Result += Indent + '  DeviceName = ''' + Caption + '''' + LineEnding +
      Indent + '  Color = $' + HexStr(Canvas.Brush.Color, 8) + LineEnding +
      Indent + '  Left = ' + IntToStr(Left) + LineEnding +
      Indent + '  Top = ' + IntToStr(Top) + LineEnding +
      Indent + '  Width = ' + IntToStr(Right - Left) + LineEnding +
      Indent + '  Height = ' + IntToStr(Bottom - Top) + LineEnding;
  end;
  Result += Indent + 'end' + LineEnding;
end;

procedure TBlock.StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  case Button of
    mbLeft:begin
      _MouseDown := True;
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
      _MouseDown := False;
    end;
  end;
end;

procedure TBlock.MouseLeaved(Sender: TObject);
begin
  _MouseDown := False;
end;

procedure TBlock.Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  dx, dy: Integer;
  R: TRect;
begin
  if(Sender = Self)and _MouseDown then begin
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

procedure TBlock.SetSeleted(AValue: Boolean);
begin
  if FSelected <> AValue then begin
    FSelected := AValue;
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
  AddNewPort(TInputPort);
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

function TSource.AddNewPort(PortType: TPortType): TPort;
var
  c: TComponent;
begin
  Result := nil;
  c := FindComponent('Output');
  if Assigned(c) and (c is TOutputPort) then begin
    Result := c as TOutputPort;
  end;
  if not Assigned(Result) then begin
    Result := TOutputPort.Create(Self);
    Result.Name := 'Output';
  end;
end;

function TSource.DeviceDescription(Indent: string): string;
var
  i: Integer;
begin
  Result := Indent + 'object ' + Name + ': T' + Name + LineEnding;
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TPort do begin
      if Name <> 'Input' then begin
        Result += DeviceDescription(Indent + '  ');
      end;
    end;
  end else with OriginalBounds do begin
    Result += Indent + '  DeviceName = ''' + Caption + '''' + LineEnding +
      Indent + '  InitialSeed = ' + IntToStr(InitialSeed) + LineEnding +
      Indent + '  Amplitude = ' + IntToStr(Amplitude) + LineEnding +
      Indent + '  Color = $' + HexStr(Canvas.Brush.Color, 8) + LineEnding +
      Indent + '  Left = ' + IntToStr(Left) + LineEnding +
      Indent + '  Top = ' + IntToStr(Top) + LineEnding +
      Indent + '  Width = ' + IntToStr(Right - Left) + LineEnding +
      Indent + '  Height = ' + IntToStr(Bottom - Top) + LineEnding;
  end;
  Result += Indent + 'end' + LineEnding;
end;

function TSource.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
begin
  Result := inherited Load(DesignDescription, ContextNode);
  InitialSeed := StrToInt(GetPropertyValue(ContextNode, 'InitialSeed', DesignDescription));
  Amplitude := StrToInt(GetPropertyValue(ContextNode, 'Amplitude', DesignDescription));
end;

constructor TProbe.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AddNewPort(TOutputPort);
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

function TProbe.AddNewPort(PortType: TPortType): TPort;
var
  c: TComponent;
begin
  Result := nil;
  c := FindComponent('Input');
  if Assigned(c) and (c is TInputPort) then begin
    Result := c as TInputPort;
  end;
  if not Assigned(Result) then begin
    Result := TInputPort.Create(Self);
    Result.Name := 'Input';
  end;
end;

function TProbe.DeviceDescription(Indent: string): string;
var
  i: Integer;
begin
  Result := Indent + 'object ' + Name + ': T' + Name + LineEnding;
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TPort do begin
      if Name <> 'Output' then begin
        Result += DeviceDescription(Indent + '  ');
      end;
    end;
  end else with OriginalBounds do begin
    Result += Indent + '  DeviceName = ''' + Caption + '''' + LineEnding +
      Indent + '  FileName = ''' + FileName + '''' + LineEnding +
      Indent + '  SampleQty = ' + IntToStr(SampleQty) + LineEnding +
      Indent + '  Color = $' + HexStr(Canvas.Brush.Color, 8) + LineEnding +
      Indent + '  Left = ' + IntToStr(Left) + LineEnding +
      Indent + '  Top = ' + IntToStr(Top) + LineEnding +
      Indent + '  Width = ' + IntToStr(Right - Left) + LineEnding +
      Indent + '  Height = ' + IntToStr(Bottom - Top) + LineEnding;
  end;
  Result += Indent + 'end' + LineEnding;
end;

function TProbe.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
begin
  Result := inherited Load(DesignDescription, ContextNode);
  FileName := GetPropertyValue(ContextNode, 'FileName', DesignDescription);
  SampleQty := StrToInt(GetPropertyValue(ContextNode, 'SampleQty', DesignDescription));
end;

end.

