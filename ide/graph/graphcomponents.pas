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

var
  DesignDir: string;

type
  TCodeType = (ctSource, ctDescription);
  TCodeTemplateType = (cttNone, cttSimulator, cttDesign, cttBlock, cttSource, cttProbe);
  TIGraphDevice = interface
    function DeviceIdentifier: string;
    function DeviceType: string;
  end;
  TCGraphDevice = class(TMagnifier, TIGraphDevice)
  public
    function DeviceIdentifier: string;
    function DeviceType: string;
  end;
  TCGraphDeviceClass = class of TCGraphDevice;
  TCGraphConnector = class;
  TCGraphPort = class(TCGraphDevice)
  private
    FConnector: TCGraphConnector;
  protected
    procedure HandleMouseEnterLeaveEvents(Sender: TObject); virtual;
    procedure HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Paint; override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); virtual; abstract;
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetUpdatedDescription(Indent: string): string;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
  end;
  TCGraphInputPort = class(TCGraphPort)
  protected
    procedure Paint; override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TCGraphOutputPort = class(TCGraphPort)
  protected
    procedure Paint; override;
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TPortType = class of TCGraphPort;
  TCGraphBlock = class(TCGraphDevice)
  private
    _MouseDown: Boolean;
    _MousePos: TPoint;
    FInputComponentCount: Integer;
    FOutputComponentCount: Integer;
    procedure StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeaved(Sender: TObject);
  public
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    function AddNewPort(PortType: TPortType): TCGraphPort; virtual;
    function GetUpdatedDescription(Indent: string): string;
    function Load: boolean;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
    function Save: boolean;
    procedure Magnify(m: Real); override;
  protected
    FSelected: Boolean;
    procedure SetSeleted(AValue: Boolean);
    procedure Paint; override;
    procedure UpdatePortsBounds(PortType: TPortType);
    procedure ValidateInsert(AComponent: TComponent); override;
  published
    property Selected: Boolean read FSelected write SetSeleted;
    property InputComponentCount: Integer read FInputComponentCount;
    property OutputComponentCount: Integer read FOutputComponentCount;
  end;
  TCGraphBlockClass = class of TCGraphBlock;
  TCGraphConnector = class(TCGraphDevice)
  private
    FInputPort: TCGraphInputPort;
    FOutputPort: TCGraphOutputPort;
    FPoints: TRoute;
  public
    constructor Create(AOwner: TComponent); override;
    function GetUpdatedDescription(Indent: string): string;
    procedure Connect(AOutputPort: TCGraphOutputPort; AInputPort: TCGraphInputPort);
  protected
    procedure Paint; override;
    procedure SetInputPort(Value: TCGraphInputPort);
    procedure SetOutputPort(Value: TCGraphOutputPort);
    procedure UpdatePoints; virtual;
  published
    property InputPort: TCGraphInputPort read FInputPort write SetInputPort;
    property OutputPort: TCGraphOutputPort read FoutputPort write SetOutputPort;
  end;
  TCGraphSource = class(TCGraphBlock)
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(PortType: TPortType): TCGraphPort; override;
  end;
  TCGraphProbe = class(TCGraphBlock)
  public
    constructor Create(AOwner: TComponent);override;
    function AddNewPort(PortType: TPortType): TCGraphPort; override;
  end;

function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): string;
function FindObjectProperty(ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
function FindObjectProperty(PropertyPath: string; ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;

implementation
uses
  DesignGraph, CodeToolManager, CodeWriter;

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
  WriteLn('GetPropertyValue: PropertyName = ', PropertyName);
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

function TCGraphDevice.DeviceIdentifier: string;
begin
  Result := Name;
end;

function TCGraphDevice.DeviceType: string;
const
  Prefix = 'TCGraph';
begin
  Result := ClassName;
  if Pos(Prefix, Result) = 1 then begin
    Delete(Result, 1, Length(Prefix));
  end;
end;

constructor TCGraphPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  UpdateBounds(-1, -1);
  OnMouseEnter := @HandleMouseEnterLeaveEvents;
  OnMouseLeave := @HandleMouseEnterLeaveEvents;
  OnMouseDown := @HandleMouseDownEvents;
  OnMouseUp := @HandleMouseUpEvents;
  Name := 'Port' + IntToStr(Owner.ComponentCount);
  Parent := TCGraphDevice(AOwner).Parent;
end;

procedure TCGraphPort.HandleMouseEnterLeaveEvents(Sender: TObject);
begin
  //WriteLn('MouseEntered = ', MouseEntered);
  Repaint;
end;

procedure TCGraphPort.HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('TCGraphPort.HandleMouseDownEvents');
  case Button of
    mbLeft:with Parent as TCGraphDesign do begin
      if Self is TCGraphOutputPort then
       SelectedOutputPort := Self as TCGraphOutputPort
      else
       SelectedOutputPort := nil;
    end;
  end;
end;

procedure TCGraphPort.HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('TCGraphPort.HandleMouseUpEvents');
  case Button of
    mbLeft:with Parent as TCGraphDesign do begin
      if (Self is TCGraphInputPort) and (Assigned(SelectedOutputPort)) then begin
        SelectedInputPort := Self as TCGraphInputPort;
        //WriteLn('SelectedOutputPort = ', SelectedOutputPort.Top, ', ', SelectedOutputPort.Left);
        //WriteLn('SelectedInputPort = ', SelectedInputPort.Top, ', ', SelectedInputPort.Left);
        ConnectPorts(Self);
      end else
        SelectedInputPort := nil;
    end;
  end;
end;

procedure TCGraphPort.Paint;
var
  PaintRect: TRect;
begin
  inherited Paint;
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

function TCGraphPort.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  Path: string;
begin
  Path := '';
  while Assigned(ContextNode) do with ContextNode do begin
    Path := Name + '.' + Path;
    ContextNode := Parent as TLFMObjectNode;
  end;
  //WriteLn('TCGraphPort : Path = ', Path);
//  Left := StrToInt(GetPropertyValue(DesignDescription, Path + 'Left'));
//  Top := StrToInt(GetPropertyValue(DesignDescription, Path + 'Top'));
  Color := clBlack;
//  Caption := GetPropertyValue(DesignDescription, Path + 'Caption');
  Result := True;
end;

function TCGraphPort.GetUpdatedDescription(Indent: string): string;
var
  PortType: string;
begin
  if Self is TCGraphInputPort then
    PortType := 'Input'
  else if Self is TCGraphOutputPort then
    PortType := 'Output'
  else
    PortType := '';
  Result := Indent + 'object ' + Name + ': T' + PortType + 'Port' + LineEnding +
    Indent + 'end' + LineEnding;
end;

procedure TCGraphPort.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TCGraphBlock then with AComponent as TCGraphBlock do begin
    ValidateInsert(Self);
  end;
end;

procedure TCGraphInputPort.Paint;
var
  PaintRect: TRect;
  Triangle: array[0..2] of TPoint;
begin
  inherited Paint;
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

procedure TCGraphInputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
  R: TRect;
begin
  with Owner as TCGraphBlock do begin
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

procedure TCGraphOutputPort.Paint;
var
  PaintRect: TRect;
  Triangle: array[0..2] of TPoint;
begin
  inherited Paint;
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

procedure TCGraphOutputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
  R: TRect;
begin
  with Owner as TCGraphBlock do begin
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

constructor TCGraphBlock.Create(AOwner:TComponent);
var
  R: TRect;
begin
  inherited Create(AOwner);
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
    Parent := TCGraphDevice(AOwner).Parent
  end;
  OnMouseDown := @StartMove;
  OnMouseUp := @EndMove;
  OnMouseMove := @Move;
  OnMouseLeave := @MouseLeaved;
  Canvas.Brush.Color := clRed;
end;

destructor TCGraphBlock.Destroy;
begin
  inherited Destroy;
end;

function TCGraphBlock.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
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

function TCGraphBlock.Load: boolean;
var
  DesignDescription: TLFMTree;
  DeviceDescription: TLFMObjectNode;
  PortName: string;
  BlockName: string;
  p: Integer;
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
  ChildNode: TLFMTreeNode;
  Port: TCGraphPort;
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
    WriteLn('Exiting');
    Exit(False);
  end;
  with CodeToolBoss do begin
    WriteLn('TCGraphBlock.Load : CodeBuffer[ctDescription] = "', CodeBuffer[ctDescription].Filename, '"');
    WriteLn('TCGraphBlock.Load : CodeBuffer[ctSource] = "', CodeBuffer[ctSource].Filename, '"');
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], DesignDescription, False, False) then begin
      if not Assigned(DesignDescription) then begin
        Exit(False);
      end else begin
        WriteLn('Errors encountered while loading design');
      end;
    end;
  end;
  WriteLn('TCGraphBlock.Load : LFM created');
  DeviceDescription := FindObjectProperty(nil, DesignDescription);
  Port := nil;
  while Assigned(DeviceDescription) do begin
    WriteLn('DeviceDescription.TypeName = ', DeviceDescription.TypeName);
    if DeviceDescription.TypeName = 'TOutputPort' then
      Port := TCGraphOutputPort.Create(Self)
    else if DeviceDescription.TypeName = 'TInputPort' then
      Port := TCGraphInputPort.Create(Self)
    else if DeviceDescription.TypeName = 'TConnector' then begin
      PortName := GetPropertyValue(DeviceDescription, 'OutputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      WriteLn('OutputPortName = ', PortName);
      //SelectedOutputPort := FindComponent(BlockName).FindComponent(PortName) as TCGraphOutputPort;
      PortName := GetPropertyValue(DeviceDescription, 'InputPort', DesignDescription);
      p := Pos('.', PortName);
      BlockName := Copy(PortName, 1, p - 1);
      PortName := Copy(PortName, p + 1, length(PortName));
      WriteLn('InputPortName = ', PortName);
      //SelectedInputPort := FindComponent(BlockName).FindComponent(PortName) as TCGraphInputPort;
      //ConnectPorts(Self);
    end else begin
      {if Assigned(SelectedBlock) then
        SelectedBlock.Selected := False;
      SelectedBlock := TCGraphBlock.Create(Self);
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
      Load(DesignDescription, DeviceDescription);
      OnDblClick := Self.OnDblClick;
      Port := nil;
    end;
    DeviceDescription := FindObjectProperty(DeviceDescription, DesignDescription);
  end;
  Selected := True;
end;

function TCGraphBlock.Save: boolean;
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
  CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttBlock, Self);
  UpdateUsedBlocks(Self, CodeBuffer[ctSource]);
  Result := Result and CodeBuffer[ctSource].Save;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      Result := Result and Save;
    end;
  end;
end;

procedure TCGraphBlock.Magnify(m: Real);
begin
  inherited Magnify(m);
  UpdatePortsBounds(TCGraphInputPort);
  UpdatePortsBounds(TCGraphOutputPort);
end;

function TCGraphBlock.AddNewPort(PortType: TPortType): TCGraphPort;
begin
  Result := PortType.Create(Self);
end;

function TCGraphBlock.GetUpdatedDescription(Indent: string): string;
var
  i: Integer;
begin
  Result := Indent + 'object ' + Name + ': T' + Name + LineEnding;
  if Indent = '' then begin
    for i := 0 to ComponentCount - 1 do with Components[i] as TCGraphPort do begin
      Result += GetUpdatedDescription(Indent + '  ');
    end;
  end else with OriginalBounds do begin
    Result +=  '  DeviceName = ''' + Caption + '''' + LineEnding +
      Indent + '  Color = $' + HexStr(Canvas.Brush.Color, 8) + LineEnding +
      Indent + '  Left = ' + IntToStr(Left) + LineEnding +
      Indent + '  Top = ' + IntToStr(Top) + LineEnding +
      Indent + '  Width = ' + IntToStr(Right - Left) + LineEnding +
      Indent + '  Height = ' + IntToStr(Bottom - Top) + LineEnding;
  end;
  Result += Indent + 'end' + LineEnding;
end;

procedure TCGraphBlock.StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TCGraphBlock.EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
  case Button of
    mbLeft:begin
      _MouseDown := False;
    end;
  end;
end;

procedure TCGraphBlock.MouseLeaved(Sender: TObject);
begin
  _MouseDown := False;
end;

procedure TCGraphBlock.Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
    UpdatePortsBounds(TCGraphInputPort);
    UpdatePortsBounds(TCGraphOutputPort);
  end;
end;

procedure TCGraphBlock.SetSeleted(AValue: Boolean);
begin
  if FSelected <> AValue then begin
    FSelected := AValue;
    Refresh;
  end
end;

procedure TCGraphBlock.Paint;
var
  PaintRect: TRect;
  TXTStyle : TTextStyle;
begin
  //WriteLn('TCGraphBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  //with PaintRect do WriteLn('TCGraphBlock.Paint PaintRect=', Left,', ', Top,', ', Right,', ', Bottom);
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
  inherited Paint;
end;

procedure TCGraphBlock.UpdatePortsBounds(PortType: TPortType);
var
  i: Integer;
  Idx: Integer;
  dy: Integer;
  Component: TComponent;
  R: TRect;
begin
  R := OriginalBounds;
  if (PortType = TCGraphInputPort) and (FInputComponentCount > 0) then
    dy := (R.Bottom - R.Top) div FInputComponentCount
  else if (PortType = TCGraphOutputPort) and (FOutputComponentCount > 0) then
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

procedure TCGraphBlock.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TCGraphInputPort then begin
    FInputComponentCount += 1;
    UpdatePortsBounds(TCGraphInputPort);
  end else if AComponent is TCGraphOutputPort then begin
    FOutputComponentCount += 1;
    UpdatePortsBounds(TCGraphOutputPort);
  end;
end;

constructor TCGraphConnector.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'Connector' + IntToStr(AOwner.ComponentCount);
end;

function TCGraphConnector.GetUpdatedDescription(Indent: string): string;
begin
  Result := Indent + 'object ' + Name + ': TConnector' + LineEnding +
    Indent + '  OutputPort = ' + OutputPort.Owner.Name + '.' + OutputPort.Name + LineEnding +
    Indent + '  InputPort = ' + InputPort.Owner.Name + '.' + InputPort.Name + LineEnding +
    Indent + 'end' + LineEnding;
end;

procedure TCGraphConnector.Connect(AOutputPort: TCGraphOutputPort; AInputPort: TCGraphInputPort);
begin
  OutputPort := AOutputPort;
  InputPort := AInputPort;
end;

Procedure TCGraphConnector.Paint;
begin
  //WriteLn('TCGraphConnector.Paint Self=', Left,', ', Top,', ', Width,', ', Height);
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
  inherited Paint;
end;

procedure TCGraphConnector.SetInputPort(Value: TCGraphInputPort);
begin
  FInputPort := Value;
  FInputPort.FConnector := Self;
  if Assigned(FOutputPort) then begin
    UpdatePoints;
  end;
end;

procedure TCGraphConnector.SetOutputPort(Value: TCGraphOutputPort);
begin
  FOutputPort := Value;
  FOutputPort.FConnector := Self;
  if Assigned(FInputPort) then begin
    UpdatePoints;
  end;
end;

procedure TCGraphConnector.UpdatePoints;
var
  P1, P2: TPoint;
begin
  P1 := RectCenter(FOutputPort.BoundsRect);
  P2 := RectCenter(FInputPort.BoundsRect);
  FPoints := Route(P1, P2, nil);
  //WriteLn('OutputPort = (', FPoints[0].x, ', ', FPoints[0].y, ' ), InputPort = (', FPoints[3].x, ', ', FPoints[3].y, ' )');
  BoundsRect := Bounds(FPoints);
  //WriteLn('Left = ', Left, ', Top = ', Top, ', Width = ', Width, ', Height = ', Height);
end;

constructor TCGraphSource.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AddNewPort(TCGraphInputPort);
end;

procedure TCGraphSource.Paint;
var
  PaintRect: TRect;
  TXTStyle : TTextStyle;
begin
  //WriteLn('TCGraphBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  //with PaintRect do WriteLn('TCGraphBlock.Paint PaintRect=', Left,', ', Top,', ', Right,', ', Bottom);
  with Canvas do begin
    if FSelected then begin
      Pen.Color := clBlack;
      Rectangle(PaintRect);
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
  inherited Paint;
end;

function TCGraphSource.AddNewPort(PortType: TPortType): TCGraphPort;
var
  c: TComponent;
begin
  Result := nil;
  c := FindComponent('Output');
  if Assigned(c) and (c is TCGraphOutputPort) then begin
    Result := c as TCGraphOutputPort;
  end;
  if not Assigned(Result) then begin
    Result := TCGraphOutputPort.Create(Self);
    Result.Name := 'Output';
  end;
end;

constructor TCGraphProbe.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  AddNewPort(TCGraphOutputPort);
end;

function TCGraphProbe.AddNewPort(PortType: TPortType): TCGraphPort;
var
  c: TComponent;
begin
  Result := nil;
  c := FindComponent('Input');
  if Assigned(c) and (c is TCGraphInputPort) then begin
    Result := c as TCGraphInputPort;
  end;
  if not Assigned(Result) then begin
    Result := TCGraphInputPort.Create(Self);
    Result.Name := 'Input';
  end;
end;

end.

