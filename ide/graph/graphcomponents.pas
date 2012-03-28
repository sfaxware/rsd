unit GraphComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Types, CodeCache, LFMTrees;

const
  DefaultBlockWidth = 100;
  DefaultBlockHeight = 100;
  DefaultPortWidth = 8;
  DefaultPortHeight = 4;
  MinPortSpacing = 10;

var
  DesignDir: string;

type
  TCodeType = (ctSource, ctDescription);
  TCGraphConnector = class;
  TCGraphPort = class(TGraphicControl)
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
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
    function Save(var f: Text): Boolean; virtual;
  end;
  TCGraphInputPort = class(TCGraphPort)
  protected
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TCGraphOutputPort = class(TCGraphPort)
  protected
    procedure UpdateBounds(Idx: Integer; Interval: Integer); override;
  end;
  TPortType = class of TCGraphPort;
  TCGraphBlock = class(TGraphicControl)
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
    function GetDescription: TLFMTree;
    function Load: boolean;
    function Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
    function Save: boolean;
    function Save(var f: Text): boolean;
  protected
    FSelected: Boolean;
    FType: string;
    procedure SetSeleted(AValue: Boolean);
    procedure Paint; override;
    procedure UpdatePortsBounds(PortType: TPortType);
    procedure ValidateInsert(AComponent: TComponent); override;
  published
    property Selected: Boolean read FSelected write SetSeleted;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Enabled;
    property Font;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnPaint;
    property OnResize;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Typ: string read FType;
    property InputComponentCount: Integer read FInputComponentCount;
    property OutputComponentCount: Integer read FOutputComponentCount;
  end;
  TCGraphConnector = class(TGraphicControl)
  private
    FInputPort: TCGraphInputPort;
    FOutputPort: TCGraphOutputPort;
    FPoints: array of TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Connect(AOutputPort: TCGraphOutputPort; AInputPort: TCGraphInputPort);
    function Save(var f: Text): Boolean;
  protected
    procedure Paint; override;
    procedure SetInputPort(Value: TCGraphInputPort);
    procedure SetOutputPort(Value: TCGraphOutputPort);
    procedure UpdatePoints; virtual;
  published
    property InputPort: TCGraphInputPort read FInputPort write SetInputPort;
    property OutputPort: TCGraphOutputPort read FoutputPort write SetOutputPort;
  end;

function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): string;
function FindObjectProperty(ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;
function FindObjectProperty(PropertyPath: string; ContextNode: TLFMTreeNode; Self: TLFMTree): TLFMObjectNode;

implementation
uses math, DesignGraph, CodeToolManager;

type
  TConnection = array of TPoint;

function GetPropertyValue(ContextNode: TLFMObjectNode; PropertyName: string; Self: TLFMTree): string;
var
  PropertyNode: TLFMPropertyNode;
  ValueNode: TLFMTreeNode;
  c: char;
  p: integer;
begin
  while Assigned(ContextNode) do with ContextNode do begin
    PropertyName := Name + '.' + PropertyName;
    ContextNode := Parent as TLFMObjectNode;
  end;
  Result := '';
  //WriteLn('GetPropertyValue : PropertyName = ', PropertyName);
  PropertyNode := Self.FindProperty(PropertyName, nil);
  ValueNode := PropertyNode.Next;
  if ValueNode.TheType = lfmnValue then with ValueNode as TLFMValueNode do
    case ValueType of
      lfmvString: Result := ReadString;
      lfmvInteger: begin
        p := StartPos;
        c := Tree.LFMBuffer.Source[p];
        while c in ['0'..'9'] do begin
          Result += c;
          p += 1;
          c := Tree.LFMBuffer.Source[p]
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
  p: LongInt;
  FirstPart: String;
  RestParts: String;
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
var
  p: LongInt;
  FirstPart: String;
  RestParts: String;
begin
  p:=System.Pos('.', PropertyPath);
  if p > 0 then begin
    FirstPart:=copy(PropertyPath, 1, p - 1);
    RestParts:=copy(PropertyPath, p + 1, length(PropertyPath));
  end else begin
    FirstPart := PropertyPath;
    RestParts := '';
  end;
  //WriteLn('FindObjectProperty : FirstPart = ', FirstPart, ', RestParts = ', RestParts);
  repeat
    Result := FindObjectProperty(ContextNode, Self);
    ContextNode := Result;
  until (Result = nil) or (SysUtils.CompareText(Result.Name, PropertyPath) = 0);
end;

function Translate(Points: TConnection; dx, dy: Integer): TConnection;
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

constructor TCGraphPort.Create(AOwner: TComponent);
begin
  Inherited Create(AOwner);
  UpdateBounds(-1, -1);
  OnMouseEnter := @HandleMouseEnterLeaveEvents;
  OnMouseLeave := @HandleMouseEnterLeaveEvents;
  OnMouseDown := @HandleMouseDownEvents;
  OnMouseUp := @HandleMouseUpEvents;
  Name := 'Port' + IntToStr(Owner.ComponentCount);
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
  TXTStyle : TTextStyle;
begin
  //WriteLn('TCGraphPort.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  with Canvas do begin
    //WriteLn('TCGraphPort.Paint PaintRect=',PaintRect.Left,', ',PaintRect.TOp,', ',PaintRect.Right,', ',PaintRect.Bottom,', ',caption,', ', TXTStyle.SystemFont);
    If not Enabled then
      Brush.Color := clBtnShadow
    else if MouseEntered then
      Brush.Color := clGray
    else
      Brush.Color:= clBlack;
    Color := clBlack;
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
          
function TCGraphPort.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  Path: string;
begin
  Path := '';
  while Assigned(ContextNode) do with ContextNode do begin
    Path := Name + '.' + Path;
    ContextNode := Parent as TLFMObjectNode;
  end;
  WriteLn('TCGraphPort : Path = ', Path);
//  Left := StrToInt(GetPropertyValue(DesignDescription, Path + 'Left'));
//  Top := StrToInt(GetPropertyValue(DesignDescription, Path + 'Top'));
  Color := clBlack;
//  Caption := GetPropertyValue(DesignDescription, Path + 'Name');
  Result := True;
end;

function TCGraphPort.Save(var f: Text): Boolean;
var
  PortType: string;
begin
  if Self is TCGraphInputPort then
    PortType := 'Input'
  else if Self is TCGraphOutputPort then
    PortType := 'Output'
  else
    PortType := '';
  WriteLn(f, '    object ', Name, ': T', PortType,'Port');
  WriteLn(f, '    end');
  Result := True;
end;

procedure TCGraphPort.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TCGraphBlock then with AComponent as TCGraphBlock do begin
    ValidateInsert(Self);
  end;
end;

procedure TCGraphInputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
begin
  with Owner as TCGraphBlock do begin
    if Interval <= 0 then begin
      Interval := Height div FInputComponentCount;
    end;
    if idx < 0 then begin
      idx := FInputComponentCount - 1;
    end;
    PortTop := Top + idx * Interval + Interval div 2 - DefaultPortHeight div 2;
    PortLeft := Left + Width;
  end;
  //WriteLn('idx = ', idx, ' PortTop = ', PortTop, ' PortLeft = ', PortLeft);
  ChangeBounds(PortLeft, PortTop, DefaultPortWidth, DefaultPortHeight);
  if Assigned(FConnector) then with FConnector do begin
    UpdatePoints;
  end;
end;

procedure TCGraphOutputPort.UpdateBounds(Idx: Integer; Interval: Integer);
var
  PortTop, PortLeft: Integer;
begin
  with Owner as TCGraphBlock do begin
    if Interval <= 0 then begin
      Interval := Height div FOutputComponentCount;
    end;
    if Idx < 0 then begin
      Idx := FOutputComponentCount - 1;
    end;
    PortTop := Top + Idx * Interval + Interval div 2 - DefaultPortHeight div 2;
    PortLeft := Left - DefaultPortWidth;
  end;
  //WriteLn('idx = ', idx, ' PortTop = ', PortTop, ' PortLeft = ', PortLeft);
  ChangeBounds(PortLeft, PortTop, DefaultPortWidth, DefaultPortHeight);
  if Assigned(FConnector) then with FConnector do begin
    UpdatePoints;
  end;
end;

constructor TCGraphBlock.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  FInputComponentCount := 0;
  FOutputComponentCount := 0;
  Width := DefaultBlockWidth;
  Height := DefaultBlockHeight;
  FSelected := False;
  OnMouseDown := @StartMove;
  OnMouseUp := @EndMove;
  OnMouseMove := @Move;
  OnMouseLeave := @MouseLeaved;
  FType := 'TCGraphBlock';
end;

function TCGraphBlock.GetDescription: TLFMTree;
begin
  if Load then with CodeToolBoss do begin
    GetCodeToolForSource(CodeBuffer[ctSource], true, false);
    if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], Result, False, False) then
      Result := nil;
  end;
end;

function TCGraphBlock.Load(const DesignDescription: TLFMTree; ContextNode:TLFMObjectNode): Boolean;
var
  ChildNode: TLFMTreeNode;
  PortDescription: TLFMObjectNode;
  Port: TCGraphPort;
begin
  ChildNode := ContextNode.FirstChild;
  Left := StrToInt(GetPropertyValue(ContextNode, 'Left', DesignDescription));
  Top := StrToInt(GetPropertyValue(ContextNode, 'Top', DesignDescription));
  Color := clRed;
  Caption := GetPropertyValue(ContextNode, 'Name', DesignDescription);
  Selected := True;
  PortDescription := FindObjectProperty(ChildNode, DesignDescription);
  while Assigned(PortDescription) do begin
    WriteLn('PortDescription.TypeName = ', PortDescription.TypeName);
    if PortDescription.TypeName = 'TOutputPort' then
      Port := TCGraphOutputPort.Create(Self)
    else if PortDescription.TypeName = 'TInputPort' then
      Port := TCGraphInputPort.Create(Self)
    else
      Exit(False);
    with Port do begin
      Parent := Self.Parent;
      Load(DesignDescription, PortDescription);
      OnDblClick := Self.OnDblClick;
    end;
    PortDescription := FindObjectProperty(PortDescription, DesignDescription);
  end;
  Result := True;
end;

function TCGraphBlock.Load: boolean;
var
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
begin
  codeFile[ctSource] := DesignDir + '/' + Name + '.pas';
  codeFile[ctDescription] := DesignDir + '/' + Name + '.lfm';
  for CodeType := Low(CodeType) To High(CodeType) do begin
    if Assigned(CodeBuffer[CodeType]) then
      CodeBuffer[CodeType].Reload
    else begin
      CodeBuffer[CodeType] := TCodeCache.Create.LoadFile(CodeFile[CodeType]);
    end;
  end;
  Result := true;
end;

function TCGraphBlock.Save: boolean;
  function WriteSourceTemplate: string;
  var
    f: System.Text;
  begin
    Result := DesignDir + '/' + Name + '.pas';
    if not FileExists(Result) then begin
      System.Assign(f, Result);
      ReWrite(f);
      WriteLn(f, 'unit ', Name, ';');
      WriteLn(f, 'interface');
      WriteLn(f, 'uses');
      WriteLn(f, '  Blocks;');
      WriteLn(f);
      WriteLn(f, 'type');
      WriteLn(f, '  T', Name, ' = class(TBlock)');
      WriteLn(f, '    procedure Execute; override;');
      WriteLn(f, '  end;');
      WriteLn(f);
      WriteLn(f, 'implementation');
      WriteLn(f, 'procedure T', Name, '.Execute;');
      WriteLn(f, 'begin;');
      WriteLn(f, '  {Write here your code}');
      WriteLn(f, 'end;');
      WriteLn(f);
      WriteLn(f, 'initialization');
      WriteLn(f);
      WriteLn(f, 'finalization');
      WriteLn(f);
      WriteLn(f, 'end.');
      System.Close(f);
    end;
  end;
var
  CodeType: TCodeType;
  f: System.Text;
begin
  Result := true;
  for CodeType := Low(CodeType) To High(CodeType) do
    if Assigned(CodeBuffer[CodeType]) then
      CodeBuffer[CodeType].Save;
  WriteSourceTemplate;
  System.Assign(f, DesignDir + '/' + Name + '.lfm');
  ReWrite(f);
  Save(f);
  System.Close(f);
  for CodeType := Low(CodeType) To High(CodeType) do
    if not Assigned(CodeBuffer[CodeType]) then
      Result := Load;
end;

function TCGraphBlock.Save(var f:Text): boolean;
var
  i: Integer;
begin
  WriteLn(f, 'object ', Name, ': T' + Name);
  WriteLn(f, '  Name = ''', Name, '''');
  WriteLn(f, '  Typ = ''', Typ, '''');
  WriteLn(f, '  Left = ', Left);
  WriteLn(f, '  Top = ', Top);
  WriteLn(f, '  Width = ', Width);
  WriteLn(f, '  Height = ', Height);
  for i := 0 to ComponentCount - 1 do with Components[i] as TCGraphPort do begin
    Save(f);
  end;
  WriteLn(f, 'end');
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
begin
  if(Sender = Self)and _MouseDown then begin
    X += Left;           
    Y += Top;
    dx := X - _MousePos.x;
    dy := Y - _MousePos.y;
    _MousePos.x := X;
    _MousePos.y := Y;
    ChangeBounds(Left + dx, Top + dy, Width, Height);
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
      Color := clBlack;
      Brush.Color := clGray;
      Rectangle(PaintRect);
      InflateRect(PaintRect, -2, -2);
    end;
    If not Enabled then
      Brush.Color := clBtnShadow
    else
      Brush.Color:= clRed;
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
begin
  if (PortType = TCGraphInputPort) and (FInputComponentCount > 0) then
    dy := Height div FInputComponentCount
  else if (PortType = TCGraphOutputPort) and (FOutputComponentCount > 0) then
    dy := Height div FOutputComponentCount
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

procedure TCGraphConnector.Connect(AOutputPort: TCGraphOutputPort; AInputPort: TCGraphInputPort);
begin
  OutputPort := AOutputPort;
  InputPort := AInputPort;
end;

function TCGraphConnector.Save(var f: Text): Boolean;
begin
  WriteLn(f, 'object ', Name, ': TConector');
  WriteLn(f, '  InputPort = ', InputPort.Owner.Name, '.', InputPort.Name);
  WriteLn(f, '  OutputPort = ', OutputPort.Owner.Name, '.', OutputPort.Name);
  WriteLn(f, 'end');
  Result := True;
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
begin
  SetLength(FPoints, 4);
  FPoints[0] := Point(FOutputPort.Left, FOutputPort.Top + FOutputPort.Height div 2);
  FPoints[3] := Point(FInputPort.Left + FInputPort.Width, FInputPort.Top + FInputPort.Height div 2);
  FPoints[1] := Point((FPoints[0].x + FPoints[3].x) div 2, FPoints[0].y);
  FPoints[2] := Point((FPoints[0].x + FPoints[3].x) div 2, FPoints[3].y);
  //WriteLn('OutputPort = (', FPoints[0].x, ', ', FPoints[0].y, ' ), InputPort = (', FPoints[3].x, ', ', FPoints[3].y, ' )');
  SetBounds(min(FOutputPort.Left, FInputPort.Left + FInputPort.Width), min(FOutputPort.Top + FOutputPort.Height div 2, FInputPort.Top + FInputPort.Height div 2), 1 + abs(FPoints[0].x - FPoints[3].x), 1 + abs(FPoints[0].y - FPoints[3].y));
end;

end.

