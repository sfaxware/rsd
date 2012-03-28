unit GraphComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Types, CodeCache;
  
type
  TCodeType = (ctSource, ctDescription);
  TCGraphBlock = class(TGraphicControl)
  private
    _MouseDown: Boolean;
    _MousePos: TPoint;
    procedure StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
    procedure EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
    procedure Move(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure MouseLeaved(Sender: TObject);
  public
    constructor Create(AOwner:TComponent);override;
    CodeBuffer: array[TCodeType] of TCodeBuffer;
    function Load: boolean;
    function Save: boolean;
  protected
    FSelected: Boolean;
    FType: string;
    procedure SetSeleted(AValue: Boolean);
    procedure Paint; override;
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
  end;

implementation

constructor TCGraphBlock.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  FSelected := False;
  OnMouseDown := @StartMove;
  OnMouseUp := @EndMove;
  OnMouseMove := @Move;
  OnMouseLeave := @MouseLeaved;
  FType := 'TCGraphBlock';
end;

function TCGraphBlock.Load: boolean;
var
  CodeFile: array[TCodeType] of string;
  CodeType: TCodeType;
begin
  codeFile[ctSource] := '/tmp/' + Name + '.pas';
  codeFile[ctDescription] := '/tmp/' + Name + '.lfm';
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
    Result := '/tmp/' + Name + '.pas';
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
  function WriteDescriptionTemplate: string;
  var
    f: System.Text;
  begin
    Result := '/tmp/' + Name + '.lfm';
    System.Assign(f, Result);
    ReWrite(f);
    WriteLn(f, 'object ', Name, ': T' + Name);
    WriteLn(f, '  Name = ''', Name, '''');
    WriteLn(f, '  Typ = ''', Typ, '''');
    WriteLn(f, '  Left = ', Left);
    WriteLn(f, '  Top = ', Top);
    WriteLn(f, '  Width = ', Width);
    WriteLn(f, '  Height = ', Height);
    WriteLn(f, 'end');
    System.Close(f);
  end;
var
  CodeType: TCodeType;
begin
  for CodeType := Low(CodeType) To High(CodeType) do
    if Assigned(CodeBuffer[CodeType]) then
      CodeBuffer[CodeType].Save;
  WriteSourceTemplate;
  WriteDescriptionTemplate;
  Result := true;
end;

procedure TCGraphBlock.StartMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
  if Sender = Self then
    case Button of
    mbLeft:begin
      _MouseDown := True;
      _MousePos.x := X + Left;
      _MousePos.y := Y + Top;
    end;
  end;
end;

procedure TCGraphBlock.EndMove(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer ) ;
begin
  _MouseDown := False;
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
  with Canvas do begin
    //WriteLn('TCGraphBlock.Paint PaintRect=',PaintRect.Left,', ',PaintRect.TOp,', ',PaintRect.Right,', ',PaintRect.Bottom,', ',caption,', ', TXTStyle.SystemFont);
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

end.

