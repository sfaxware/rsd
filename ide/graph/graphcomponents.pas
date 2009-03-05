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
    CodeCache: array[TCodeType] of TCodeCache;
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
    property Color;
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
var
  CodeType: TCodeType;
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
  for CodeType := Low(TCodeType) To High(TCodeType) do
    CodeCache[CodeType] := TCodeCache.Create;
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
      Rectangle(PaintRect);
      InflateRect(PaintRect, -2, -2);
    end;
    If not Enabled then
      Color := clBtnShadow
    else
      Color:= Self.Color;
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

