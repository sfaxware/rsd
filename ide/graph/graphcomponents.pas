unit GraphComponents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Types;
  
type
  TCGraphBlock = class(TGraphicControl)
  public
    constructor Create(AOwner:TComponent);override;
  protected
    procedure Paint; override;
  published
//    property Action;
//    property Align;
//    property Anchors;
    property BorderSpacing;
    property Constraints;
    property Caption;
    property Color;
    property Enabled;
    property Font;
//    property Glyph;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnPaint;
    property OnResize;
    property OnChangeBounds;
    property ShowHint;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
  end;

implementation

uses buttons;

constructor TCGraphBlock.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
  Left := Random(256);
  Top := Random(256);
  Color := clRed;
  Caption:= 'Hello';
end;

procedure TCGraphBlock.Paint;
var
  PaintRect: TRect;
  GlyphWidth, GlyphHeight: Integer;
  Offset, OffsetCap: TPoint;
  ClientSize, TotalSize, TextSize, GlyphSize: TSize;
  TXTStyle : TTextStyle;
  SIndex : Longint;
  myparent:TCustomControl;
begin
  WriteLn('TCGraphBlock.Paint ',Name,':',ClassName,' Parent.Name=',Parent.Name);
  PaintRect:=ClientRect;
  ClientSize.cx:= PaintRect.Right - PaintRect.Left;
  ClientSize.cy:= PaintRect.Bottom - PaintRect.Top;
  TextSize.CY := PaintRect.Bottom - PaintRect.Top;
  TextSize.CX := PaintRect.Right - PaintRect.Left;
  Offset.X:= (ClientSize.cx - TextSize.cx) div 2;
  Offset.Y:= (ClientSize.cy - TextSize.cy) div 2;
  OffsetCap.X:= (ClientSize.cx - TextSize.cx) div 2;
  OffsetCap.Y:= (ClientSize.cy - TextSize.cy) div 2;

  if Caption <> '' then
  begin
    TXTStyle := Canvas.TextStyle;
    TXTStyle.Opaque := False;
    TXTStyle.Clipping := True;
//    TXTStyle.ShowPrefix := ShowAccelChar;
    TXTStyle.Alignment := taLeftJustify;
    TXTStyle.Layout := tlTop;
    // set color here, otherwise SystemFont is wrong if the button was disabled before
    Canvas.Font.Color := Font.Color;
    TXTStyle.SystemFont := Canvas.Font.IsDefault;//Match System Default Style

    With PaintRect, OffsetCap do begin
      Left := Left + X;
      Top := Top + Y;
    end;

    with Canvas do begin
      If not Enabled then begin
      Font.Color := clBtnHighlight;
      OffsetRect(PaintRect, 1, 1);
      TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
      Font.Color := clBtnShadow;
      OffsetRect(PaintRect, -1, -1);
      end;
      WriteLn('TCGraphBlock.Paint PaintRect=',PaintRect.Left,', ',PaintRect.TOp,', ',PaintRect.Right,', ',PaintRect.Bottom,', ',caption,', ', TXTStyle.SystemFont);
      Brush.Color:= Color;
      FillRect(PaintRect);
      TextRect(PaintRect, PaintRect.Left, PaintRect.Top, Caption, TXTStyle);
    end;
  end;
  inherited Paint;
end;

end.

