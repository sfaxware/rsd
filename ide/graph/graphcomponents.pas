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

constructor TCGraphBlock.Create(AOwner:TComponent);
begin
  inherited Create(AOwner);
  Width := 100;
  Height := 100;
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

