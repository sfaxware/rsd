unit Magnifier;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TMagnifier = class(TGraphicControl)
  private
    FBounds: TRect;
    FMagnification: Integer;
    function GetMagnification: Real;
    function GetOriginalBounds: TRect;
    procedure SetOriginalBounds(B: TRect);
    procedure DoMagnify;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Magnify(m:Real); virtual;
    property OriginalBounds: TRect read GetOriginalBounds write SetOriginalBounds;
    property Magnification: Real read GetMagnification;
  end;

implementation

constructor TMagnifier.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMagnification := 256; // 1.0
  if AOwner is TControl then with AOwner as TControl do begin
    Self.OnMouseWheel := OnMouseWheel;
  end;
end;

procedure TMagnifier.DoMagnify;
var
  R: TRect;
begin
  R := FBounds;
  with R do begin
    //WriteLn('Old bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
    //WriteLn('FMagnification = ', FMagnification, ' <=> ', FMagnification / 256);
    Left :=  FMagnification * Left div 256;
    Top :=  FMagnification * Top shr 8;
    Right :=  FMagnification * Right shr 8;
    Bottom :=  FMagnification * Bottom shr 8;
    //WriteLn('New bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  end;
  BoundsRect := R;
end;

function TMagnifier.GetMagnification: Real;
begin
  Result := FMagnification / 256;
end;

function TMagnifier.GetOriginalBounds: TRect;
begin
  Result := FBounds;
end;

procedure TMagnifier.SetOriginalBounds(B: TRect);
begin
  //with B do begin
    //WriteLn('Changing bounds (', Name, ') = ((', Left, ', ', Top, '), (', Right, ', ', Bottom, '))');
  //end;
  FBounds := B;
  Magnify(Magnification);
end;

procedure TMagnifier.Magnify(m:Real);
var
  i: Integer;
  Component: TComponent;
begin
  FMagnification := Round(m * 256);
  DoMagnify;
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TMagnifier then with Component as TMagnifier do begin
      Magnify(m);
    end;
  end;
end;

end.

