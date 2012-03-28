unit routing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSegment = array[1..2]of TPoint;
  TRoute = array of TPoint;

function Intersect(const S1, S2: TSegment): Boolean;
function Intersect(const S1: TSegment; S: array of TSegment): Boolean;
function Intersect(const S1: TSegment; S: TRoute): Boolean;
function Segment(const P1, P2: TPoint): TSegment;

implementation

function Intersect(const S1, S2: TSegment): Boolean;
var
  dx1, dx2, dy1, dy2, det, dx, dy: Integer;
  a1, a2: Real;
begin
  dx1 := S1[2].x - S1[1].x;
  dy1 := S1[2].y - S1[1].y;
  dx2 := S2[2].x - S2[1].x;
  dy2 := S2[2].y - S2[1].y;
  det := dx1 * dy2 - dy1 * dx2;
  if det = 0 then
    Exit(False);
  dx := S2[1].x - S1[1].x;
  dy := S2[1].y - S1[1].y;
  a1 := (dx * dy2 - dy * dx2) / det;
  a2 := (dx * dy1 - dy * dx1) / det;
  result := (0 <= a1) and (a1 <= 1) and (0 <= a2) and (a2 <= 1);
end;

function Intersect(const S1: TSEgment; S: array of TSegment): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(S) to High(S) do begin
    if Intersect(S1, S[i]) then
      Exit(True);
  end;
end;

function Intersect(const S1: TSEgment; S: TRoute): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(S) to High(S) - 1 do begin
    if Intersect(S1, Segment(S[i], S[i + 1])) then
      Exit(True);
  end;
end;

function Segment(const P1, P2: TPoint): TSegment;
begin
  Result[1] := P1;
  Result[2] := P2;
end;

end.

