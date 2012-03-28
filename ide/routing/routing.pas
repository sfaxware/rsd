unit Routing;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSegment = array[1..2]of TPoint;
  TSegments = array of TSegment;
  TRoute = array of TPoint;
  TRoutes = array of TRoute;
  TArea = array of TRect;
  TIndex = Integer;
  TIndexes = array of TIndex;
  TAdjacenceGraph = array of TIndexes;

function Bounds(R: TRoute): TRect;
function Intersect(const S1, S2: TSegment): Boolean;
function Intersect(const S1: TSegment; S: TSegments): Boolean;
function Intersect(const S1: TSegment; S: TRoute): Boolean;
function Segment(const P1, P2: TPoint): TSegment;
function RectCenter(Rect: TRect): TPoint;
function Route(const P1, P2: TPoint; const Area: TArea; out ARoute: TRoute): Boolean;
procedure InsertRoute(const R: TRoute; var Routes: TRoutes);
procedure RemoveRoute(const R: TRoute; var Routes: Troutes);
procedure AddRect(var A: TArea; const R: TRect);
procedure RemoveRect(var A: TArea; n: TIndex);
procedure RemoveRect(var A: TArea; const R: TRect);
procedure RemoveRect(var A: TArea; n: TIndex; const R: TRect);
function RemoveRect(const B, R: TRect):TArea;
function Intersection(out I: TArea; const A: TArea; const P: TPoint): TIndexes;
function Intersection(out I: TArea; const A: TArea; const R: TRect): TIndexes;
function Containers(const A: TArea; P: TPoint): TIndexes;
function Adjacents(const A: TArea; n: TIndex): TIndexes;
function AdjacenceGraph(A: TArea): TAdjacenceGraph;

implementation

uses
  Types;

function Bounds(R: TRoute): TRect;
var
  i: Integer;
begin
  with Result do begin
    Top := MaxInt;
    Left := MaxInt;
    Bottom := 0;
    Right := 0;
    for i := Low(R) to High(R) do with R[i] do begin
      if Left > x then
        Left := x;
      if Right < x then
        Right := x;
      if Top > y then
        Top := y;
      if Bottom < y then
        Bottom := y;
    end;
    Right += 1;
    Bottom += 1;
  end;
end;

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

function Intersect(const S1: TSegment; S: TSegments): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(S) to High(S) do begin
    if Intersect(S1, S[i]) then
      Exit(True);
  end;
end;

function Intersect(const S1: TSegment; S: TRoute): Boolean;
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

function RectCenter(Rect: TRect): TPoint;
begin
  with Rect do begin
    Result.x := (Left + Right) div 2;
    Result.y := (Top + Bottom) div 2;
  end;
end;

function Route(const P1, P2: TPoint; const Area: TArea; out ARoute: TRoute): Boolean;
begin
  SetLength(ARoute, 4);
  ARoute[0] := P1;
  ARoute[3] := P2;
  ARoute[1] := Point((ARoute[0].x + ARoute[3].x) div 2, ARoute[0].y);
  ARoute[2] := Point((ARoute[0].x + ARoute[3].x) div 2, ARoute[3].y);
  Result := True;
end;

procedure InsertRoute(const R: TRoute; var Routes: TRoutes);
begin
  SetLength(Routes, Length(Routes) + 1);
  Routes[Length(Routes) - 1] := R;
end;

procedure RemoveRoute(const R: TRoute; var Routes: TRoutes);
begin

end;

function Intersection(out I: TArea; const A: TArea; const P: TPoint): TIndexes;
var
  n: Integer;
  l: Integer;
begin
  l := 0;
  SetLength(Result, l);
  SetLength(I, 0);
  for n := Low(A) to High(A) do begin
    if PtInRect(A[n], P) then begin
      AddRect(I, A[n]);
      SetLength(Result, l + 1);
      Result[l] := n;
      l += 1;
    end;
  end;
end;

function Intersection(out I: TArea; const A: TArea; const R: TRect): TIndexes;
var
  n: Integer;
  l: Integer;
  X: TRect;
begin
  l := 0;
  SetLength(Result, l);
  SetLength(I, 0);
  for n := Low(A) to High(A) do begin
    if IntersectRect(X, A[n], R) then begin
      AddRect(I, X);
      SetLength(Result, l + 1);
      Result[l] := n;
      l += 1;
    end;
  end;
end;

procedure AddRect(var A: TArea; const R: TRect);
var
  l: Integer;
begin
  if not IsRectEmpty(R) then begin
    l := Length(A);
    SetLength(A, l + 1);
    A[l] := R;
  end;
end;

procedure RemoveRect(var A: TArea; n: TIndex);
var
  p: TIndex;
begin
  if(n >= Low(A)) and (n <= High(A)) then begin
    for p := n to High(A) - 1 do begin
      A[p] := A[p + 1];
    end;
    SetLength(A, Length(A) - 1);
  end;
end;

procedure RemoveRect(var A: TArea; const R: TRect);
var
  n: Integer;
  I: TArea;
  X: TIndexes;
begin
  X := Intersection(I, A, R);
  for n := Low(X) to High(X) do begin
    RemoveRect(A, X[n], R);
  end;
end;

procedure RemoveRect(var A: TArea; n: TIndex; const R: TRect);
var
  B, S: TRect;
  RectQty: Integer;
begin
  B := A[n];
  if IntersectRect(S, R, B) then begin
    if EqualRect(S, B) then begin
      RemoveRect(A, n);
    end else begin
      AddRect(A, Rect(B.Left, B.Top, B.Right, S.Top));
      AddRect(A, Rect(S.Left, S.Top, B.Right, B.Bottom));
      AddRect(A, Rect(B.Left, S.Top, S.Left, S.Bottom));
      AddRect(A, Rect(B.Left, S.Bottom, B.Right, B.Bottom));
    end;
  end;
end;

function RemoveRect(const B, R: TRect): TArea;
begin
  SetLength(Result, 1);
  Result[0] := B;
  RemoveRect(Result, 0, R);
end;

function Containers(const A: TArea; P: TPoint): TIndexes;
var
  n: Integer;
  l: Integer;
begin
  l := 0;
  SetLength(Result, l);
  for n := Low(A) to High(A) do begin
    if PtInRect(A[n], P) then begin
      SetLength(Result, l + 1);
      Result[l] := n;
      l += 1;
    end;
  end;
end;

function Adjacents(const A: TArea; n: TIndex): TIndexes;
var
  p: Integer;
  l: Integer;
  X: TRect;
begin
  l := 0;
  SetLength(Result, l);
  for p := Low(A) to High(A) do begin
    if IntersectRect(X, A[p], A[n]) then begin
      SetLength(Result, l + 1);
      Result[l] := p;
      l += 1;
    end;
  end;
end;

function AdjacenceGraph(A: TArea): TAdjacenceGraph;
var
  n: TIndex;
begin
  SetLength(Result, Length(A));
  for n := Low(A) to High(A) do begin
    Result[n] := Adjacents(A, n);
  end;
end;

end.
