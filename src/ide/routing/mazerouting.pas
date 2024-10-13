unit MazeRouting;

interface

type
  TMapIndex = Word;

const
  MapWidth = 16;
  MapHeight = 16;
  MaxRouteLength = 2 * (MapWidth + MapHeight);
  MaxRoutesQty = 9;
  vBlock = High(TMapIndex);
  vRouteMax = vBlock - 1;
  vRouteMin = vBlock - MaxRoutesQty;
  vEmpty = MaxRouteLength + 1;
{$IF vEmpty >= vRouteMin}
  {$ERROR Map index type does not allow to support defined map size!}
{$ENDIF vEmpty >= vRouteMin}

type
  TPathPoint = record
    L: TMapIndex;
    pX, pY: TMapIndex;
  end;

  PMap = ^TMap;
  TMap = array[1..MapHeight, 1..MapWidth] of TMapIndex;
  PPath = ^TPath;
  TPath = array[1..MapHeight, 1..MapWidth] of TPathPoint;

  TBorderPoint = array[1..2] of TMapIndex;
  PBorder = ^TBorder;
  TBorder = array[0..MapHeight + MapWidth, 1..2] of TMapIndex;

procedure Block(var Map: TMap; xA, yA, xB, yB: Integer);
procedure Clear(out Map: TMap);
procedure Print(Map: TMap);
procedure Route(var Map: TMap; xB, yB, xE, yE: Integer; C: Integer);

implementation
{$RangeChecks ON}

function BorderPoint(x, y: TMapIndex): TBorderPoint;
begin
  Result[1] := x;
  Result[2] := y;
end;

procedure Clear(out Map: TMap);
var
  n, m: Integer;
begin
  for n := Low(Map) to High(Map) do begin
    for m := Low(Map[n]) to High(Map[n]) do begin
      Map[n, m] := vEmpty;
    end;
  end;
end;

function PathOf(Map: TMap): PPath;
var
  n, m: Integer;
begin
  New(Result);
  for n := Low(Map) to High(Map) do begin
    for m := Low(Map[n]) to High(Map[n]) do with Result^[n, m] do begin
      pX := 0;
      pY := 0;
      L := Map[n, m];
    end;
  end;
end;

function MapOf(Path: TPath): TMap;
var
  n, m: Integer;
begin
  for n := Low(Path) to High(Path) do begin
    for m := Low(Path[n]) to High(Path[n]) do with Path[n, m] do begin
      Result[n, m] := L;
    end;
  end;
end;

procedure Print(Map: TMap);
  procedure PrintLine;
  var
    m: Integer;
  begin
    WriteLn;
    Write('    ');
    for m := Low(Map[Low(Map)]) to High(Map[Low(Map)]) do begin
      Write('+---');
    end;
    WriteLn('+');
  end;
var
  n, m: Integer;
begin
  Write('y \ x');
  for m := Low(Map[Low(Map)]) to High(Map[Low(Map)]) do begin
    Write(m:3, ' ');
  end;
  PrintLine;
  for n := Low(Map) to High(Map) do begin
    Write(n:3, ' |');
    for m := Low(Map[n]) to High(Map[n]) do begin
      case Map[n, m] of
        vBlock: Write('XXX|');
        vRouteMin .. vRouteMax: Write(' ', Map[n, m] - vRouteMin + 1, ' |');
        vEmpty: Write('   |');
      else
        Write(Map[n, m]:3, '|');
      end;
    end;
    PrintLine;
  end;
  WriteLn;
end;

procedure Block(var Map: TMap; xA, yA, xB, yB: Integer);
var
  n, m: Integer;
begin
  for n := yA to yB do begin
    for m := xA to xB do begin
      Map[n, m] := vBlock;
    end;
  end;
end;

procedure SwapBuffers(var b1, b2: Pointer);
var
  tmp: Pointer;
begin
  tmp := b1;
  b1 := b2;
  b2 := tmp;
end;

procedure Route(var Map: TMap; xB, yB, xE, yE: Integer; C: Integer);
  var
    count: Integer;

  function WeightNeighbor(var Path: TPath; x, y, m, n: Integer): Boolean;
  begin
    count += 1;
    Result := (y + n >= Low(Path)) and (y + n <= High(Path)) and
              (x + m >= Low(Path[y + m])) and (x + m <= High(Path[y + m]));
    if Result then with Path[y + n, x + m] do begin
      Result := (L > Path[y, x].L + 1) and (L <= vEmpty);
      if Result then begin
        L := Path[y, x].L + Abs(n) + Abs(m);
        pX := x;
        pY := y;
      end;
    end;
  end;

  procedure WeightPathNeighbors(out NewBorder: TBorder; var Path: TPath; const Border: TBorder);
  var
    k, L, n, m: Integer;
    x, y: Integer;
  begin
    L := 0;
    Assert(High(Border) >= Border[0, 1]);
    for k := 1 to Border[0, 1] do begin
      x := Border[k, 1];
      y := Border[k, 2];
      for n := -1 to 1 do begin
        for m := -1 to 1 do begin
          if ((n = 0) xor (m = 0)) and WeightNeighbor(Path, x, y, m, n) then begin
            L += 1;
            NewBorder[L, 1] := x + m;
            NewBorder[L, 2] := y + n;
          end;
        end;
      end;
    end;
    NewBorder[0, 1] := L;
    NewBorder[0, 2] := Border[0, 2] + 1;
  end;

  procedure WeightNeighbors(var Path: TPath; xB, yB: Integer);
  var
   Border, NewBorder: PBorder;
  begin
    New(Border);
    Border^[0] := BorderPoint(1, 0);
    Border^[1] := BorderPoint(xB, yB);
    New(NewBorder);
    repeat
      WeightPathNeighbors(NewBorder^, Path, Border^);
      SwapBuffers(Border, NewBorder);
    until Border^[0, 1] = 0;
    Dispose(NewBorder);
    Dispose(Border);
  end;

  procedure UpdateMap(var Map: TMap; const Path: TPath; x, y: Integer; C: Integer);
  var
    n, m: Integer;
  begin
    C += vRouteMin - 1;
    m := x;
    n := y;
    repeat
      with Path[n, m] do begin
        Map[n, m] := C;
        m := pX;
        n := pY;
      end;
    until Path[n, m].L = 0;
    Map[n, m] := C;
  end;

var
  Path: PPath;
begin
  Map[yB, xB] := 0;
  Path := PathOf(Map);
  count := 0;
  WeightNeighbors(Path^, xB, yB);
  //Print(MapOf(Path^));
  UpdateMap(Map, Path^, xE, yE, C);
  Dispose(Path);
  WriteLn('count = ', count);
end;

end.
