unit RoutingTestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  TTestCase1 = class(TTestCase)
  published
    procedure TestAddRectDisjointArea;
    procedure TestRemoveRectDisjointArea;
    procedure TestAdjacenceGraph;
    procedure TestHookUp4;
  end;

implementation

uses
  Types, Routing;

const
  DisjointArea: array[1..3] of TRect = (
    (Left: 1; Top: 1; Right: 5; Bottom: 5),
    (Left: 6; Top: 6; Right: 50; Bottom: 50),
    (Left: 100; Top: 100; Right: 500; Bottom: 500)
  );
  JointArea: array[1..4] of TRect = (
    (Left: 1; Top: 1; Right: 5; Bottom: 5),
    (Left: 6; Top: 6; Right: 50; Bottom: 50),
    (Left: 100; Top: 100; Right: 500; Bottom: 500),
    (Left: 5; Top: 5; Right: 7; Bottom: 50)
  );
  {JointAreaAdjacenceGraph: array[1..3] of TIndex = (
    (),
    (),
    ()
  );}

var
  B: TArea;

function BuildArea(const A: array of TRect): TArea;
var
  i: TIndex;
  o: Integer;
begin
  SetLength(Result, Length(A));
  o := Low(A) - Low(Result);
  for i := Low(Result) to High(Result) do begin
    Result[i] := A[i + o];
  end;
end;

procedure TTestCase1.TestAddRectDisjointArea;
var
  i: TIndex;
begin
  AssertNull('Area not empty ' + hexStr(Pointer(B)) + 'h', Pointer(B));
  for i := Low(DisjointArea) to High(DisjointArea) do begin
    AddRect(B, DisjointArea[i]);
    AssertEquals(i, Length(B));
  end;
end; 

procedure TTestCase1.TestRemoveRectDisjointArea;
var
  R: TRect;
  i: TIndex;
begin
  for i := Low(DisjointArea) to High(DisjointArea) do begin
    RemoveRect(B, DisjointArea[i]);
    AssertEquals('Remove [' + IntToStr(i) + ']', Length(DisjointArea) - i, Length(B));
  end;
  AssertNull('Area not empty ' + hexStr(Pointer(B)) + 'h', Pointer(B));
end;

procedure TTestCase1.TestAdjacenceGraph;
var
  G: TAdjacenceGraph;
  A: TArea;
  i: TIndex;
begin
  A := BuildArea(DisjointArea);
  G := AdjacenceGraph(A);
  AssertEquals('Invalid graph length', Length(DisjointArea), Length(G));
  for i := Low(G) to High(G) do begin
    AssertEquals('Invalid adjacent list for [' + IntToStr(i) + '] length', 0, Length(G[i]));
  end;
end;

procedure TTestCase1.TestHookUp4;
begin
end;

initialization

  RegisterTest(TTestCase1); 
end.

