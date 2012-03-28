unit RoutingTestCase1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  TTestCase1 = class(TTestCase)
  published
    procedure TestAddRect;
    procedure TestRemoveRect;
    procedure TestHookUp3;
    procedure TestHookUp4;
  end;

implementation

uses
  Types, Routing;

const
  PtQty = 3;
  A: array[1..PtQty] of TRect = (
    (Left: 1; Top: 1; Right: 5; Bottom: 5),
    (Left: 6; Top: 6; Right: 50; Bottom: 50),
    (Left: 100; Top: 100; Right: 500; Bottom: 500)
  );

var
  B: TArea;

procedure TTestCase1.TestAddRect;
var
  i: TIndex;
begin
  AssertNull('Area not empty ' + hexStr(Pointer(B)) + 'h', Pointer(B));
  for i := Low(A) to High(A) do begin
    AddRect(B, A[i]);
    AssertEquals(i, Length(B));
  end;
end; 

procedure TTestCase1.TestRemoveRect;
var
  R: TRect;
  i: TIndex;
begin
  for i := Low(A) to High(A) do begin
    RemoveRect(B, A[i]);
    AssertEquals('Remove [' + IntToStr(i) + ']', Length(A) - i, Length(B));
  end;
  AssertNull('Area not empty ' + hexStr(Pointer(B)) + 'h', Pointer(B));
end;

procedure TTestCase1.TestHookUp3;
begin
  Fail('Write your own test');
end;

procedure TTestCase1.TestHookUp4;
begin
  Fail('Write your own test');
end;

initialization

  RegisterTest(TTestCase1); 
end.

