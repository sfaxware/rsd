unit TestCase1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry; 

type

  MathTest= class(TTestCase)
  published
    procedure TestHookUp; 
  end;

implementation

procedure MathTest.TestHookUp; 
begin
  Fail('Write your own test'); 
end; 

initialization

  RegisterTest(MathTest); 
end.

