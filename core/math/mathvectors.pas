unit MathVectors;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils, MathBasics;

type
  TIVector = interface(TIMathObject)
    function Add(const x:TIVector):TIVector;
    function Sub(const x:TIVector):TIVector;
    function Mul(const x:TIVector):TIVector;
    property AsString:String;
    property components[index:Integer]:TIMathObject; default;
  end;

  TCVector = class(TIVector)
  private
    _components: Array of TIMathObject;
    function GetComponent(idx:Integer):TIMathObject;
    procedure SetComponent(idx:Integer;const component:TIMathObject);
  public
    constructor Create(dim:Integer);
    function Add(const x:TIVector):TIVector;
    function Sub(const x:TIVector):TIVector;
    function Mul(const x:TIVector):TIVector;
    property AsString:String;
    function IsIn(S:TIMathSet):Boolean;
    property components[index:Integer]:TIMathObject read GetComponent write SetComponent; default;
    destructor Destroy;
  end;

implementation

constructor TCVector.Create(dim:Integer);
begin
  SetLength(_components, dim);
end;

function TCVector.Add(const x:TIVector):TIVector;
var
  i: Integer;
begin
end;

function TCVector.Sub(const x:TIVector):TIVector;
begin
end;

function TCVector.Mul(const x:TIVector):TIVector;
begin
end;

function TCVector.GetComponent(idx: Integer):TIMathObject;
begin
  Result := _components[idx];
end;

procedure TCVector.SetComponent(idx: Integer; const component:TIMathObject);
begin
  _components[idx] := component;
end;

function TCVector.IsIn(S: TIMathSet):Boolean;
begin
  Result := S.Contains(TIVector(Self));
end;

destructor TCVector.Destroy;
begin
  SetLength(_components, 0);
end;

end.

