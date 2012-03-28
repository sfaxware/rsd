unit MathVectors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MathBasics;

type
  TIVector = interface(TIMathObject)
    property components[index:Integer]:TIMathObject; default;
  end;

  TCVector = class(TIVector)
  private
    _components: Array of TIMathObject;
    function GetComponent(idx:Integer):TIMathObject;
    procedure SetComponent(idx:Integer;const component:TIMathObject);
  public
    function IsIn(S:TIMathSet):Boolean;
    property components[index:Integer]:TIMathObject read GetComponent write SetComponent; default;
  end;

implementation

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

end.

