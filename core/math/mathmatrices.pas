unit MathMatrices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MathBasics;

type
  TIMatrix = interface(TIMathObject)
    function IsIn(S: TIMathSet):Boolean;
    property components[index:Array of Integer]:TIMathObject;default;
  end;

  TCMatrix = class(TIMatrix)
  private
    _components: Array of Array of TIMathObject;
    function GetComponent(idx: Array of Integer):TIMathObject;
    procedure SetComponent(idx: Array of Integer; const component:TIMathObject);
  public
    function IsIn(S: TIMathSet):Boolean;
    property components[index:Array of Integer]:TIMathObject read GetComponent write SetComponent; default;
  end;

implementation

function TCMatrix.GetComponent(idx: Array of Integer):TIMathObject;
begin
  Result := _components[idx[0], idx[1]];
end;

procedure TCMatrix.SetComponent(idx: Array of Integer; const component:TIMathObject);
begin
  _components[idx[0], idx[1]] := component;
end;

function TCMatrix.IsIn(S: TIMathSet):Boolean;
begin
  Result := S.Contains(TIMatrix(Self));
end;

end.

