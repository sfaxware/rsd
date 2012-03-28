unit MathBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils; 

type
  TIMathSet = interface;

  TIMathObject = interface
    function IsIn(S:TIMathSet):Boolean;
  end;

  TIMathSet = interface(TIMathObject)
    function Contains(x:TIMathObject):Boolean;
    function IsSubSet(S:TIMathSet):Boolean;
    function Complement(S:TIMathSet):TIMathSet;
    function Cross(S:TIMathSet):TIMathSet;
    function Intersection(S:TIMathSet):TIMathSet;
    function Union(S:TIMathSet):TIMathSet;
  end;

  TMathRelation = function(const x,y:TIMathObject):Boolean;

//operator in(const x:TIMathObject;const S:TIMathSet):Boolean;

operator +(const S:TIMathSet;const E:TIMathSet):TIMathSet;
operator -(const S:TIMathSet;const E:TIMathSet):TIMathSet;
operator *(const S:TIMathSet;const E:TIMathSet):TIMathSet;
operator **(const S:TIMathSet;const E:TIMathSet):TIMathSet;

implementation

//operator in(const x:TIMathObject;const S:TIMathSet):Boolean;
//begin
//  result := x.IsIn(S);
//end;

operator +(const S:TIMathSet;const E:TIMathSet):TIMathSet;
begin
  result := S.Union(E);
end;

operator -(const S:TIMathSet;const E:TIMathSet):TIMathSet;
begin
  result := S.Complement(E);
end;

operator *(const S:TIMathSet;const E:TIMathSet):TIMathSet;
begin
  result := S.Intersection(E);
end;

operator **(const S:TIMathSet;const E:TIMathSet):TIMathSet;
begin
  result := S.Cross(E);
end;

end.

