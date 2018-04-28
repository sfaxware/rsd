//*****************************************************************************
// File                   : reals.pas
// Author                 : Mazen NEIFER
// Creation date          : 2010-07-18
// Last modification date : 2010-07-18
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
unit Reals;
{$MODE ObjFpc}
interface
type
  TReal = Real;{This can be changed to any real type to support more huge values}
  PReal =^ TReal;

const
  Digit:Byte=3;{I prefer 3 zeros but you can change it}

function Module(r: TReal): TReal; overload;
function Sqr(r: TReal): Treal; overload;
procedure Read(out r: TReal); overload;
procedure Write(r: TReal); overload;

implementation

function Module(r: TReal): TReal;
begin
  Result := Abs(r);
end;

function Sqr(r: TReal): Treal; overload;
begin
  Result := System.Sqr(r);
end;

procedure Read(out r: TReal);
begin
  System.Read(r);
end;

procedure Write(r : TReal);
begin
  System.Write(r:1:Digit);
end;

end.
