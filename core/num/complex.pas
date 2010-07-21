//*****************************************************************************
// File                   : complex.pas
// Author                 : Mazen NEIFER
// Creation date          : 2000-09-21
// Last modification date : 2010-07-18
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
unit Complex;
{$MODE ObjFpc}
interface

uses
  Reals;

type
  TReal = Real;{This can be changed to any real type to support more huge values}
  PReal = ^TReal;
  PComplex = ^TComplex;
  TComplex = record
    x, y: Real;{It's too simple a complex is a couple of reals!}
  end;

const
  i: TComplex = (x: 0; y: 1);{And what about the solution of x^2+1=0!!!!!?}

operator :=(r: Real):TComplex;
operator -(z: TComplex):TComplex;
operator not(z: TComplex): TComplex;
operator +(z1, z2: TComplex): TComplex;
operator -(z1, z2: TComplex): TComplex;
operator *(z1, z2: TComplex): TComplex;
operator /(z1, z2: TComplex): TComplex;
operator and(z1, z2: TComplex): TComplex;
operator **(z: TComplex; n: Byte): TComplex;
operator +(r: Real; z: TComplex): TComplex;
operator *(r: Real; z: TComplex): TComplex;
operator /(z: TComplex; r: Real): TComplex;
operator =(z: TComplex; r: Real): Boolean;
function exp(z: TComplex): TComplex; overload;
function Module(z: TComplex): TReal; overload;
function Sqr(z: TComplex): TReal; overload;
procedure Read(out z: TComplex); overload;
procedure Write(z: TComplex); overload;
procedure FFT(var C{:ARRAY[1..n]OF TComplex}; n: Byte);
{This function returns the FFT of a signal. The signal is supposed to be stored 
in an array wich size is 2^n. It stores the result at the same array that
contains the input signal. So you have to keep the original signal some where
if you need it. 
Suppose you have a signal defined by regular spaced points. Those points define
an array of complex values lets call it Signal=ARRAY[1..size]OF TCcomplex. When
you applay FFT(Signal,n) (of course size=2^n to get some thing correct), you
get in Signal no more the signal itself but its FFT values. Of course to
restore the signal you have to applay IFFT(Signal,n) without modifing any value in the array Signal.}

procedure IFFT(var C{:ARRAY[1..n]OF TComplex}; n: Byte);
{This function is too similar to the FFT one. It calculates the FFT-1 of a
signal. It is used like the FFT function.}

implementation

operator :=(r: Real): TComplex;
begin
  with Result do begin
    x:=r;
    y:=0;
  end;
end;

operator -(z: TComplex): TComplex;
begin
  with Result do begin
    x:=-z.x;
    y:=-z.y;
  end;
end;

operator not(z: TComplex): TComplex;
begin
  with Result do begin
    x := z.x;
    y := -z.y;
  end;
end;

operator +(z1 ,z2: TComplex): TComplex;
begin
  with Result do begin
    x := z1.x + z2.x;
    y := z1.y + z2.y;
  end;
end;

operator -(z1 ,z2: TComplex): TComplex;
begin
  with Result do begin
    x:=z1.x-z2.x;
    y:=z1.y-z2.y;
  end;
end;

operator *(z1, z2: TComplex): TComplex;
begin
  with Result do begin
    x := z1.x * z2.x - z1.y * z2.y;
    y := z1.x * z2.y + z1.y * z2.x;
 end;
end;

operator /(z1, z2: TComplex): TComplex;
var
  M:Real;
begin
  M := Module(z2);
  with Result do begin
    x:=(z1.x*z2.x+z1.y*z2.y)/M;
    y:=(z1.y*z2.x-z1.x*z2.y)/M;
  end;
end;

operator and(z1, z2: TComplex): TComplex;
begin
  with result do begin
     x := z1.x * z2.x + z1.y * z2.y;
     y := z1.y * z2.x - z1.x * z2.y;
  end;
end;

operator **(z:TComplex;n:Byte) :TComplex;
var
    i: Byte;
begin
  with Result do begin
    x := 1;
    y := 0;
    for i := 1 to n do begin
      x := x * z.x - y * z.y;
      y := x * z.y + y * z.x;
    end;
  end;
end;

operator /(z: TComplex; n: Byte): TComplex;
begin
  with Result do begin
    x := z.x / n;
    y := z.y / n;
  end;
end;

operator /(z: TComplex; n: DWord): TComplex;
begin
  with Result do begin
    x := z.x / n;
    y := z.y / n;
  end;
end;

operator +(r: Real; z: TComplex): TComplex;
begin
  with Result do begin
    x := z.x + r;
    y := z.y;
  end;
end;

operator *(r: Real; z: TComplex): TComplex;
begin
  with Result do begin
    x := r * z.x;
    y := r * z.y;
  end;
end;

operator /(z: TComplex;r: Real): TComplex;
begin
  with Result do begin
    x := z.x / r;
    y := z.y / r;
  end;
end;

operator =(z: TComplex; r: Real): Boolean;
begin
  with z do
    Result := (x = r) AND (y = 0);
end;

function exp(z: TComplex): TComplex;
const
  MaxLevel = 20;
var
  k: Byte;
begin
  Result := 1;
  for k := MaxLevel downto 1 do
    Result := 1.0 + Result * (z / k);
end;

function Module(z: TComplex): TReal;
begin
  with z do
    Result := Sqrt(Sqr(x) + Sqr(y));
end;

function Sqr(z: TComplex): TReal;
begin
  with z do
    Result := Sqr(x) + Sqr(y);
end;

procedure Read(out z: TComplex);
begin
  with z do
    System.Read(x, y);
end;

procedure Write(z: TComplex);
begin
  with z do
    System.Write(x:1:Digit, ' + i * ', y:1:Digit);
end;

procedure FFT(var C{:ARRAY[1..2^n]OF TComplex}; n: Byte);
var
  p:BYTE;
  j,k,dp,dj,dk,dr:DWord;
  X,z:TComplex;
  Ci,Co,Ct:PComplex;
  r:Real;
begin
  Ci := @C;
  GetMem(Co, (1 shl n) * SizeOf(TComplex));
  dp := (1 shl (n - 1)) * SizeOf(TComplex);
  for p := 0 to n - 1 do begin
    dk := 1 shl (n - p - 1) * SizeOf(TComplex);
    for k := 0 to ((1 shl p) - 1) do begin
      X := exp((-(Pi * k) / (1 shl p)) * i);{I prefer storing it to calculating it twice}
      for j := 0 to ((1 shl(n - p - 1)) - 1) do begin
        dj := ((k shl(n - p - 1)) + j) * SizeOf(TComplex);
        dr := ((k shl(n - p)) + j) * SizeOf(TComplex);
        z := (Ci + dr)^ + X * (Ci + dk + dr)^;
        (Co + dj)^ := z;
        z := (Ci + dr)^ - X * (Ci + dk + dr)^;
        (Co + dp + dj)^ := z;
        end;
      end;
      Ct := Co;
      Co := Ci;
      Ci := Ct;
    end;
    r := 1 shl(n div 2);
    if odd(n) then
      r := r * Sqrt(2);{r=2**(n/2)}
    Ct := @C;
    for k := 0 to (1 shl n) - 1 do begin
      z := (Ci + k * SizeOf(TComplex))^ / r;
      (Ct + k * SizeOf(TComplex))^ := z;
    end;
    if Odd(n) then
      FreeMem(Ci, (1 shl n) * SizeOf(TComplex))
    else
      FreeMem(Co, (1 shl n) * SizeOf(TComplex));
    {Return value is C}
end;
procedure IFFT(var C{:ARRAY[1..n]OF TComplex}; n: Byte);
var
  p:BYTE;
  j,k,dp,dj,dk,dr:DWord;
  X,z:TComplex;
  Ci,Co,Ct:PComplex;
  r:Real;
begin
  Ci := @C;
  GetMem(Co, (1 shl n) * SizeOf(TComplex));
  dp:=(1 shl (n - 1)) * SizeOf(TComplex);
  for p := 0 to n - 1 do begin
    dk := 1 shl (n - p - 1) * SizeOf(TComplex);
    for k := 0 to ((1 shl p) - 1) do begin
      X := exp(((Pi * k) / (1 shl p)) * i);{I prefer store it than calculate it twice}
      for j := 0 to ((1 shl (n - p - 1)) - 1) do begin
        dj := ((k shl (n - p - 1)) + j) * SizeOf(TComplex);
        dr := ((k shl (n - p)) + j) * SizeOf(TComplex);
        z := (Ci + dr)^ + X * (Ci + dk + dr)^;
        (Co + dj)^ := z;
        z := (Ci + dr)^ - X * (Ci + dk + dr)^;
        (Co + dp + dj)^ := z;
      end;
    end;
    Ct := Co;
    Co := Ci;
    Ci := Ct;
  end;
  r := 1 shl(n div 2);
  if odd(n) then
    r := r * Sqrt(2);{r=2**(n/2)}
  Ct := @C;
  for k := 0 to (1 shl n) - 1 do begin
    z := (Ci + k * SizeOf(TComplex))^ / r;
    (Ct + k * SizeOf(TComplex))^ := z;
  end;
  IF Odd(n) then
    FreeMem(Ci, (1 shl n) * SizeOf(TComplex))
  else
    FreeMem(Co, (1 shl n) * SizeOf(TComplex));
  {Return value is C}
end;
end.
