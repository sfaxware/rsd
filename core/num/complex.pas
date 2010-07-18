//*****************************************************************************
// File                   : complex.pas
// Author                 : Mazen NEIFER
// Creation date          : 2000-09-21
// Last modification date : 2010-07-18
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
UNIT Complex;
{$MODE ObjFpc}
INTERFACE

uses
  Reals;

TYPE
  TReal=Real;{This can be changed to any real type to support more huge values}
  PReal=^TReal;
  PComplex=^TComplex;
  TComplex=RECORD
    x,y:Real;{It's too simple a complex is a couple of reals!}
  END;
CONST
  i:TComplex=(x:0;y:1);{And what about the solution of x^2+1=0!!!!!?}
  Digit:Byte=3;{I prefer 3 zeros but you can change it}
OPERATOR :=(r: Real):TComplex;
OPERATOR -(z: TComplex):TComplex;
operator not (z: TComplex): TComplex;
OPERATOR +(z1, z2: TComplex): TComplex;
OPERATOR -(z1, z2: TComplex): TComplex;
OPERATOR *(z1, z2: TComplex): TComplex;
OPERATOR /(z1, z2: TComplex): TComplex;
operator and(z1, z2: TComplex): TComplex;
OPERATOR * (z: TComplex; n: Byte): TComplex;
OPERATOR **(z: TComplex; n: Byte): TComplex;
OPERATOR /(z: TComplex; n: Byte): TComplex;
OPERATOR /(z: TComplex; n: DWord): TComplex;
OPERATOR +(r: Real; z: TComplex): TComplex;
OPERATOR *(r: Real; z: TComplex): TComplex;
OPERATOR /(z: TComplex; r: Real): TComplex;
OPERATOR =(z: TComplex; r: Real): Boolean;
FUNCTION exp(z:TComplex):TComplex; overload;
function Module(z: TComplex): TReal; overload;
function Sqr(z: TComplex): TReal; overload;
PROCEDURE Read(VAR z:TComplex); overload;
PROCEDURE Write(z:TComplex); overload;
PROCEDURE FFT(VAR C{:ARRAY[1..n]OF TComplex};n:Byte);
{This function returns the FFT of a signal. The signal is supposed to be stored 
in an array wich size is 2^n. It stores the result at the same array that
contains the input signal. So you have to keep the original signal some where
if you need it. 
Suppose you have a signal defined by regular spaced points. Those points define
an array of complex values lets call it Signal=ARRAY[1..size]OF TCcomplex. When
you applay FFT(Signal,n) (of course size=2^n to get some thing correct), you
get in Signal no more the signal itself but its FFT values. Of course to
restore the signal you have to applay IFFT(Signal,n) without modifing any value in the array Signal.}

PROCEDURE IFFT(VAR C{:ARRAY[1..n]OF TComplex};n:Byte);
{This function is too similar to the FFT one. It calculates the FFT-1 of a
signal. It is used like the FFT function.}
IMPLEMENTATION
OPERATOR :=(r:Real):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=r;
        y:=0;
      END;
  END;
OPERATOR -(z:TComplex):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=-z.x;
        y:=-z.y;
      END;
  END;

operator not(z:TComplex):TComplex;
begin
  with Result do
    y := -z.y;
end;

OPERATOR +(z1,z2:TComplex):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z1.x+z2.x;
        y:=z1.y+z2.y;
      END;
  END;
OPERATOR -(z1,z2:TComplex)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z1.x-z2.x;
        y:=z1.y-z2.y;
      END;
  END;
OPERATOR *(z1,z2:TComplex):TComplex;
   BEGIN
     WITH RESULT DO
       BEGIN
         x:=z1.x*z2.x-z1.y*z2.y;
         y:=z1.x*z2.y+z1.y*z2.x;
       END;
   END;
OPERATOR /(z1,z2:TComplex)RESULT:TComplex;
  VAR
    M:Real;
  BEGIN
    M:=Module(z2);
    WITH RESULT DO
      BEGIN
        x:=(z1.x*z2.x+z1.y*z2.y)/M;
        y:=(z1.y*z2.x-z1.x*z2.y)/M;
      END;
  END;

operator and(z1, z2: TComplex): TComplex;
begin
  with result do begin
     x := z1.x * z2.x + z1.y * z2.y;
     y := z1.y * z2.x - z1.x * z2.y;
  end;
end;

OPERATOR *(z:TComplex;n:Byte):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x*n;
        y:=z.y*n;
      END;
  END;
OPERATOR **(z:TComplex;n:Byte) :TComplex;
  VAR
    i:Byte;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=1;
        y:=0;
        FOR i:=1 TO n DO
          BEGIN
            x:=x*z.x-y*z.y;
            y:=x*z.y+y*z.x;
          END;
      END;
  END;
OPERATOR /(z:TComplex;n:Byte):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x/n;
        y:=z.y/n;
      END;
  END;
OPERATOR /(z:TComplex;n:DWord):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x/n;
        y:=z.y/n;
      END;
  END;
OPERATOR +(r:Real;z:TComplex):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x+r;
        y:=z.y;
      END;
  END;
OPERATOR *(r:Real;z:TComplex)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=r*z.x;
        y:=r*z.y;
      END;
  END;
OPERATOR /(z:TComplex;r:Real):TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x/r;
        y:=z.y/r;
      END;
  END;
OPERATOR =(z:TComplex;r:Real)RESULT:Boolean;
  BEGIN
    WITH z DO
      RESULT:=(x=r)AND(y=0);
  END;
FUNCTION exp(z:TComplex):TComplex;
  CONST
    MaxLevel=20;
  VAR
    k:Byte;
  BEGIN
    RESULT:=1;
    FOR k:=MaxLevel DOWNTO 1 DO
        RESULT:=1.0 + RESULT*(z/k);
  END;

function Module(z: TComplex): TReal;
begin
  with z do
    Result := Sqrt(Sqr(x) + Sqr(y));
end;

function Sqr(z: TComplex): TReal; overload;
begin
  with z do
    Result := Sqr(x) + Sqr(y);
end;

PROCEDURE Read(VAR z:TComplex);
  BEGIN
    WITH z DO
      System.Read(x,y);
  END;
{  VAR
    s:STRING;
  BEGIN
    Read(s);
  END;}
PROCEDURE Write(z:TComplex);
  BEGIN
    WITH z DO
      System.Write(x:1:Digit,' + i * ',y:1:Digit);
  END;
PROCEDURE FFT(VAR C{:ARRAY[1..2^n]OF TComplex};n:Byte);
  VAR
    p:BYTE;
    j,k,dp,dj,dk,dr:DWord;
    X,z:TComplex;
    Ci,Co,Ct:PComplex;
    r:Real;
  BEGIN
    Ci:=@C;
    GetMem(Co,(1 SHL n)*SizeOf(TComplex));
    dp:=(1 SHL(n-1))*SizeOf(TComplex);
    FOR p:=0 TO n-1 DO  
      BEGIN
        dk:=1 SHL(n-p-1)*SizeOf(TComplex);
        FOR k:=0 TO ((1 SHL p)-1) DO
          BEGIN   
            X:=exp((-(Pi*k)/(1 SHL p))*i);{I prefer storing it to calculating it twice}
            FOR j:=0 TO ((1 SHL(n-p-1))-1) DO
              BEGIN
                dj:=((k SHL(n-p-1))+j)*SizeOf(TComplex);
                dr:=((k SHL(n-p))+j)*SizeOf(TComplex);
                z:=(Ci+dr)^+X*(Ci+dk+dr)^; 
                (Co+dj)^:=z;
                z:=(Ci+dr)^-X*(Ci+dk+dr)^;
                (Co+dp+dj)^:=z;
              END;
          END;
        Ct:=Co; 
        Co:=Ci;
        Ci:=Ct;        
      END;
    r:=1 SHL(n DIV 2);    
    IF odd(n)
    THEN
      r:=r*Sqrt(2);{r=2**(n/2)}
    Ct:=@C;
    FOR k:=0 TO (1 SHL n)-1 DO
      BEGIN
        z:=(Ci+k*SizeOf(TComplex))^/r;
        (Ct+k*SizeOf(TComplex))^:=z; 
      END;
    IF Odd(n)
    THEN
      FreeMem(Ci,(1 SHL n)*SizeOf(TComplex))   
    ELSE
      FreeMem(Co,(1 SHL n)*SizeOf(TComplex));
    {Return value is C}
  END;
PROCEDURE IFFT(VAR C{:ARRAY[1..n]OF TComplex};n:Byte);
  VAR         
    p:BYTE;
    j,k,dp,dj,dk,dr:DWord;
    X,z:TComplex;
    Ci,Co,Ct:PComplex;
    r:Real;
  BEGIN 
    Ci:=@C; 
    GetMem(Co,(1 SHL n)*SizeOf(TComplex)); 
    dp:=(1 SHL(n-1))*SizeOf(TComplex); 
    FOR p:=0 TO n-1 DO 
      BEGIN
        dk:=1 SHL(n-p-1)*SizeOf(TComplex);
        FOR k:=0 TO ((1 SHL p)-1)DO
          BEGIN
            X:=exp(((Pi*k)/(1 SHL p))*i);{I prefer store it than calculate it twice}
            FOR j:=0 TO ((1 SHL(n-p-1))-1) DO
              BEGIN
                dj:=((k SHL(n-p-1))+j)*SizeOf(TComplex);
                dr:=((k SHL(n-p))+j)*SizeOf(TComplex);
                z:=(Ci+dr)^+X*(Ci+dk+dr)^;
                (Co+dj)^:=z;
                z:=(Ci+dr)^-X*(Ci+dk+dr)^;
                (Co+dp+dj)^:=z;
              END;
          END;
        Ct:=Co; 
        Co:=Ci;
        Ci:=Ct; 
      END;
    r:=1 SHL(n DIV 2); 
    IF odd(n)
    THEN 
      r:=r*Sqrt(2);{r=2**(n/2)} 
    Ct:=@C; 
    FOR k:=0 TO (1 SHL n)-1 DO 
      BEGIN 
        z:=(Ci+k*SizeOf(TComplex))^/r;                                 
        (Ct+k*SizeOf(TComplex))^:=z;                                    
      END;                                                            
    IF Odd(n)                                         
    THEN
      FreeMem(Ci,(1 SHL n)*SizeOf(TComplex))
    ELSE 
      FreeMem(Co,(1 SHL n)*SizeOf(TComplex));  
    {Return value is C}     
  END;
END .
