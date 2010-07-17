{*****************************************************************************}
{ File                   : complex.pas
  Author                 : Mazen NEIFER
  Creation date          : 21/09/2000
  Last modification date : 27/09/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT Complex;
{$MODE FPC}
INTERFACE
TYPE
  TReal=Real;{This can be changed to any real type to support more huge values}
  PReal=^TReal;
  PComplex=^TComplex;
  TComplex=RECORD
    x,y:Real;{It's too simple a complex is a couple of reals!}
  END;
  PMatrix=^TMatrix;
  TMatrix=RECORD
    n,p:Byte;{It will be nice if we can use "TMatrix=ARRAY[n,p]OF TComplex;"}
    Values:PComplex;{Till this will be possible we can usally use this}
  END;  
CONST
  i:TComplex=(x:0;y:1);{And what about the solution of x^2+1=0!!!!!?}
  Digit:Byte=3;{I prefer 3 zeros but you can change it}
OPERATOR :=(r:Real)RESULT:TComplex;
OPERATOR -(z:TComplex)RESULT:TComplex;
OPERATOR +(z1,z2:TComplex)RESULT:TComplex;
OPERATOR -(z1,z2:TComplex)RESULT:TComplex;
OPERATOR *(z1,z2:TComplex)RESULT:TComplex;
OPERATOR /(z1,z2:TComplex)RESULT:TComplex;
OPERATOR * (z:TComplex;n:Byte)RESULT:TComplex;
OPERATOR **(z:TComplex;n:Byte)RESULT:TComplex;
OPERATOR /(z:TComplex;n:Byte)RESULT:TComplex;
OPERATOR /(z:TComplex;n:DWord)RESULT:TComplex;
OPERATOR +(r:Real;z:TComplex)RESULT:TComplex;
OPERATOR *(r:Real;z:TComplex)RESULT:TComplex;
OPERATOR /(z:TComplex;r:Real)RESULT:TComplex;
OPERATOR =(z:TComplex;r:Real)RESULT:Boolean;
FUNCTION exp(z:TComplex):TComplex;
FUNCTION Module(z:TComplex):Real;
PROCEDURE ReadComplex(VAR z:TComplex);
PROCEDURE WriteComplex(z:TComplex);
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
{***************************Matrix********************************}
FUNCTION Matrix(_n,_p:Byte):TMatrix;
PROCEDURE KillMatrix(VAR aMatrix:TMatrix);
OPERATOR +(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR -(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR *(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR /(M1,M2:TMatrix)RESULT:TMatrix;
FUNCTION Det(aMatrix:TMatrix):TComplex;
FUNCTION Cofactor(aMatrix:TMatrix;k,l:Byte):TMatrix;
PROCEDURE ReadMatrix(n,p:Byte;VAR M:TMatrix);
PROCEDURE WriteMatrix(M:TMatrix);
IMPLEMENTATION
OPERATOR :=(r:Real)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=r;
        y:=0;
      END;
  END;
OPERATOR -(z:TComplex)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=-z.x;
        y:=-z.y;
      END;
  END;
OPERATOR +(z1,z2:TComplex)RESULT:TComplex;
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
OPERATOR *(z1,z2:TComplex)RESULT:TComplex;
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
OPERATOR *(z:TComplex;n:Byte)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x*n;
        y:=z.y*n;
      END;
  END;
OPERATOR **(z:TComplex;n:Byte) RESULT:TComplex;
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
OPERATOR /(z:TComplex;n:Byte)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x/n;
        y:=z.y/n;
      END;
  END;
OPERATOR /(z:TComplex;n:DWord)RESULT:TComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        x:=z.x/n;
        y:=z.y/n;
      END;
  END;
OPERATOR +(r:Real;z:TComplex)RESULT:TComplex;
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
OPERATOR /(z:TComplex;r:Real)RESULT:TComplex;
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
    RESULT:TComplex;
  BEGIN
    RESULT:=1;
    FOR k:=MaxLevel DOWNTO 1 DO
        RESULT:=1.0 + RESULT*(z/k);
    exp:=RESULT;
  END;
FUNCTION Module(z:TComplex):Real;
  BEGIN
    WITH z DO
      Module:=sqrt(sqr(x)+sqr(y));
  END;
PROCEDURE ReadComplex(VAR z:TComplex);
  BEGIN
    WITH z DO
      Read(x,y);
  END;
{  VAR
    s:STRING;
  BEGIN
    Read(s);
  END;}
PROCEDURE WriteComplex(z:TComplex);
  BEGIN
    WITH z DO
      Write(x:1:Digit,'+i*',y:1:Digit);
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
FUNCTION Matrix(_n,_p:Byte):TMatrix;
  BEGIN
    WITH Matrix DO
      BEGIN
        n:=_n;
        p:=_p;
        GetMem(Values,n*p*SizeOf(TComplex))
      END;
  END;
PROCEDURE KillMatrix(VAR aMatrix:TMatrix); 
  BEGIN
    WITH aMatrix DO
      BEGIN
        FreeMem(Values,n*p*SizeOf(TComplex));
        n:=0;
        p:=0;
      END;
  END;   
FUNCTION max(n,p:Byte):Byte;
  BEGIN
    IF n<p
    THEN
      max:=p
    ELSE
      max:=n;
  END;
OPERATOR +(M1,M2:TMatrix)RESULT:TMatrix;
  VAR
    i,j:Byte;
    _M,_M1,_M2:PComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        n:=max(M1.n,m2.n);
        p:=max(M1.p,M2.p);
        RESULT:=Matrix(n,p);
        _M:=Values;
        _M1:=M1.Values;
        _M2:=M2.Values;
        FOR i:=1 TO n DO
          FOR j:=1 TO p DO
            BEGIN
              _M^:=0;
              IF(i<=M1.n)AND(j<=M1.p)
              THEN
                _M^:=_M^+_M1^;
              IF(i<=M2.n)AND(j<=M2.p)
              THEN
                _M^:=_M^+_M2^;
              inc(_M);
              inc(_M1);
              inc(_M2);
            END;
      END;
  END;
OPERATOR -(M1,M2:TMatrix)RESULT:TMatrix;
  VAR
    i,j:Byte;
    _M,_M1,_M2:PComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        n:=max(M1.n,m2.n);
        p:=max(M1.p,M2.p);
        RESULT:=Matrix(n,p);
        _M:=Values;
        _M1:=M1.Values;
        _M2:=M2.Values;
        FOR i:=1 TO n DO
          FOR j:=1 TO p DO
            BEGIN
              _M^:=0;
              IF(i<=M1.n)AND(j<=M1.p)
              THEN
                _M^:=_M^+_M1^;
              IF(i<=M2.n)AND(j<=M2.p)
              THEN
                _M^:=_M^-_M2^;
              inc(_M);
              inc(_M1);
              inc(_M2);
            END;
      END;
  END;
OPERATOR *(M1,M2:TMatrix)RESULT:TMatrix;
  VAR
    i,j:Byte;
    _M,_M1,_M2:PComplex;
  BEGIN
    WITH RESULT DO
      BEGIN
        n:=max(M1.n,m2.n);
        p:=max(M1.p,M2.p);
        RESULT:=Matrix(n,p);
        _M:=Values;
        _M1:=M1.Values;
        _M2:=M2.Values;
        FOR i:=1 TO n DO
          FOR j:=1 TO p DO
            BEGIN
              _M^:=0;
              IF(i<=M1.n)AND(j<=M1.p)
              THEN
                _M^:=_M^+_M1^;
              IF(i<=M2.n)AND(j<=M2.p)
              THEN
                _M^:=_M^+_M2^;
              inc(_M);
              inc(_M1);
              inc(_M2);
            END;
      END;
  END;
OPERATOR /(M1,M2:TMatrix)RESULT:TMatrix;
  VAR
    i,j:Byte;
    _M,_M1,_M2:PComplex;
  BEGIN
    IF Det(M2)=0
    THEN
      RunError(0);
    WITH RESULT DO
      BEGIN
        n:=max(M1.n,m2.n);
        p:=max(M1.p,M2.p);
        RESULT:=Matrix(n,p);
        _M:=Values;
        _M1:=M1.Values;
        _M2:=M2.Values;
        FOR i:=1 TO n DO
          FOR j:=1 TO p DO
            BEGIN
              _M^:=0;
              IF(i<=M1.n)AND(j<=M1.p)
              THEN
                _M^:=_M^+_M1^;
              IF(i<=M2.n)AND(j<=M2.p)
              THEN
                _M^:=_M^+_M2^;
              inc(_M);
              inc(_M1);
              inc(_M2);
            END;
      END;
  END;
FUNCTION Cofactor(aMatrix:TMatrix;k,l:Byte):TMatrix;
  VAR
    i,j,i2,j2:Byte;
  BEGIN
    WITH aMatrix DO
      Cofactor:=Matrix(n-1,p-1);
    i:=0;
    j:=0;
    i2:=0;
    j2:=0;
    WITH Cofactor DO
      WHILE i<n DO
        BEGIN
	  IF i<>k
	  THEN
	    BEGIN
	      WHILE j<p DO
	        BEGIN
	          IF j<>l
	          THEN
	            BEGIN
	              Values[i2*(n-1)+j2]:=aMatrix.Values[i*n+j];
		      inc(j2);
		    END;
                  inc(j);
                END;
	      inc(i2);;
	    END;
	  inc(i);
        END;	       
  END;
FUNCTION Det(aMatrix:TMatrix):TComplex;
  VAR 
    j:Byte;
  BEGIN
    Det:=0;
    WITH aMatrix DO
      FOR j:=1 TO n DO
        Det:=Det+Det(Cofactor(aMatrix,1,j))*Values[j];
  END;
PROCEDURE WriteMatrix(M:TMatrix);
  VAR
    i,j:Byte;
  BEGIN
    WITH M DO
      FOR i:=0 TO n-1 DO
        BEGIN
	  FOR j:=0 TO p-1 DO
	    BEGIN
	      WriteComplex(Values[i*n+j]);
	      Write(' ');
	    END;
	  WriteLn;
	END;
  END;
PROCEDURE ReadMatrix(n,p:Byte;VAR M:TMatrix);
  VAR
    i,j:Byte;
  BEGIN
    M:=Matrix(n,p);
    WITH M DO
      FOR i:=1 TO n DO
        FOR j:=1 TO n DO
	  BEGIN
	    Write('M[',i,',',j,']=');
	    ReadComplex(Values[i*n+j]);
	  END;
  END;
END .
