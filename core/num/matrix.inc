{*****************************************************************************}
{ File                   : matrix.inc
  Author                 : Mazen NEIFER
  Creation date          : 13/10/2000
  Last modification date : 13/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
{$IFDEF Interface}
TYPE
   PMatrixBase=^TMatrixBase;
   PMatrix=^TMatrix;
   TMatrix=RECORD
     n,p:Word;{It will be nice if we can use "TMatrix=ARRAY[n,p]OF TComplex;"}
     Values:PMatrixBase;{Till this will be possible we can usally use this}
   END;
FUNCTION Matrix(_n,_p:Byte):TMatrix;
PROCEDURE KillMatrix(VAR M:TMatrix);
OPERATOR +(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR -(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR *(M1,M2:TMatrix)RESULT:TMatrix;
OPERATOR /(M1,M2:TMatrix)RESULT:TMatrix;
FUNCTION Det(M:TMatrix):TMatrixBase;
FUNCTION Cofactor(M:TMatrix;k,l:Byte):TMatrix;
FUNCTION Inv(M:TMatrix):TMatrix;
PROCEDURE Read(n,p:Byte;VAR M:TMatrix); overload;
PROCEDURE Write(M:TMatrix); overload;
{$ELSE}
PROCEDURE GenerateError(c:Char);
  BEGIN
    WriteLn('Unit MatrixBaseMatrix : Size mismatch in call to OPERATOR',c,
            '(x,y:TMatrix):RESULT;');
    RunError(1);
  END;	      
FUNCTION Matrix(_n,_p:Byte):TMatrix;
  BEGIN
    WITH Matrix DO
      BEGIN
        n:=_n;
        p:=_p;
        GetMem(Values,n*p*SizeOf(TMatrixBase))
      END;
  END;
PROCEDURE KillMatrix(VAR M:TMatrix); 
  BEGIN
    WITH M DO
      BEGIN
        FreeMem(Values,n*p*SizeOf(TMatrixBase));
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
    _M,_M1,_M2:PMatrixBase;
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
    _M,_M1,_M2:PMatrixBase;
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
    _M,_M1,_M2:PMatrixBase;
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
    _M,_M1,_M2:PMatrixBase;
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
FUNCTION Cofactor(M:TMatrix;k,l:Byte):TMatrix;
  VAR
    i,j,i2,j2:Byte;
  BEGIN
    WITH M DO
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
	              Values[i2*(n-1)+j2]:=M.Values[i*n+j];
		      inc(j2);
		    END;
                  inc(j);
                END;
	      inc(i2);;
	    END;
	  inc(i);
        END;	       
  END;
FUNCTION Det(M:TMatrix):TMatrixBase;
  VAR 
    j:Byte;
  BEGIN
    Det:=0;
    WITH M DO
      FOR j:=1 TO n DO
        Det:=Det+Det(Cofactor(M,1,j))*Values[j];
  END;
PROCEDURE Write(M:TMatrix);
  VAR
    i,j:Byte;
  BEGIN
    WITH M DO
      FOR i:=0 TO n-1 DO
        BEGIN
	  FOR j:=0 TO p-1 DO
	    BEGIN
	      Write(Values[i*n+j]);
	      System.Write(' ');
	    END;
	  WriteLn;
	END;
  END;
FUNCTION Inv(M:TMatrix):TMatrix;
  VAR
    d:TMatrixBase;
    i,j:Word;
  BEGIN
    d:=Det(M);
    IF d=0
    THEN
      BEGIN
        WriteLn('Error : Can not inverse matrix M with Det(M)=0');
	Halt(1);
      END
    ELSE
      BEGIN
        Inv:=Matrix(M.n,M.p);
        WITH Inv DO
          FOR i:=0 TO n-1 DO
            FOR j:=0 TO p-1 DO
	      Values[j*n+i]:=Det(Cofactor(M,i,j))/d;{Transposition is done as}
      END;{we used Values[j,i] instead of Values[i,j]}
  END;
PROCEDURE Read(n,p:Byte;VAR M:TMatrix);
  VAR
    i,j:Byte;
  BEGIN
    M:=Matrix(n,p);
    WITH M DO
      FOR i:=1 TO n DO
        FOR j:=1 TO n DO
	  BEGIN
	    System.Write('M[',i,',',j,']=');
	    Read(Values[i*n+j]);
	  END;
  END;
{$ENDIF}
