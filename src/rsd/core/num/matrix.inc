{*****************************************************************************}
{ File                   : matrix.inc
  Author                 : Mazen NEIFER
  Creation date          : 13/10/2000
  Last modification date : 13/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
{$IFDEF INTERFACE}
TYPE
   PMatrixBase = ^TMatrixBase;
   PMatrix = ^TMatrix;
   TMatrix = record
     n, p: TMatrixIndex;{It will be nice if we can use "TMatrix=ARRAY[n,p]OF TComplex;"}
     Values: PMatrixBase;{Till this will be possible we can usally use this}
   end;

procedure KillMatrix(var M: TMatrix);
operator +(M1, M2: TMatrix): TMatrix;
operator -(M1, M2: TMatrix): TMatrix;
operator *(M1, M2: TMatrix): TMatrix;
operator /(M1, M2: TMatrix): TMatrix;
function Det(M: TMatrix): TMatrixBase;
function Cofactor(M: TMatrix; k, l: TMatrixIndex): TMatrix;
function Inv(M: TMatrix): TMatrix;
function Matrix(_n, _p: TMatrixIndex): TMatrix;
procedure Read(n, p: TMatrixIndex; var M: TMatrix); overload;
procedure Write(M: TMatrix); overload;
{$else INTERFACE}
procedure GenerateError(c: Char);
begin
  WriteLn('Unit MatrixBaseMatrix : Size mismatch in call to operator',c,
          '(x, y: TMatrix);');
  RunError(1);
end;

function Matrix(_n, _p: TMatrixIndex): TMatrix;
begin
  with Matrix do begin
    n := _n;
    p := _p;
    GetMem(Values, n * p * SizeOf(TMatrixBase))
  end;
end;

procedure KillMatrix(var M: TMatrix);
begin
  with M do begin
    FreeMem(Values, n * p * SizeOf(TMatrixBase));
    n := 0;
    p := 0;
  end;
end;

function max(n, p: TMatrixIndex): TMatrixIndex;
begin
  if n < p
  then
    max := p
  else
    max := n;
end;

operator +(M1, M2: TMatrix): TMatrix;
var
  i, j: TMatrixIndex;
  _M, _M1, _M2: PMatrixBase;
begin
  with Result do begin
    n := max(M1.n, m2.n);
    p := max(M1.p, M2.p);
    Result := Matrix(n, p);
    _M := Values;
    _M1 := M1.Values;
    _M2 := M2.Values;
    for i := 1 to n do
      for j := 1 to p do begin
        _M^ := 0;
        if(i <= M1.n) and (j <= M1.p) then
          _M^ += _M1^;
        if(i <= M2.n) and (j <= M2.p) then
          _M^ += _M2^;
        inc(_M);
        inc(_M1);
        inc(_M2);
      end;
  end;
end;

operator -(M1, M2: TMatrix): TMatrix;
var
  i, j: TMatrixIndex;
  _M, _M1, _M2: PMatrixBase;
begin
  with Result do begin
    n := max(M1.n, m2.n);
    p := max(M1.p, M2.p);
    Result := Matrix(n, p);
    _M := Values;
    _M1 := M1.Values;
    _M2 := M2.Values;
    for i := 1 to n do
      for j := 1 to p do begin
        _M^ := 0;
        if(i <= M1.n) and (j <= M1.p) then
          _M^ += _M1^;
        if(i <= M2.n) and (j <= M2.p) then
          _M^ -= _M2^;
        inc(_M);
        inc(_M1);
        inc(_M2);
      end;
  end;
end;

operator *(M1, M2: TMatrix): TMatrix;
var
  i, j: TMatrixIndex;
  _M, _M1, _M2: PMatrixBase;
begin
  with Result do begin
    n := max(M1.n, m2.n);
    p := max(M1.p, M2.p);
    Result := Matrix(n, p);
    _M := Values;
    _M1 := M1.Values;
    _M2 := M2.Values;
    for i := 1 to n do
      for j := 1 to p do begin
        _M^ := 0;
        if(i <= M1.n) and (j <= M1.p)
        then
          _M^:=_M^+_M1^; //TODO: Fix matrix multiplication
        if(i <= M2.n) and (j <= M2.p)
        then
          _M^:=_M^+_M2^;
        inc(_M);
        inc(_M1);
        inc(_M2);
      end;
  end;
end;
operator /(M1, M2: TMatrix): TMatrix;
var
  i, j: TMatrixIndex;
  _M, _M1, _M2: PMatrixBase;
begin
  if Det(M2)=0
  then
    RunError(0);
  with Result do
    begin
      n := max(M1.n, m2.n);
      p := max(M1.p, M2.p);
      Result := Matrix(n, p);
      _M := Values;
      _M1 := M1.Values;
      _M2 := M2.Values;
      for i:=1 to n do
        for j:=1 to p do
          begin
            _M^:=0;
            if(i <= M1.n) and (j <= M1.p) then
              _M^:=_M^+_M1^;
            if(i <= M2.n) and (j <= M2.p) then
              _M^:=_M^+_M2^;
            inc(_M);
            inc(_M1);
            inc(_M2);
          end;
    end;
end;

function Cofactor(M: TMatrix; k, l: TMatrixIndex): TMatrix;
var
  i, j, i2, j2:TMatrixIndex;
begin
  with M do
    Cofactor := Matrix(n - 1, p - 1);
  i := 0;
  j := 0;
  i2 := 0;
  j2 := 0;
  with Cofactor do
    while i < n do begin
	    if i <> k then begin
	      while j < p do begin
	        if j<>l then begin
	          Values[i2 * n + j2] := M.Values[i * M.n + j];
		        inc(j2);
		      end;
          inc(j);
        end;
	      inc(i2);;
	    end;
	    inc(i);
    end;
end;

function Det(M: TMatrix): TMatrixBase;
var
  j: TMatrixIndex;
begin
  Det := 0;
  with M do
    for j := 0 to n do
      Det += Det(Cofactor(M, 1, j)) * Values[j];
end;

procedure Write(M: TMatrix);
var
  i, j: TMatrixIndex;
begin
  with M do
    for i := 0 to n do begin
  	  for j := 0 to p do begin
	      Write(Values[i * n + j]);
	      System.Write(' ');
	    end;
	  WriteLn;
	end;
end;

function Inv(M: TMatrix): TMatrix;
var
  d: TMatrixBase;
  i, j: Word;
begin
  d := Det(M);
  if d = 0 then begin
    WriteLn('Error : Can not inverse matrix M with Det(M)=0');
	  Halt(1);
  end else begin
    Inv := Matrix(M.n, M.p);
    with Inv do
      for i := 0 to n do
        for j := 0 to p do
	        Values[j * n + i] := Det(Cofactor(M, i, j)) / d;{Transposition is done as}
  end;{we used Values[j,i] instead of Values[i,j]}
end;

procedure Read(n, p: TMatrixIndex; var M: TMatrix);
var
  i, j: TMatrixIndex;
begin
  M := Matrix(n, p);
  with M do
    for i := 0 to n do
      for j := 0 to n do begin
	      System.Write('M[',i,', ',j,'] := ');
	      Read(Values[i * n + j]);
	    end;
end;
{$ENDIF INTERFACE}
