{*****************************************************************************}
{ File                   : vectors.inc
  Author                 : Mazen NEIFER
  Creation date          : 12/10/2000
  Last modification date : 31/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
{Interface part of real/Longint/...vectors.pas}
{$IFDEF INTERFACE}
  PVectorBase=^TVectorBase;{Base type must be defined before including this}
  PVector=^TVector;{different types. It is possible to use a generic code}
  TVector=RECORD{then compile it with different base types (Real, LongInt...}
    n:TVectorIndex;{n=MaxOffset=Dim(vector)-1);}
    Values:PVectorBase;{Points to the first entry of the stored vales table}
  END;
OPERATOR +(x,y:TVector)RESULT:TVector;{Use at your own risk}
OPERATOR -(x,y:TVector)RESULT:TVector;{Those functions creates ther results}
OPERATOR *(x,y:TVector)RESULT:TVectorBase;{and it is up to you to take care}
OPERATOR *(r:TVectorBase;x:TVector)RESULT:TVector;{of avalable memory and}
PROCEDURE Acc(VAR x:TVector;r:TVectorBase;y:TVector);{to free them.}
{This procedure adds to vector "x" a vector "y" mutiplied by a scalar "r"
result is stored in "x". It is equavalent to x:=x+r*y;. This
procedure is just better than its equivalent given code because it is faster
and it saves memory. See comments of next function for more details.}
PROCEDURE Add(VAR x:TVector;y:TVector);
{This function allow you to make vector addition. It expects two Vector "x"
and "y" that have the same dimention and adds "y" to "x" storing result in "x".
This function has the same effect as "x:=x+y" expect that, in this last case,
a new vector is created on the heap and is returned to "x". Of course, if we
didn't make the precaution to store the value of "x" in an other variable, the
array into which it pointed is not pointed any more and may be lost until the
program exits. When used once it can be accepted but in a signal processing
loop this can cause a lack of memory. That's why using "Add(x,y)" is safer as
no new heap is allocated and can be used almost as many time as you want. 
Please note that the "y" vector is not killed after calcultion so it will not
be safe to use "Add(x,f(y))" where "f" is a function that creates new variable.
In fact such a call is performed by creating a temporary variable which will be
removed just before "Add" exits. This will cause the same problem as mensioned
erlyer.}
FUNCTION Copy(x:TVector):TVector;
{This function creates a new instance of a Vector. It deffers from "x:=y" in
that this last call causes the compiler to generate a code that make "x" point
to the same value array as y. It is just same as assigning pointers. When using
"x:=Copy(y)" the function "Copy" will creates a new array and copy in it the
values of the array pointed by "y". This is particulary useful to make
additions. The following codes have the same effect "z:=x+y" and
"z:=Copy(x);Add(z,y)"}
FUNCTION Dim(x:TVector):TVectorIndex;
{This function just return the dimention of the vector. It doesn't return the
size of the variable and so it is different from "SizeOf(x)". The dimension of
a vector is stored in a the "x.n" field and is equal of the array size. Please
notice that it is better to use "Dim(x)" than "x.n" as the way of coding
"TVector" may be changed and that "Dim(x)" will keep code comptibility.}
PROCEDURE KillVector(VAR x:TVector);
{This procedure kills a vector by restoring its memory in the heap and assiging
it (0,NIL). Please notice that if you assigned a vector y using "y:=x", killing
"x" or "y" kills the two Vector. There is however a little problem as calling
"KillVector(x)" after this kind off assignement will makes "y" have an erronous
values as its dimension is false and it "Values" points on an unavailable part
of the memory.}
FUNCTION Mean(x: TVector): TReal;
{This function returns the mean value of the vector. It is just the sommation
of all Vector coordinates divided by the dimension}
FUNCTION Module(x:TVector):Real;
{"Module" or "Power" allow you to caculate the mean of the vector of same
dimension as "x" and which components are equal to the squre of those of "x".
This quantity is the module of the vector that is called the signal power or
the signal energy if the vector represents a signal}
PROCEDURE Mul(VAR x:TVector;r:TVectorBase);
{This function do the multiplication of vector "x" by the scalar "r". The
result is a vector which every component is the product of the corresponding
"x" component by "r". It is equivalent to "x:=r*x" expect this function doesn't
create a new dinamical variable and doesn't cause loss of heap.}
PROCEDURE ReDim(VAR x:TVector;n:TVectorIndex);
{This function allow you to change the vector dimention. If the new size of the
vector is less than its orginal size, then a part of the vector memory will be
freed but the first values will be kept end will not be changed. This is
particulary useful if you want to truncate a vector and you didn't need the
last values. Instead of killing it with KillVector and recreating it you will
just need to redimension it. If the new size is greater than the old one the
ReDim function will allocate a new table, copy old vector in the first part of
the vector then free the old one. This mechanisme is transparent to the user
that can consider that his vector just changed size and kept his old values in
the components indexed by integers below or equal to the minimum of the new
size and the olde one.}
PROCEDURE Sub(VAR x:TVector;y:TVector);
{This function allow you to make vector substraction. It returns in "x" the
Vector that is the diffrence between "x" and "y". The code "Sub(x,y)" is
almost equivalent to "x:=x-y" expect, in the first case, no new value table is
created. In the latest, if no precaution was made to save "x" value, it will
cause the program to have a dinamical variable allocated from the heap but that
is no more pointed and thus can't be deallocated until the program exits. It
works same as "Add" expect it substract Vector.}
FUNCTION SubVector(x:TVector;n,i:TVectorIndex):TVector;
{This function returns a vector of dimention n, wich first entry is the 
ith entry of "x". It just assigns a correct value to "x.Values" but
didn't allocate memory and copy values on it. So it you can not remove it using
"KillVector" function. }
FUNCTION Vector(n:TVectorIndex):TVector;
{This function creates a new vector by allocating enoth memory to store n
values and returns a vector with dimension equal to "n". Please note that you
need to call this function only if you will assign your vector value by value
otherwise all functions that returns result of type "TVector" uses it to
create there return variables. This causes a restriction in using vector result
function as they may be used only this way "y:=f(x...)". Once you didn't need
"y" any more you need to delete it from the heap yourself.}
{Implementation part of real/longint/...Vector.pas}
{$ELSE}
{$IFNDEF USE_CUSTOM_FASTMOVE}
PROCEDURE FastMove(VAR Destination, Source: TVectorBase; n: TVectorIndex);INLINE;
begin
  Move(Destination, Source, n * SizeOf(TVectorBase));
end;
{$ENDIF USE_CUSTOM_FASTMOVE}
PROCEDURE GenerateError(c:Char);
  BEGIN
    WriteLn('Unit VectorBaseVector : Size mismatch in call to OPERATOR',c,
            '(x,y:TVector):RESULT;');
    RunError(1);
  END;
OPERATOR +(x,y:TVector)RESULT:TVector;
  VAR
    i:TVectorIndex;
  BEGIN
    WITH RESULT DO
      IF x.n=y.n
      THEN
        BEGIN
	  RESULT:=Vector(x.n);
          FOR i:=0 TO n DO
	  Values[i]:=x.Values[i]+y.Values[i];
	END
      ELSE
        GenerateError('+');
  END;
OPERATOR -(x,y:TVector)RESULT:TVector;
  VAR
    i:TVectorIndex;
  BEGIN
    WITH RESULT DO
      IF x.n=y.n
      THEN
        BEGIN
	  RESULT:=Vector(x.n);
          FOR i:=0 TO n DO
	  Values[i]:=x.Values[i]-y.Values[i];
	END
      ELSE
        GenerateError('-');
  END;
OPERATOR *(x,y:TVector)RESULT:TVectorBase;
  VAR
    i:TVectorIndex;
  BEGIN
    IF x.n=y.n
    THEN
      BEGIN
        RESULT:=0;
        FOR i:=0 TO x.n DO
          RESULT+=x.Values[i]*y.Values[i];
      END
    ELSE
      GenerateError('*');
  END;
OPERATOR *(r:TVectorBase;x:TVector)RESULT:TVector;
  VAR
    I:TVectorIndex;
  BEGIN
    WITH RESULT DO
      BEGIN
        RESULT:=Vector(x.n);
	FOR i:=0 TO n DO
          Values[i]:=r*x.Values[i];
      END;
  END;
PROCEDURE Acc(VAR x:TVector;r:TVectorBase;y:TVector);
  VAR
    i:TVectorIndex;
  BEGIN
    WITH x DO
      IF n=y.n 
      THEN
        FOR i:=0 TO n DO
	  Values[i]+=r*y.Values[i];
  END;
PROCEDURE Add(VAR x:TVector;y:TVector);
  VAR
    i:TVectorIndex;
  BEGIN
    WITH x DO
      IF n=y.n 
      THEN
        FOR i:=0 TO n DO
	  Values[i]+=y.Values[i];
  END;
FUNCTION Copy(x:TVector):TVector;
  BEGIN
    Copy:=Vector(x.n+1);
    WITH Copy DO
      FastMove(Values^,x.Values^,n+1);
  END;
FUNCTION Dim(x:TVector):TVectorIndex;
  BEGIN
    Dim:=x.n+1;
  END;
PROCEDURE KillVector(VAR x:TVector);
  BEGIN
    WITH x DO
      BEGIN
        FreeMem(Values,n+1);
        n:=0;
      END;
  END;
function Mean(x: TVector): TReal;
var
  i: TVectorIndex;
begin
  with x do begin
    Mean := 0;
	  for i := 0 to n do
	    Mean += Sqr(Values[i]);
  	Mean /= n;
  end;
end;

function Module(x:TVector):Real;
var
  i:TVectorIndex;
begin
  {Module:=Sqrt(x .* x);}
  Module := 0;
  with x do
    for i:=0 to n do
      Module += Sqr(Values[i]);
  Module := Sqrt(Module);
END;

PROCEDURE Mul(VAR x:TVector;r:TVectorBase);
  VAR
    i:TVectorIndex;
  BEGIN
    WITH x DO
      FOR i:=0 to n DO
        Values[i]*=r;
  END;
PROCEDURE ReDim(VAR x:TVector;n:TVectorIndex);
  VAR
    t:TVector;
  BEGIN
    IF n<x.n
    THEN
      FreeMem(x.Values,x.n-n)
    ELSE
      BEGIN
        t:=x;
	x:=Vector(n);
	FastMove(x.Values^,t.Values^,n);
        KillVector(t);
      END;
  END;
PROCEDURE Sub(VAR x:TVector;y:TVector);
  VAR
    i:TVectorIndex;
  BEGIN
    WITH x DO
      IF n=y.n
      THEN
        FOR i:=0 TO n DO
	  Values[i]-=y.Values[i];
  END;
FUNCTION SubVector(x:TVector;n,i:TVectorIndex):TVector;
  BEGIN
    SubVector.n:=n-1;
    SubVector.Values:=x.Values+i;
  END;
FUNCTION Vector(n:TVectorIndex):TVector;
  BEGIN
    Vector.n:=n-1;
    Vector.Values:=GetMem(n*SizeOf(TVectorBase));
  END;
{$ENDIF}
