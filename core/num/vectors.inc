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
  PVectorsBase=^TVectorsBase;{Base type must be defined before including this}
  PVectors=^TVectors;{different types. It is possible to use a generic code}
  TVectors=RECORD{then compile it with different base types (Real, LongInt...}
    n:TVectorsIndex;{n=MaxOffset=Dim(vector)-1);}
    Values:PVectorsBase;{Points to the first entry of the stored vales table} 
  END;
OPERATOR +(x,y:TVectors)RESULT:TVectors;{Use at your own risk}
OPERATOR -(x,y:TVectors)RESULT:TVectors;{Those functions creates ther results}
OPERATOR *(x,y:TVectors)RESULT:TVectorsBase;{and it is up to you to take care}
OPERATOR *(r:TVectorsBase;x:TVectors)RESULT:TVectors;{of avalable memory and}
PROCEDURE Add(VAR x:TVectors;r:TVectorsBase;y:TVectors);{to free them.}
{This procedure adds to vector "x" a vector "y" mutiplied by a scalar "r"
resalt is stored in "x". It is equavalent to x:=x+r*y;. This
procedure is just better than its equivalent given code because it is faster
and it saves memory. See comments of next function for more details.}
PROCEDURE Add(VAR x:TVectors;y:TVectors);
{This function allow you to make vector addition. It expects two vectors "x"
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
FUNCTION Copy(x:TVectors):TVectors;
{This function creates a new instance of a vectors. It deffers from "x:=y" in
that this last call causes the compiler to generate a code that make "x" point
to the same value array as y. It is just same as assigning pointers. When using
"x:=Copy(y)" the function "Copy" will creates a new array and copy in it the
values of the array pointed by "y". This is particulary useful to make
additions. The following codes have the same effect "z:=x+y" and
"z:=Copy(x);Add(z,y)"}
FUNCTION Dim(x:TVectors):TVectorsIndex;
{This function just return the dimention of the vector. It doesn't return the
size of the variable and so it is different from "SizeOf(x)". The dimension of
a vector is stored in a the "x.n" field and is equal of the array size. Please
notice that it is better to use "Dim(x)" than "x.n" as the way of coding
"TVectors" may be changed and that "Dim(x)" will keep code comptibility.}
PROCEDURE KillVector(VAR x:TVectors);
{This procedure kills a vector by restoring its memory in the heap and assiging
it (0,NIL). Please notice that if you assigned a vector y using "y:=x", killing
"x" or "y" kills the two vectors. There is however a little problem as calling
"KillVector(x)" after this kind off assignement will makes "y" have an erronous
values as its dimension is false and it "Values" points on an unavailable part
of the memory.}
FUNCTION Mean(x:TVectors):Real;
{This function returns the mean value of the vector. It is just the sommation
of all vectors coordinates divided by the dimension}
FUNCTION Module(x:TVectors):Real;
{"Module" or "Power" allow you to caculate the mean of the vector of same
dimension as "x" and which components are equal to the squre of those of "x".
This quantity is the module of the vector that is called the signal power or
the signal energy if the vector represents a signal}
PROCEDURE Mul(VAR x:TVectors;r:TVectorsBase);
{This function do the multiplication of vector "x" by the scalar "r". The
result is a vector which every component is the product of the corresponding
"x" component by "r". It is equivalent to "x:=r*x" expect this function doesn't
create a new dinamical variable and doesn't cause loss of heap.}
PROCEDURE ReDim(VAR x:TVectors;n:TVectorsIndex);
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
PROCEDURE Sub(VAR x:TVectors;y:TVectors);
{This function allow you to make vector substraction. It returns in "x" the
vectors that is the diffrence between "x" and "y". The code "Sub(x,y)" is
almost equivalent to "x:=x-y" expect, in the first case, no new value table is
created. In the latest, if no precaution was made to save "x" value, it will
cause the program to have a dinamical variable allocated from the heap but that
is no more pointed and thus can't be deallocated until the program exits. It
works same as "Add" expect it substract vectors.}
FUNCTION SubVector(x:TVectors;n,i:TVectorsIndex):TVectors;
{This function returns a vector of dimention n, wich first entry is the 
ith entry of "x". It just assigns a correct value to "x.Values" but
didn't allocate memory and copy values on it. So it you can not remove it using
"KillVector" function. }
FUNCTION Vector(n:TVectorsIndex):TVectors;
{This function creates a new vector by allocating enoth memory to store n
values and returns a vector with dimension equal to "n". Please note that you
need to call this function only if you will assign your vector value by value
otherwise all functions that returns result of type "TVectors" uses it to
create there return variables. This causes a restriction in using vector result
function as they may be used only this way "y:=f(x...)". Once you didn't need
"y" any more you need to delete it from the heap yourself.}
{Implementation part of real/longint/...vectors.pas}
{$ELSE}
{$IFNDEF USE_CUSTOM_FASTMOVE}
PROCEDURE FastMove(VAR Destination, Source: TVectorsBase; n: TVectorsIndex);INLINE;
begin
  Move(Destination, Source, n * SizeOf(TVectorsBase));
end;
{$ENDIF USE_CUSTOM_FASTMOVE}
PROCEDURE GenerateError(c:Char);
  BEGIN
    WriteLn('Unit VectorsBaseVectors : Size mismatch in call to OPERATOR',c,
            '(x,y:TVectors):RESULT;');
    RunError(1);
  END;
OPERATOR +(x,y:TVectors)RESULT:TVectors;
  VAR
    i:TVectorsIndex;
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
OPERATOR -(x,y:TVectors)RESULT:TVectors;
  VAR
    i:TVectorsIndex;
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
OPERATOR *(x,y:TVectors)RESULT:TVectorsBase;
  VAR
    i:TVectorsIndex;
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
OPERATOR *(r:TVectorsBase;x:TVectors)RESULT:TVectors;
  VAR
    I:TVectorsIndex;
  BEGIN
    WITH RESULT DO
      BEGIN
        RESULT:=Vector(x.n);
	FOR i:=0 TO n DO
          Values[i]:=r*x.Values[i];
      END;
  END;
PROCEDURE Add(VAR x:TVectors;r:TVectorsBase;y:TVectors);
  VAR
    i:TVectorsIndex;
  BEGIN
    WITH x DO
      IF n=y.n 
      THEN
        FOR i:=0 TO n DO
	  Values[i]+=r*y.Values[i];
  END;
PROCEDURE Add(VAR x:TVectors;y:Tvectors);
  VAR
    i:TVectorsIndex;
  BEGIN
    WITH x DO
      IF n=y.n 
      THEN
        FOR i:=0 TO n DO
	  Values[i]+=y.Values[i];
  END;
FUNCTION Copy(x:TVectors):TVectors;
  BEGIN
    Copy:=Vector(x.n+1);
    WITH Copy DO
      FastMove(Values^,x.Values^,n+1);
  END;
FUNCTION Dim(x:TVectors):TVectorsIndex;
  BEGIN
    Dim:=x.n+1;
  END;
PROCEDURE KillVector(VAR x:TVectors);
  BEGIN
    WITH x DO
      BEGIN
        FreeMem(Values,n+1);
        n:=0;
      END;
  END;
FUNCTION Mean(x:TVectors):Real;
  VAR
    i:TVectorsIndex;
  BEGIN
    WITH x DO
      BEGIN
        Mean:=0;
	FOR i:=0 TO n DO
	  Mean+=Values[i];
	Mean/=n;
      END
  END;
FUNCTION Module(x:TVectors):Real;
  VAR
    i:TVectorsIndex;
  BEGIN
    {Module:=Sqrt(x*x);}
    Module:=0;
    WITH x DO
      FOR i:=0 TO n DO
        Module+=Values[i]*Values[i];
    Module:=Sqrt(Module);
  END;
PROCEDURE Mul(VAR x:TVectors;r:TVectorsBase);
  VAR
    i:TVectorsIndex;
  BEGIN
    WITH x DO
      FOR i:=0 to n DO
        Values[i]*=r;
  END;
PROCEDURE ReDim(VAR x:TVectors;n:TVectorsIndex);
  VAR
    t:TVectors;
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
PROCEDURE Sub(VAR x:TVectors;y:TVectors);
  VAR
    i:TVectorsIndex;
  BEGIN
    WITH x DO
      IF n=y.n
      THEN
        FOR i:=0 TO n DO
	  Values[i]-=y.Values[i];
  END;
FUNCTION SubVector(x:TVectors;n,i:TVectorsIndex):TVectors;
  BEGIN
    SubVector.n:=n-1;
    SubVector.Values:=x.Values+i;
  END;
FUNCTION Vector(n:TVectorsIndex):TVectors;
  BEGIN
    Vector.n:=n-1;
    Vector.Values:=GetMem(n*SizeOf(TVectorsBase));
  END;
{$ENDIF}
