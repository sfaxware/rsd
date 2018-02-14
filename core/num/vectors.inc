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
type
  PVectorBase = ^TVectorBase;{Base type must be defined before including this}
  PVector = ^TVector;{different types. It is possible to use a generic code}
  TVector = record{then compile it with different base types (Real, LongInt...}
    n: TVectorIndex;{n=MaxOffset=Dim(vector)-1);}
    Values: PVectorBase;{Points to the first entry of the stored vales table}
  end;

operator +(x, y: TVector): TVector;{Use at your own risk}
operator -(x, y: TVector): TVector;{Those functions creates ther results}
operator *(x, y: TVector): TVectorBase;{and it is up to you to take care}
operator *(r: TVectorBase; x: TVector): TVector;{of available memory and}
procedure Acc(var x: TVector; r: TVectorBase; y: TVector);{to free them.}
{This procedure adds to vector "x" a vector "y" mutiplied by a scalar "r"
result is stored in "x". It is equavalent to x:=x+r*y;. This
procedure is just better than its equivalent given code because it is faster
and it saves memory. See comments of next function for more details.}
procedure Add(var x:TVector; y: TVector);
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
function Copy(x: TVector): TVector;
{This function creates a new instance of a Vector. It deffers from "x:=y" in
that this last call causes the compiler to generate a code that make "x" point
to the same value array as y. It is just same as assigning pointers. When using
"x:=Copy(y)" the function "Copy" will creates a new array and copy in it the
values of the array pointed by "y". This is particulary useful to make
additions. The following codes have the same effect "z:=x+y" and
"z:=Copy(x);Add(z,y)"}
function  Dim(x: TVector): TVectorIndex;
{This function just return the dimention of the vector. It doesn't return the
size of the variable and so it is different from "SizeOf(x)". The dimension of
a vector is stored in a the "x.n" field and is equal of the array size. Please
notice that it is better to use "Dim(x)" than "x.n" as the way of coding
"TVector" may be changed and that "Dim(x)" will keep code comptibility.}
procedure KillVector(var x: TVector);
{This procedure kills a vector by restoring its memory in the heap and assiging
it (0,NIL). Please notice that if you assigned a vector y using "y:=x", killing
"x" or "y" kills the two Vector. There is however a little problem as calling
"KillVector(x)" after this kind off assignement will makes "y" have an erronous
values as its dimension is false and it "Values" points on an unavailable part
of the memory.}
function Mean(x: TVector): TReal;
{This function returns the mean value of the vector. It is just the sommation
of all Vector coordinates divided by the dimension}
function Module(x: TVector): TReal;
{"Module" or "Power" allow you to caculate the mean of the vector of same
dimension as "x" and which components are equal to the squre of those of "x".
This quantity is the module of the vector that is called the signal power or
the signal energy if the vector represents a signal}
procedure Mul(var x: TVector; r: TVectorBase);
{This function do the multiplication of vector "x" by the scalar "r". The
result is a vector which every component is the product of the corresponding
"x" component by "r". It is equivalent to "x:=r*x" expect this function doesn't
create a new dinamical variable and doesn't cause loss of heap.}
procedure ReDim(var x: TVector; n: TVectorIndex);
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
procedure Sub(var x: TVector; y: TVector);
{This function allow you to make vector substraction. It returns in "x" the
Vector that is the diffrence between "x" and "y". The code "Sub(x,y)" is
almost equivalent to "x:=x-y" expect, in the first case, no new value table is
created. In the latest, if no precaution was made to save "x" value, it will
cause the program to have a dinamical variable allocated from the heap but that
is no more pointed and thus can't be deallocated until the program exits. It
works same as "Add" expect it substract Vector.}
function SubVector(x: TVector; Idx,Qty: TVectorIndex): TVector;
{This function returns a vector of dimention n, wich first entry is the 
ith entry of "x". It just assigns a correct value to "x.Values" but
didn't allocate memory and copy values on it. So it you can not remove it using
"KillVector" function. }
function Vector(n: TVectorIndex): TVector;
{This function creates a new vector by allocating enoth memory to store n
values and returns a vector with dimension equal to "n". Please note that you
need to call this function only if you will assign your vector value by value
otherwise all functions that returns result of type "TVector" uses it to
create there return variables. This causes a restriction in using vector result
function as they may be used only this way "y:=f(x...)". Once you didn't need
"y" any more you need to delete it from the heap yourself.}
{Implementation part of real/longint/...Vector.pas}
{$ELSE INTERFACE}
{$IFNDEF USE_CUSTOM_FASTMOVE}
procedure FastMove(var Destination, Source: TVectorBase; n: TVectorIndex);inline;
begin
  Move(Destination, Source, n * SizeOf(TVectorBase));
end;
{$ENDIF USE_CUSTOM_FASTMOVE}

procedure GenerateError(c: Char);
begin
  WriteLn('Unit VectorBaseVector : Size mismatch in call to operator',c,
          '(x, y: TVector);');
  RunError(1);
end;

operator +(x, y: TVector): TVector;
var
  i: TVectorIndex;
begin
  if x.n = y.n then with Result do begin
	  Result := Vector(x.n);
    for i:=0 to n do begin
	    Values[i] := x.Values[i] + y.Values[i];
    end;
  end else begin
    GenerateError('+');
  end;
end;

operator -(x, y: TVector): TVector;
var
  i: TVectorIndex;
begin
  if x.n = y.n then with Result do begin
	  Result := Vector(x.n);
    for i:=0 to n do begin
	    Values[i] := x.Values[i] - y.Values[i];
    end;
  end else begin
      GenerateError('-');
  end;
end;

operator *(x, y: TVector): TVectorBase;
var
  i: TVectorIndex;
begin
  if x.n = y.n then begin
    Result := 0;
    for i := 0 to x.n do begin
      Result += x.Values[i] * y.Values[i];
    end
  end else begin
    GenerateError('*');
  end;
end;

operator *(r: TVectorBase; x: TVector): TVector;
var
  I: TVectorIndex;
begin
  with Result do begin
    Result := Vector(x.n);
	  for i:=0 to n do begin
      Values[i] := r * x.Values[i];
    end;
  end;
end;

procedure Acc(var x: TVector; r: TVectorBase; y: TVector);
var
  i: TVectorIndex;
begin
  with x do begin
    if n = y.n then begin
      for i := 0 to n do begin
  	    Values[i] += r * y.Values[i];
      end;
    end;
  end;
end;

procedure Add(var x: TVector; y: TVector);
var
  i: TVectorIndex;
begin
  with x do begin
    if n = y.n then begin
      for i := 0 to n do begin
  	    Values[i]+=y.Values[i];
      end;
    end;
  end;
end;

function Copy(x: TVector): TVector;
begin
  Result := Vector(x.n + 1);
  with Result do begin
    FastMove(Values^, x.Values^, n + 1);
  end;
end;

function Dim(x: TVector): TVectorIndex;
begin
  Dim := x.n + 1;
end;

procedure KillVector(var x: TVector);
begin
  with x do begin
    FreeMem(Values,n + 1);
    n := 0;
  end;
end;

function Mean(x: TVector): TReal;
var
  i: TVectorIndex;
begin
  with x do begin
    Result := 0;
	  for i := 0 to n do begin
	    Result += Sqr(Values[i]);
    end;
  	Result /= n;
  end;
end;

function Module(x: TVector): TReal;
var
  i: TVectorIndex;
begin
  {Module:=Sqrt(x .* x);}
  Result := 0;
  with x do begin
    for i := 0 to n do begin
      Result += Sqr(Values[i]);
    end;
  end;
  Result := Sqrt(Result);
end;

procedure Mul(var x: TVector; r: TVectorBase);
var
  i: TVectorIndex;
begin
  with x do begin
    for i := 0 to n do begin
      Values[i] *= r;
    end;
  end;
end;

procedure ReDim(var x: TVector; n: TVectorIndex);
var
  t: TVector;
begin
  if n < x.n then begin
    FreeMem(x.Values, x.n - n);
  end else begin
    t := x;
  	x := Vector(n);
  	FastMove(x.Values^, t.Values^, n);
    KillVector(t);
  end;
end;

procedure Sub(var x: TVector; y: TVector);
var
  i: TVectorIndex;
begin
  with x do begin
    if n = y.n then begin
      for i := 0 to n do begin
    	  Values[i]-=y.Values[i];
      end;
    end;
  end;
end;

function SubVector(x: TVector; Idx, Qty: TVectorIndex): TVector;
begin
  Result := Vector(Qty);
  with Result do begin
    FastMove(Values^, x.Values[Idx], Qty);
  end;
end;

function Vector(n: TVectorIndex): TVector;
begin
  Vector.n := n - 1;
  Vector.Values := GetMem(n * SizeOf(TVectorBase));
end;
{$ENDIF INTERFACE}
