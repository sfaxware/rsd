{*****************************************************************************}
{ File                   : lmsfilter.pas
  Author                 : Mazen NEIFER
  Creation date          : 20/10/2000
  Last modification date : 28/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT LMSfilter;
INTERFACE
USES RealVectors, RealMatrix;
FUNCTION FastConv(x,h:TVector):TVectorBase;
{This function makes a very fast convolution of two Vector. The first "x" is
supposed to be the signal to be convoluted and the second "h" is supposed to be
the filter to convoluate with. It supposes implicetly that Dim(h) is below or
equal to than Dim(x) but it doesn't make a test <SMALL>(it is a fast rooutine,
it do its job without testing entries)</SMALL>. Please notice that it gives one
value only which corresponds to the last output of the filter when exited by 
the given signal. Code <CODE>y:=FastConv(x,h)</CODE> results in y=h*x(Dim(x)).}
FUNCTION Wiener(VAR h:TVector;x,d:TVector;mu:Real):TVector;
{This function applies an adaptative Wiener filter to signal "x", desired to be
"d" and gives "y:=h[n]*x[n]" as result. The filter is initialize using h, and
is corrected depending on parameter "mu". The filter is finally returned in "h"
so you have to ensure you will no more need this vector before you call this 
function and you have to destroy it once you don't need it any more.}
PROCEDURE LMS(VAR y,e:Real;VAR h:TVector;x:TVector;d,mu:Real);
IMPLEMENTATION
FUNCTION FastConv(x,h:TVector):TVectorBase;
  VAR
    i:Word;
  BEGIN
    WITH h DO
      BEGIN
        FastConv:=0;
        FOR i:=0 to n DO
          FastConv+=Values[i]*x.Values[n-i];
      END;
  END;
PROCEDURE LMS(VAR y,e:Real;VAR h:TVector;x:TVector;d,mu:Real);
  BEGIN
    y:=FastConv(h,x);{We begin by estimating the "x" at current instant!}
    e:=d-y;{Then we calculate the error. Estimation doesn't give us the correct
            value but just a very close value for good estimations}
    Acc(h,2*mu*e,x);{"h:=h+2*mu*e*x" : Then we applay LMS algorithme to correct
                   the "h" value using error. We try to not use operators du to
		   some memory problems with function that return pointer when
		   called one as a transmitted parameter to an other. for more
		   details see <A HREF="realVector.pas.html">realVector</A>.}
  END;
FUNCTION Wiener(VAR h:TVector;x,d:TVector;mu:Real):TVector;
  VAR
    i:Word;
    e:TVectorBase;
  BEGIN
    Wiener:=Vector(Dim(x)-Dim(h));
    WITH Wiener DO
      FOR i:=0 TO n DO
        LMS(Values[i],e,h,SubVector(x,h.n+1,i),d.Values[h.n+i+1],mu);
  END;
END.
