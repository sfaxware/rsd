{*****************************************************************************}
{ File                   : realmatrix.pas
  Author                 : Mazen NEIFER
  Creation date          : 13/10/2000
  Last modification date : 13/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT RealMatrix;
{$MODE FPC}
INTERFACE
{$DEFINE INTERFACE}
TYPE
  TMatrixBase=Real;{If you want more precision you can chage to extended/...}
  TMatrixIndex=DWord;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.}
  {$I matrix.inc} {See code of <A HREF="vectors.inc.html#interface">vectors.inc</A>.}
IMPLEMENTATION
{$UNDEF INTERFACE}
{$I matrix.inc}
END.
