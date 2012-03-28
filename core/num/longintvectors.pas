{*****************************************************************************}
{ File                   : longintvectors.pas
  Author                 : Mazen NEIFER
  Creation date          : 13/10/2000
  Last modification date : 22/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT LongIntVectors;
{$MODE FPC}
INTERFACE
{$DEFINE Interface}
TYPE
  TVectorsBase=LongInt;
  TVectorsIndex=LongInt;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.}
{$I vectors.inc}
IMPLEMENTATION
{$UNDEF Interface}
{$I vectors.inc}
END.
