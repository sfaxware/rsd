//*****************************************************************************
// File                   : realvectors.pas
// Author                 : Mazen NEIFER
// Creation date          : 2010-07-18
// Last modification date : 2010-07-18
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
unit ComplexVectors;
{$MODE FPC}
interface
{$DEFINE INTERFACE}

uses
  Reals, Complex;

type
  TVectorBase=TComplex;{If you want more precision you can chage to extended/...}
  TVectorIndex=DWord;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.} 

{$I vectors.inc} {See code of <A HREF="vectors.inc.html#interface">vectors.inc</A>.}

implementation
{$UNDEF INTERFACE}

{$I vectors.inc} {See code of <A HREF="vectors.inc.html#implementation">vectors.inc</A>.}
end.
