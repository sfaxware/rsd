//*****************************************************************************
// File                   : realmatrix.pas
// Author                 : Mazen NEIFER
// Creation date          : 2000-10-13
// Last modification date : 2010-07-18
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
unit RealMatrix;
{$MODE OBJFPC}
interface
{$DEFINE INTERFACE}

uses
  Reals;

type
  TMatrixBase = TReal;{If you want more precision you can chage to extended/...}
  TMatrixIndex = DWord;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.}

{$INCLUDE matrix.inc} {See code of <A HREF="vectors.inc.html#interface">vectors.inc</A>.}

implementation
{$UNDEF INTERFACE}

{$INCLUDE matrix.inc}

end.
