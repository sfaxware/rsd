//*****************************************************************************
// File                   : integervectors.pas
// Author                 : Mazen NEIFER
// Creation date          : 2000-10-13
// Last modification date : 2010-07-20
// Licence                : GPL
// Bug report             : mazen.neifer@supaero.org
//*****************************************************************************
unit IntegerVectors;
{$MODE OBJFPC}
interface
{$DEFINE INTERFACE}

uses
  Reals;

type
  TVectorBase = Integer;
  TVectorIndex = Integer;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.}

{$I vectors.inc}

implementation
{$UNDEF INTERFACE}

{$I vectors.inc}
end.
