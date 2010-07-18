{*****************************************************************************}
{ File                   : realvectors.pas
  Author                 : Mazen NEIFER
  Creation date          : 13/10/2000
  Last modification date : 31/10/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT RealVectors;
{$MODE FPC}
INTERFACE
{$DEFINE INTERFACE}
TYPE
  TReal = Real;
  TVectorBase = TReal;{If you want more precision you can chage to extended/...}
  TVectorIndex = DWord;{This allows us to use huge vectors}
  {The upper part (just up to this line) is used to define the types that will
  be used to generate executable code. All pascal code that is written before
  this line will compile with more than base type. Independent implementation
  is written in the file <A HREF="vectors.inc.html">vectors.inc</A> to provide
  an type independant implementation.} 
  {$I vectors.inc} {See code of <A HREF="vectors.inc.html#interface">vectors.inc</A>.}
IMPLEMENTATION
{$UNDEF INTERFACE}
{$ASMMODE INTEL}
PROCEDURE FastMove(VAR Destination,Source;n:DWord);INLINE;ASSEMBLER;
  ASM
    MOV   ECX,n
    SAL   ECX,1 
    MOV   EDI,Destination
    MOV   ESI,Source
    CLD
    REP   MOVSD
  END['ECX','EDI','ESI'];
{$DEFINE USE_CUSTOM_FASTMOVE}
{$I vectors.inc} {See code of <A HREF="vectors.inc.html#implementation">vectors.inc</A>.}
END.
