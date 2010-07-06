unit Sources;

{$mode objfpc}{$H+}

interface

uses
  Classes, Blocks;

type
  TSourceSeed = LongWord;
  TSource = class(TBlock)
  private
    FInitialSeed: TSourceSeed;
    FCurrentSeed: TSourceSeed;
    FAmplitude: TSourceSeed;
    procedure SetInitialSeed(Seed: TSourceSeed); virtual;
    procedure UpdateCurrentSeed; virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
    property CurrentSeed: TSourceSeed read FCurrentSeed;
  published
    Output: TOutputPort;
    property InitialSeed: TSourceSeed read FInitialSeed write SetInitialSeed;
    property Amplitude: TSourceSeed read FAmplitude write FAmplitude;
  end;

  TRandomSource = class(TSource)
  private
    mt: array of LongInt;
    mti: LongInt; // mti=MT19937N+1 means mt[] is not initialized
    procedure sgenrand_MT19937(seed: longint);
    function genrand_MT19937: longint;
    function random(l:longint): longint;
    function random(l:int64): int64;
    procedure SetInitialSeed(Seed: TSourceSeed); override;
    procedure UpdateCurrentSeed; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  SysUtils, BlockBasics;

procedure TSource.SetInitialSeed(Seed: TSourceSeed);
begin
  FInitialSeed := Seed;
  if Abs(Seed) > FAmplitude then begin
    FCurrentSeed := FAmplitude;
  end else begin
    FCurrentSeed := FInitialSeed;
  end;
end;

constructor TSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAmplitude := MaxUIntValue;
end;

procedure TSource.Execute;
begin
  UpdateCurrentSeed;
  Output.Push(FCurrentSeed);
end;

{----------------------------------------------------------------------
   Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator.

   What is Mersenne Twister?
   Mersenne Twister(MT) is a pseudorandom number generator developped by
   Makoto Matsumoto and Takuji Nishimura (alphabetical order) during
   1996-1997. MT has the following merits:
   It is designed with consideration on the flaws of various existing
   generators.
   Far longer period and far higher order of equidistribution than any
   other implemented generators. (It is proved that the period is 2^19937-1,
   and 623-dimensional equidistribution property is assured.)
   Fast generation. (Although it depends on the system, it is reported that
   MT is sometimes faster than the standard ANSI-C library in a system
   with pipeline and cache memory.)
   Efficient use of the memory. (The implemented C-code mt19937.c
   consumes only 624 words of working area.)

   home page
     http://www.math.keio.ac.jp/~matumoto/emt.html
   original c source
     http://www.math.keio.ac.jp/~nisimura/random/int/mt19937int.c

   Coded by Takuji Nishimura, considering the suggestions by
   Topher Cooper and Marc Rieffel in July-Aug. 1997.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later
   version.
   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Library General Public License for more details.
   You should have received a copy of the GNU Library General
   Public License along with this library; if not, write to the
   Free Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307  USA

   Copyright (C) 1997, 1999 Makoto Matsumoto and Takuji Nishimura.
   When you use this, send an email to: matumoto@math.keio.ac.jp
   with an appropriate reference to your work.

   REFERENCE
   M. Matsumoto and T. Nishimura,
   "Mersenne Twister: A 623-Dimensionally Equidistributed Uniform
   Pseudo-Random Number Generator",
   ACM Transactions on Modeling and Computer Simulation,
   Vol. 8, No. 1, January 1998, pp 3--30.


  Translated to OP and Delphi interface added by Roman Krejci (6.12.1999)

  http://www.rksolution.cz/delphi/tips.htm

  Revised 21.6.2000: Bug in the function RandInt_MT19937 fixed

  2003/10/26: adapted to use the improved intialisation mentioned at
  <http://www.math.keio.ac.jp/~matumoto/MT2002/emt19937ar.html> and
  removed the assembler code

 ----------------------------------------------------------------------}

{$R-} {range checking off}
{$Q-} {overflow checking off}

{ Period parameters }
const
  MT19937N=624;
  MT19937M=397;
  MT19937MATRIX_A  =$9908b0df;  // constant vector a
  MT19937UPPER_MASK=longint($80000000);  // most significant w-r bits
  MT19937LOWER_MASK=longint($7fffffff);  // least significant r bits

{ Tempering parameters }
  TEMPERING_MASK_B=longint($9d2c5680);
  TEMPERING_MASK_C=longint($efc60000);

{ Initializing the array with a seed }
procedure TRandomSource.sgenrand_MT19937(seed: longint);
var
  i: longint;
begin
  mt[0] := seed;
  for i := 1 to MT19937N - 1 do
    begin
      mt[i] := 1812433253 * (mt[i-1] xor (mt[i-1] shr 30)) + i;
      { See Knuth TAOCP Vol2. 3rd Ed. P.106 for multiplier. }
      { In the previous versions, MSBs of the seed affect   }
      { only MSBs of the array mt[].                        }
      { 2002/01/09 modified by Makoto Matsumoto             }
    end;
  mti := MT19937N;
end;

function TRandomSource.genrand_MT19937: longint;
const
  mag01 : array [0..1] of longint =(0, longint(MT19937MATRIX_A));
var
  y: longint;
  kk: longint;
begin
  if (mti >= MT19937N) { generate MT19937N longints at one time }
  then begin
     for kk:=0 to MT19937N-MT19937M-1 do begin
        y := (mt[kk] and MT19937UPPER_MASK) or (mt[kk+1] and MT19937LOWER_MASK);
        mt[kk] := mt[kk+MT19937M] xor (y shr 1) xor mag01[y and $00000001];
     end;
     for kk:= MT19937N-MT19937M to MT19937N-2 do begin
       y := (mt[kk] and MT19937UPPER_MASK) or (mt[kk+1] and MT19937LOWER_MASK);
       mt[kk] := mt[kk+(MT19937M-MT19937N)] xor (y shr 1) xor mag01[y and $00000001];
     end;
     y := (mt[MT19937N-1] and MT19937UPPER_MASK) or (mt[0] and MT19937LOWER_MASK);
     mt[MT19937N-1] := mt[MT19937M-1] xor (y shr 1) xor mag01[y and $00000001];
     mti := 0;
  end;
  y := mt[mti]; inc(mti);
  y := y xor (y shr 11);
  y := y xor (y shl 7)  and TEMPERING_MASK_B;
  y := y xor (y shl 15) and TEMPERING_MASK_C;
  y := y xor (y shr 18);
  Result := y;
end;


function TRandomSource.random(l:longint): longint;
begin
  { otherwise we can return values = l (JM) }
  if (l < 0) then
    inc(l);
  Result := longint((int64(cardinal(genrand_MT19937))*l) shr 32);
end;

function TRandomSource.random(l:int64): int64;
begin
  { always call random, so the random generator cycles (TP-compatible) (JM) }
  Result := int64((qword(cardinal(genrand_MT19937)) or ((qword(cardinal(genrand_MT19937)) shl 32))) and $7fffffffffffffff);
  if (l<>0) then
    Result := Result mod l
  else
    Result := 0;
end;

procedure TRandomSource.SetInitialSeed(Seed: TSourceSeed);
begin
  //WriteLn(FuncB('TRandomSource.SetInitialSeed'));
  SetLength(mt, MT19937N);
  inherited SetInitialSeed(Seed);
  sgenrand_MT19937(FInitialSeed);   // default initial seed is used
  //WriteLn(FuncC('TRandomSource.SetInitialSeed'), 'Seed = ', Seed);
  //WriteLn(FuncB('TRandomSource.SetInitialSeed'));
end;

procedure TRandomSource.UpdateCurrentSeed;
begin
  FCurrentSeed := random(FAmplitude);
end;

constructor TRandomSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitialSeed := Trunc(Now * 123456789);
end;

initialization
  RegisterClass(TRandomSource);
  RegisterClass(TSource);

finalization

end.

