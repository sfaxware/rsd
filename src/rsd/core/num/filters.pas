{*****************************************************************************}
{ File                   : complex.pas
  Author                 : Mazen NEIFER
  Creation date          : 29/09/2000
  Last modification date : 29/09/2000
  Licence                : GPL                                                
  Bug report             : mazen_nefer@ayna.com                               }
{*****************************************************************************}
UNIT Filters;
INTERFACE
procedure FIR_Word(N,M:Word;var e{:ARRAY[0..N-1]OF Word},
                                h{:ARRAY[0..M-1]OF Word});
procedure FIR_DWord(N,M:Word;var e{:ARRAY[0..N-1]OF DWord},
                                 h{:ARRAY[0..M-1]OF DWord});		
procedure FIR_Real(N,M:Word;var e{:ARRAY[0..N-1]OF TReal},
                                h{:ARRAY[0..M-1]OF TReal});
{This procedure applaies the h filter to the e signal }
IMPLEMENTATION

uses
  Reals;

{$ASMMODE INTEL}
FUNCTION min(i,j:LongInt):LongInt;INLINE;ASSEMBLER;
ASM
   MOV  EAX,i
   MOV  EDX,j
   CMP  EAX,EDX
   JB   @1
   MOV  EAX,EDX
@1: 
end;

procedure FIR_Word(N,M:Word;var e,h);
var
  i,j:Word;
  E_,H_:PWord;
begin
  E_:=@e;
  H_:=@h;
  for i:=N-1 downto 0 do begin
	  E_[i]*=H_[0];
    for j:=1 TO min(M-1,i) do
	    E_[i]+=H_[j]*E_[i-j];
  end;
end;

procedure FIR_DWord(N,M:Word;var e,h);
var
  i,j:Word;
  E_,H_:PDWord;
begin
  E_:=@e;
  H_:=@h;
  for i:=N-1 downto 0 do begin
  	E_[i]*=H_[0];
    for j:=1 TO min(M-1,i) do
	    E_[i]+=H_[j]*E_[i-j];
  end;
end;

procedure FIR_Real(N,M:Word;var e,h);
var
  i,j:Word;
  E_,H_:PReal;
begin
  E_:=@e;
  H_:=@h;
  for i:=N-1 downto 0 do begin
  	E_[i]*=H_[0];
    for j:=1 TO min(M-1,i) do
	    E_[i]+=H_[j]*E_[i-j];
  end;
end;

end .
