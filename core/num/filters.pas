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
USES Complex;
PROCEDURE FIR_Word(N,M:Word;VAR e{:ARRAY[0..N-1]OF Word},
                                h{:ARRAY[0..M-1]OF Word});
PROCEDURE FIR_DWord(N,M:Word;VAR e{:ARRAY[0..N-1]OF DWord},
                                 h{:ARRAY[0..M-1]OF DWord});		
PROCEDURE FIR_Real(N,M:Word;VAR e{:ARRAY[0..N-1]OF TReal},
                                h{:ARRAY[0..M-1]OF TReal});
{This procedure applaies the h filter to the e signal }
IMPLEMENTATION
{$ASMMODE INTEL}
FUNCTION min(i,j:LongInt):LongInt;INLINE;ASSEMBLER;
  ASM
   MOV  EAX,i
   MOV  EDX,j
   CMP  EAX,EDX
   JB   @1
   MOV  EAX,EDX
@1: 
  END;
PROCEDURE FIR_Word(N,M:Word;VAR e,h);
  VAR
    i,j:Word;
    E_,H_:PWord;
  BEGIN
    E_:=@e;
    H_:=@h;
    FOR i:=N-1 DOWNTO 0 DO
      BEGIN
	E_[i]*=H_[0];
        FOR j:=1 TO min(M-1,i) DO
	  E_[i]+=H_[j]*E_[i-j];
      END;
  END;
PROCEDURE FIR_DWord(N,M:Word;VAR e,h);
  VAR
    i,j:Word;
    E_,H_:PDWord;
  BEGIN
    E_:=@e;
    H_:=@h;
    FOR i:=N-1 DOWNTO 0 DO
      BEGIN
	E_[i]*=H_[0];
        FOR j:=1 TO min(M-1,i) DO
	  E_[i]+=H_[j]*E_[i-j];
      END;
  END;
PROCEDURE FIR_Real(N,M:Word;VAR e,h);
  VAR
    i,j:Word;
    E_,H_:PReal;
  BEGIN
    E_:=@e;
    H_:=@h;
    FOR i:=N-1 DOWNTO 0 DO
      BEGIN
	E_[i]*=H_[0];
        FOR j:=1 TO min(M-1,i) DO
	  E_[i]+=H_[j]*E_[i-j];
      END;
  END;
END .
