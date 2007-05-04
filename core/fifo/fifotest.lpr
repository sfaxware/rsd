program FifoUnitTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, fifobasics;

procedure TestFifo;
var
  Fifo:TCFifo;
  Sz:Integer;
  i:Integer;
begin
  Sz := Random(1024);
  WriteLn('Allocating Fifo with Sz = ', Sz);
  Fifo := TCFifo.Init(Sz);
  With Fifo Do begin
    For i := 1 to Random(Sz) Do
      Push(Pointer(i));
    WriteLn(GetPendingQty);
    WriteLn(GetAvailableQty);
    For i := 1 to Random(Sz) Do
      Write(Integer(Pop):4);
    WriteLn;
    WriteLn(GetPendingQty);
    WriteLn(GetAvailableQty);
  end;
end;

begin
  Randomize;
  TestFifo;
end.

