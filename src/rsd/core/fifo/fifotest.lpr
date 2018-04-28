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
  i, j:Integer;
begin
  Sz := Random(1024);
  WriteLn('Allocating Fifo with Sz = ', Sz);
  Fifo := TCFifo.Create(Sz);
  WriteLn('Pushing entries');
  With Fifo Do begin
    For i := 1 to Random(Sz) Do
      Push(Pointer(i));
    WriteLn('Number of pending entries = ', GetPendingQty);
    WriteLn('Number of available entries = ', GetAvailableQty);
    WriteLn('Poping elements');
    For i := 1 to Random(Sz) Do
      Pop(Pointer(j));
      if Integer(j) <> i then
        WriteLn('Error @ i = ', i);
    WriteLn('Number of pending entries = ', GetPendingQty);
    WriteLn('Number of available entries = ', GetAvailableQty);
    WriteLn('Saturating FIFO');
    For i := 1 to Sz Do
      if not Push(Pointer(i)) then
        WriteLn('Can not push element i = ', i);
    WriteLn('Number of pending entries = ', GetPendingQty);
    WriteLn('Number of available entries = ', GetAvailableQty);
  end;
  Fifo.Destroy;
end;

begin
  Randomize;
  TestFifo;
end.

