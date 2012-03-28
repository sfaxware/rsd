unit FifoBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils; 

type
  TIFifo = interface
    function Push(p:Pointer):Boolean;
    function Pop:Pointer;
    function GetPendingQty:Integer;
    function GetAvailableQty:Integer;
  end;

  TCFifo = class(TIFifo)
  private
    Sz:Integer;
    Rd:Integer;
    Wr:Integer;
    Buffer:Array Of Pointer;
  public
    constructor Init(Size:Integer);
    function Push(p:Pointer):Boolean;
    function Pop:Pointer;
    function GetPendingQty:Integer;
    function GetAvailableQty:Integer;
  end;
  
implementation

constructor TCFifo.Init(Size:Integer);
var
  i:Integer;
begin
  Sz := Size;
  SetLength(Buffer, Sz);
  Rd := 0;
  Wr := 0;
  For i := 0 to Sz - 1 Do
    Buffer[i] := Nil;
end;

function TCFifo.Push(p:Pointer):Boolean;
var
  nWr:Integer;
begin
  nWr := Wr + 1;
  if nWr >= Sz then
     nWr := 0;
  if nWr = Rd then
    Result := False
  else if Buffer[nWr] <> Nil then
    Result := False //TODO: should raise an exception here
  else begin
    Buffer[Wr] := p;
    Wr := nWr;
    Result := True;
  end;
end;

function TCFifo.Pop:Pointer;
begin
  if Rd = Wr then
    Result := Nil
  else begin
    Result := Buffer[Rd];
    Buffer[Rd] := Nil;
    Rd := Rd + 1;
    if Rd >= Sz then
      Rd := 0;
  end;
end;

function TCFifo.GetPendingQty:Integer;
begin
  Result := Wr - Rd;
  if Result < 0 then
    Result := Result + Sz;
end;

function TCFifo.GetAvailableQty:Integer;
begin
  Result := Sz - GetPendingQty - 1;
end;

end.

