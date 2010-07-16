unit FifoBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils; 

type
  TIFifo = interface
    function Push(p: Pointer): Boolean;
    function Pop(out p: Pointer): Boolean;
    function GetPendingQty:Integer;
    function GetAvailableQty:Integer;
    property RdIdx: Integer;
    property WrIdx: Integer;
    property Size: Integer;
  end;

  TCFifo = class(TIFifo)
  private
    Sz:Integer;
    Rd:Integer;
    Wr:Integer;
    Buffer:Array Of Pointer;
  public
    constructor Create(Size: Integer);
    function Push(p: Pointer): Boolean;
    function Pop(out p: Pointer): Boolean;
    function GetPendingQty:Integer;
    function GetAvailableQty:Integer;
    destructor Destroy; override;
    property RdIdx: Integer read Rd;
    property WrIdx: Integer read Wr;
    property Size: Integer read Sz;
  end;
  
implementation

constructor TCFifo.Create(Size:Integer);
var
  i:Integer;
begin
  Sz := Size + 1;
  {Usally allocate Sz + 1 elements as fifo is full when Wr = Rd + 1 mod Sz}
  SetLength(Buffer, Sz + 1);
  Rd := 0;
  Wr := 0;
  For i := 0 to Sz Do
    Buffer[i] := Nil;
end;

destructor TCFifo.Destroy;
begin
  SetLength(Buffer, 0);
end;

function TCFifo.Push(p:Pointer):Boolean;
var
  nWr:Integer;
begin
  nWr := Wr + 1;
  if nWr > Sz then
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

function TCFifo.Pop(out p: Pointer): Boolean;
begin
  if Rd = Wr then
    Result := False
  else begin
    p := Buffer[Rd];
    Buffer[Rd] := Nil;
    Rd := Rd + 1;
    if Rd > Sz then
      Rd := 0;
    Result := True;
  end;
end;

function TCFifo.GetPendingQty:Integer;
begin
  Result := Wr - Rd;
  if Result < 0 then
    Result := Result + Sz + 1;
end;

function TCFifo.GetAvailableQty:Integer;
begin
  Result := Sz - GetPendingQty - 1;
end;

end.

