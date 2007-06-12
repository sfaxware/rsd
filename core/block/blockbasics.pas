unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils; 

type
  TIPort = interface
  end;
  
  TIInputPort = interface(TIPort)
  end;

  TIOutputPort = interface(TIPort)
  end;
  
  TIBlock = interface
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const InputName: string): Integer;
    property Input[index: string]: TIInputPort;
    property Output[index: string]: TIInputPort;
  end;
  
  TCBlock = class(TIBlock)
  private
  public
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const InputName: string): Integer;
  end;

implementation

function TCBlock.GetInputQty: Integer;
begin
end;

function TCBlock.GetOutputQty: Integer;
begin
end;

function TCBlock.GetInputIdx(const InputName: string): Integer;
begin
end;

function TCBlock.GetOutputIdx(const InputName: string): Integer;
begin
end;

end.

