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

  TIConnector = interface
    property OutputPort: TIOutputPort;
    property InputPort: TIInputPort;
  end;

  TCInputPort = class(TIInputPort)
  end;
  
  TCOutputPort = class(TIOutputPort)
  end;

  TCBlock = class(TIBlock)
  private
    Blocks: array of TIBlock;
    InputPorts: array of TIInputPort;
    OutputPorts: array of TIOutputPort;
  public
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const InputName: string): Integer;
    procedure Run;
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

procedure TCBlock.Run;
begin
  WriteLn('TCBlock.Run');
end;

end.

