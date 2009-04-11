unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils; 

type
  TIConnector = interface;

  TIPort = interface
  	function GetConnector: TIConnector;
	procedure SetConnector(Value: TIConnector);
    property Connector: TIConnector read GetConnector write SetConnector;
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
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
    property OutputPort: TIOutputPort;
    property InputPort: TIInputPort;
  end;

  TCPort = class(TIPort)
  private
    FConnector: TIConnector;
  	function GetConnector: TIConnector;
	procedure SetConnector(Value: TIConnector);
  end;

  TCInputPort = class(TCPort, TIInputPort)
  end;
  
  TCOutputPort = class(TCPort, TIOutputPort)
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

  TCConnector = class(TIConnector)
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
  end;

procedure ConnectPorts(Output: TIOutputPort; Input:TIInputPort);

implementation

procedure ConnectPorts(Output: TIOutputPort; Input:TIInputPort);
var
  Connector: TCConnector;
begin
  Connector := TCConnector.Create;
  with Connector do begin
    Connect(Output, Input);
  end;
end;

function TCPort.GetConnector: TIConnector;
begin
  Result := FConnector;
end;

procedure TCPort.SetConnector(Value: TIConnector);
begin
  FConnector := Value;
end;

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

procedure TCConnector.Connect(Output:TIOutputPort; Input: TIInputPort);
begin
  Output.Connector := Self;
  Input.Connector := Self;
end;

end.

