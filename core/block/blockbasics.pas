unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

type
  TIConnector = interface;

  TIDevice = interface
    function GetName:string;
    procedure Execute;
    property Name: string read GetName;
  end;

  TIPort = interface(TIDevice)
    function GetConnector: TIConnector;
    procedure SetConnector(Value: TIConnector);
    property Connector: TIConnector read GetConnector write SetConnector;
  end;
  
  TIInputPort = interface(TIPort)
  end;

  TIOutputPort = interface(TIPort)
  end;
  
  TIBlock = interface(TIDevice)
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const InputName: string): Integer;
    property Input[index: string]: TIInputPort;
    property Output[index: string]: TIInputPort;
  end;

  TIConnector = interface(TIDevice)
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
    property OutputPort: TIOutputPort;
    property InputPort: TIInputPort;
  end;

  TCPort = class(TIPort)
  private
    FConnector: TIConnector;
    FName: string;
    function GetConnector: TIConnector;
    function GetName: string;
    procedure SetConnector(Value: TIConnector);
    procedure Execute; virtual; abstract;
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
    FName: string;
    function GetName: string;
  public
    constructor Create(Name: string);
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const OutputName: string): Integer;
    procedure Execute; virtual;
  end;

  TCConnector = class(TIConnector)
  private
    FName: string;
    function GetName: string;
  public
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
    procedure Execute; virtual; abstract;
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

function TCPort.GetName: string;
begin
  Result := FName;
end;

procedure TCPort.SetConnector(Value: TIConnector);
begin
  FConnector := Value;
end;

constructor TCBlock.Create(Name: string);
begin
  FName := Name
end;

function TCBlock.GetInputQty: Integer;
begin
  Result := Length(InputPorts);
end;

function TCBlock.GetOutputQty: Integer;
begin
  Result := Length(OutputPorts);
end;

function TCBlock.GetInputIdx(const InputName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(InputPorts) to High(InputPorts) do begin
    if InputPorts[i].Name = InputName then begin
      Exit(i);
    end;
  end;
end;

function TCBlock.GetOutputIdx(const OutputName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(OutputPorts) to High(OutputPorts) do begin
    if OutputPorts[i].Name = OutputName then begin
      Exit(i);
    end;
  end;
end;

function TCBlock.GetName: string;
begin
  Result := Fname;
end;

procedure TCBlock.Execute;
var
  i: Integer;
begin
  WriteLn('TCBlock.Execute : Name = ', FName);
  for i := Low(InputPorts) to High(InputPorts) do with InputPorts[i] do begin
    Execute;
  end;
  for i := Low(Blocks) to High(Blocks) do with Blocks[i] do begin
    Execute;
  end;
  for i := Low(OutputPorts) to High(OutputPorts) do with OutputPorts[i] do begin
    Execute;
  end;
end;

function TCConnector.GetName:string;
begin
  Result := FName;
end;

procedure TCConnector.Connect(Output:TIOutputPort; Input: TIInputPort);
begin
  Output.Connector := Self;
  Input.Connector := Self;
end;

end.

