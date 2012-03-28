unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes;

type
  TIConnector = interface;

  TIDevice = interface
    function GetName:string;
    procedure Execute;
    property DeviceName: string read GetName;
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

  TCPort = class(TComponent, TIPort)
  private
    FConnector: TIConnector;
    FCaption: string;
    function GetConnector: TIConnector;
    function GetName: string;
    procedure SetConnector(Value: TIConnector);
  protected
    procedure ValidateContainer(AComponent: TComponent); override;
  public
    procedure Execute; virtual; abstract;
  published
    property Caption: string read FCaption write FCaption;
  end;

  TCInputPort = class(TCPort, TIInputPort)
  end;
  
  TCOutputPort = class(TCPort, TIOutputPort)
  end;

  TCBlock = class(TComponent, TIBlock)
  private
    FBlocks: array of TIBlock;
    FInputPorts: array of TIInputPort;
    FOutputPorts: array of TIOutputPort;
    FInputComponentCount: Integer;
    FOutputComponentCount: Integer;
    FCaption: string;
    function GetName: string;
  protected
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const OutputName: string): Integer;
    procedure Execute; virtual;
  published
    property Caption: string read FCaption write FCaption;
  end;

  TCConnector = class(TComponent,TIConnector)
  private
    FCaption: string;
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
  Connector := TCConnector.Create(nil);
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
  Result := FCaption;
end;

procedure TCPort.SetConnector(Value: TIConnector);
begin
  FConnector := Value;
end;

procedure TCPort.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TCBlock then with AComponent as TCBlock do begin
    ValidateInsert(Self);
  end;
end;

constructor TCBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

function TCBlock.GetInputQty: Integer;
begin
  Result := Length(FInputPorts);
end;

function TCBlock.GetOutputQty: Integer;
begin
  Result := Length(FOutputPorts);
end;

function TCBlock.GetInputIdx(const InputName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(FInputPorts) to High(FInputPorts) do begin
    if FInputPorts[i].DeviceName = InputName then begin
      Exit(i);
    end;
  end;
end;

function TCBlock.GetOutputIdx(const OutputName: string): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(FOutputPorts) to High(FOutputPorts) do begin
    if FOutputPorts[i].DeviceName = OutputName then begin
      Exit(i);
    end;
  end;
end;

function TCBlock.GetName: string;
begin
  Result := FCaption;
end;

procedure TCBlock.ValidateInsert(AComponent: TComponent);
begin
  if AComponent is TCInputPort then begin
    FInputPorts[FInputComponentCount] := AComponent as TCInputPort;
    FInputComponentCount += 1;
  end else if AComponent is TCOutputPort then begin
    FOutputPorts[FOutputComponentCount] := AComponent as TCOutputPort;
    FOutputComponentCount += 1;
  end;
end;

procedure TCBlock.Execute;
var
  i: Integer;
begin
  //WriteLn('TCBlock.Execute : Name = ', FCaption);
  for i := Low(FInputPorts) to High(FInputPorts) do with FInputPorts[i] do begin
    Execute;
  end;
  for i := Low(FBlocks) to High(FBlocks) do with FBlocks[i] do begin
    Execute;
  end;
  for i := Low(FOutputPorts) to High(FOutputPorts) do with FOutputPorts[i] do begin
    Execute;
  end;
end;

function TCConnector.GetName:string;
begin
  Result := FCaption;
end;

procedure TCConnector.Connect(Output:TIOutputPort; Input: TIInputPort);
begin
  Output.Connector := Self;
  Input.Connector := Self;
end;

end.

