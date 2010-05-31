unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes;

type
  TIConnector = interface;

  TIDevice = interface
    function GetName:string;
    property DeviceName: string read GetName;
  end;

  TIPort = interface(TIDevice)
    function GetConnector: TIConnector;
    procedure SetConnector(Value: TIConnector);
    property Connector: TIConnector read GetConnector write SetConnector;
  end;
  
  TIInputPort = interface(TIPort)
    function GetIsEmpty: Boolean;
    property IsEmpty: Boolean read GetIsEmpty;
  end;

  TIOutputPort = interface(TIPort)
    function GetIsFull: Boolean;
    property IsFull: Boolean read GetIsFull;
  end;
  
  TIBlock = interface(TIDevice)
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const InputName: string): Integer;
    procedure Execute;
    property Input[index: string]: TIInputPort;
    property Output[index: string]: TIInputPort;
  end;

  TIConnector = interface(TIDevice)
    function GetIsEmpty: Boolean;
    function GetIsFull: Boolean;
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
    property OutputPort: TIOutputPort;
    property InputPort: TIInputPort;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
  end;

  TCDevice = class(TComponent, TIDevice)
  private
    FDeviceName: string;
    function GetName: string;
  protected
    procedure ValidateContainer(AComponent: TComponent); override;
    procedure ValidateInsert(AComponent: TComponent); override; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TCDevice); virtual;
  published
    property DeviceName: string read FDeviceName write FDeviceName;
  end;

  TCPort = class(TCDevice, TIPort)
  private
    FConnector: TIConnector;
    function GetConnector: TIConnector;
    procedure SetConnector(Value: TIConnector);
  public
  end;

  TCInputPort = class(TCPort, TIInputPort)
  protected
    function GetIsEmpty: Boolean;
  public
    property IsEmpty: Boolean read GetIsEmpty;
  end;
  
  TCOutputPort = class(TCPort, TIOutputPort)
  protected
    function GetIsFull: Boolean;
  public
    property IsFull: Boolean read GetIsFull;
  end;

  TCBlock = class(TCDevice, TIBlock)
  private
    FBlocks: array of TIBlock;
    FInputPorts: array of TIInputPort;
    FOutputPorts: array of TIOutputPort;
  protected
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TCDevice); override;
    constructor Create(AOwner: TCBlock); virtual;
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const OutputName: string): Integer;
    procedure ConnectPorts(Output: TIOutputPort; Input:TIInputPort);
    procedure Execute; virtual;
  end;

  TCConnector = class(TCDevice,TIConnector)
  private
    FOutputPort: TIOutputPort;
    FInputPort: TIInputPort;
  protected
    function GetIsEmpty: Boolean;
    function GetIsFull: Boolean;
  public
    procedure Connect(Output: TIOutputPort; Input:TIInputPort);
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
  published
    property OutputPort: TIOutputPort read FOutputPort write FOutputPort;
    property InputPort: TIInputPort read FInputPort write FInputPort;
 end;

implementation

uses
  LResources;

function TCDevice.GetName: string;
begin
  Result := FDeviceName;
end;

procedure TCDevice.ValidateContainer(AComponent: TComponent);
begin
  if AComponent is TCBlock then with AComponent as TCBlock do begin
    ValidateInsert(Self);
  end;
end;

constructor TCDevice.Create(AOwner: TComponent);
begin
  if AOwner is TCDevice then begin
    Create(AOwner as TCDevice);
  end;
end;

constructor TCDevice.Create(AOwner: TCDevice);
begin
  inherited Create(AOwner);
  WriteLn('>>TCDevice.Create: ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount, ', Name = ', Name);
  if Assigned(AOwner) then begin
    WriteLn('AOwner.Name = ', AOwner.Name);
  end;
  if not InitResourceComponent(Self, TCDevice) then
    WriteLn('Failure');
  WriteLn('<<TCDevice.Create: ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount, ', Name = ', Name);
end;

function TCPort.GetConnector: TIConnector;
begin
  Result := FConnector;
end;

procedure TCPort.SetConnector(Value: TIConnector);
begin
  FConnector := Value;
end;

function TCInputPort.GetIsEmpty: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsEmpty;
end;

function TCOutputPort.GetIsFull: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsEmpty;
end;

constructor TCBlock.Create(AOwner: TCDevice);
begin
  if AOwner is TCBlock then begin
    Create(AOwner as TCBlock);
  end;
end;

constructor TCBlock.Create(AOwner: TCBlock);
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

procedure TCBlock.ValidateInsert(AComponent: TComponent);
var
  l: Integer;
begin
  WriteLn('>>TCBlock.ValidateInsert: InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
  WriteLn('TCBlock.ValidateInsert: AComponent.ClassName = ', AComponent.ClassName);
  if AComponent is TCInputPort then begin
    l := Length(FInputPorts);
    SetLength(FInputPorts, l + 1);
    FInputPorts[l] := AComponent as TCInputPort;
  end else if AComponent is TCOutputPort then begin
    l := Length(FOutputPorts);
    SetLength(FOutputPorts, l + 1);
    FOutputPorts[l] := AComponent as TCOutputPort;
  end else if AComponent is TCBlock then begin
    l := Length(FBlocks);
    SetLength(FBlocks, l + 1);
    FBlocks[l] := AComponent as TCBlock;
  end;
  WriteLn('<<TCBlock.ValidateInsert: InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
end;

procedure TCBlock.ConnectPorts(Output: TIOutputPort; Input:TIInputPort);
var
  Connector: TCConnector;
begin
  Connector := TCConnector.Create(Self);
  with Connector do begin
    Connect(Output, Input);
  end;
end;

procedure TCBlock.Execute;
var
  i: Integer;
begin
  WriteLn('TCBlock.Execute : Name = ', FDeviceName, ', BlocksCount = ', Length(FBlocks));
  for i := Low(FInputPorts) to High(FInputPorts) do with FInputPorts[i] do begin
    if IsEmpty then
      Exit;
  end;
  for i := Low(FOutputPorts) to High(FOutputPorts) do with FOutputPorts[i] do begin
    if IsFull then
      Exit;
  end;
  for i := Low(FBlocks) to High(FBlocks) do with FBlocks[i] do begin
    Execute;
  end;
end;

function TCConnector.GetIsEmpty: Boolean;
begin
  Result := False;
end;

function TCConnector.GetIsFull: Boolean;
begin
  Result := False;
end;

procedure TCConnector.Connect(Output:TIOutputPort; Input: TIInputPort);
begin
  FOutputPort := Output;
  FInputPort := Input;
  Output.Connector := Self;
  Input.Connector := Self;
end;

end.

