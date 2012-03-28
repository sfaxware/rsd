unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, FifoBasics;

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
    function GetOutputPort: TIOutputPort;
    function GetInputPort: TIInputPort;
    procedure SetOutputPort(Output: TIOutputPort);
    procedure SetInputPort(Input:TIInputPort);
    procedure Push(Sample: Integer);
    procedure Pop(out Sample: Integer);
    property OutputPort: TIOutputPort read GetOutputPort write SetOutputPort;
    property InputPort: TIInputPort read GetInputPort write SetInputPort;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
  end;

  TCBlock = class;

  TCDevice = class(TComponent, TIDevice)
  private
    FDeviceName: string;
    function GetName: string;
  protected
    procedure ValidateContainer(AComponent: TComponent); override;
    procedure ValidateInsert(AComponent: TComponent); override; abstract;
  public
    constructor Create(AOwner: TComponent); override;
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
    procedure Pop(out Sample: Integer);
  end;
  
  TCOutputPort = class(TCPort, TIOutputPort)
  protected
    function GetIsFull: Boolean;
  public
    property IsFull: Boolean read GetIsFull;
    procedure Push(Sample: Integer);
  end;

  TCBlock = class(TCDevice, TIBlock)
  private
    FBlocks: array of TIBlock;
    FInputPorts: array of TIInputPort;
    FOutputPorts: array of TIOutputPort;
  protected
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const OutputName: string): Integer;
    procedure Execute; virtual;
  end;

  TCConnector = class(TCDevice,TIConnector)
  private
    FOutputPort: TCOutputPort;
    FInputPort: TCInputPort;
    FSamples: TCFifo;
  protected
    function GetIsEmpty: Boolean;
    function GetIsFull: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    function GetOutputPort: TIOutputPort;
    function GetInputPort: TIInputPort;
    procedure SetOutputPort(Output: TIOutputPort);
    procedure SetOutputPort(Output: TCOutputPort);
    procedure SetInputPort(Input:TIInputPort);
    procedure SetInputPort(Input:TCInputPort);
    procedure Push(Sample: Integer);
    procedure Pop(out Sample: Integer);
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
  published
    property OutputPort: TCOutputPort read FOutputPort write SetOutputPort;
    property InputPort: TCInputPort read FInputPort write SetInputPort;
 end;

  TFuncName = string[31];
  TFuncPrefix = string[63];

function FuncB(name: TFuncName): TFuncPrefix;
function FuncC(name: TFuncName): TFuncPrefix;
function FuncE(name: TFuncName): TFuncPrefix;

implementation

uses
  LResources;

var
  FuncLevel: TFuncName;

function FuncB(name: TFuncName): TFuncPrefix;
begin
  Result := FuncLevel + '>>' + name + ': ';
  FuncLevel += '  ';
end;

function FuncC(name: TFuncName): TFuncPrefix;
begin
  Result := FuncLevel + name + ': ';
end;

function FuncE(name: TFuncName): TFuncPrefix;
begin
  SetLength(FuncLevel,  Length(FuncLevel) - 2);
  Result := FuncLevel + '<<' + name + ': ';
end;

function TCDevice.GetName: string;
begin
  Result := FDeviceName;
end;

procedure TCDevice.ValidateContainer(AComponent: TComponent);
begin
  //WriteLn(FuncB('TCDevice.ValidateContainer'), 'Name = ', Name, ', ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount);
  //with AComponent do begin
  //  WriteLn(FuncC('TCDevice.ValidateContainer'), 'Name = ', Name, ', ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount);
  //end;
  if AComponent is TCBlock then with AComponent as TCBlock do begin
    ValidateInsert(Self);
  end;
  //WriteLn(FuncE('TCDevice.ValidateContainer'), 'Name = ', Name, ', ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount);
end;

constructor TCDevice.Create(AOwner: TComponent);
  function InitComponentFromResource(Instance: TComponent; ClassType: TClass): Boolean;
  var
    FPResource: TFPResourceHandle;
    ResName: String;
    Stream: TStream;
    Reader: TReader;
    DestroyDriver: Boolean;
    Driver: TAbstractObjectReader;
  begin
    Result := False;
    Stream := nil;
    ResName := ClassType.ClassName;
    if Stream = nil then
    begin
      FPResource := FindResource(HInstance, PChar(ResName), RT_RCDATA);
      if FPResource <> 0 then
        Stream := TLazarusResourceStream.CreateFromHandle(HInstance, FPResource);
    end;
    if Stream = nil then
      Exit;
    DestroyDriver:=false;
    try
      Reader := CreateLRSReader(Stream, DestroyDriver);
      try
        Reader.ReadRootComponent(Instance);
      finally
        Driver := Reader.Driver;
        Reader.Free;
        if DestroyDriver then
          Driver.Free;
      end;
    finally
      Stream.Free;
    end;
    Result := True;
  end;
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  //if AOwner is TCBlock then begin
//    Create(AOwner as TCBlock);
  //end else begin
  //  Fail;
  //end;
  //WriteLn(FuncB('TCDevice.Create'), 'ClassName = ', ClassName, ', Name = ', Name, ', Owner.Name = ', cn, ', ComponentCount = ', ComponentCount);
  inherited Create(AOwner);
  if not InitComponentFromResource(Self, ClassType) then begin
    WriteLn('Failed to initilize component ', AOwner.Name, '.', Name, ': ', ClassName);
  end;
  //WriteLn(FuncE('TCDevice.Create'), 'ClassName = ', ClassName, ', Name = ', Name, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount);
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

procedure TCInputPort.Pop(out Sample: Integer);
begin
  //WriteLn(FuncB('TCInputPort.Pop'));
  if Assigned(FConnector) then begin
    //WriteLn(FuncC('TCInputPort.Pop'), 'Connector assigned');
    FConnector.Pop(Sample);
  end else begin
    //WriteLn(FuncC('TCInputPort.Pop'), 'Connector not assigned');
  end;
  //WriteLn(FuncE('TCInputPort.Pop'));
end;

function TCOutputPort.GetIsFull: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsEmpty;
end;

procedure TCOutputPort.Push(Sample: Integer);
begin
  //WriteLn(FuncB('TCInputPort.Push'));
  if Assigned(FConnector) then begin
    //WriteLn(FuncC('TCInputPort.Push'), 'Connector assigned');
    FConnector.Push(Sample);
  end else begin
    //WriteLn(FuncC('TCInputPort.Push'), 'Connector not assigned');
  end;
  //WriteLn(FuncE('TCInputPort.Push'));
end;

constructor TCBlock.Create(AOwner: TComponent);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  //WriteLn(FuncB('TCBlock.Create(AOwner: TComponent)'), 'AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  //WriteLn(FuncE('TCBlock.Create(AOwner: TComponent)'), 'AOwner.ClassName = ', cn);
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
  //WriteLn(FuncB('TCBlock.ValidateInsert'), 'Name = ', Name, ', ClassName = ', ClassName, ', DeviceName = ', DeviceName);
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'AComponent.ClassName = ', AComponent.ClassName);
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
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
  //WriteLn(FuncE('TCBlock.ValidateInsert'), 'Name = ', Name, ', ClassName = ', ClassName, ', DeviceName = ', DeviceName);
end;

procedure TCBlock.Execute;
var
  i: Integer;
begin
  //WriteLn(FuncB('TCBlock.Execute'), 'Name = ', FDeviceName, ', BlocksCount = ', Length(FBlocks));
  for i := Low(FInputPorts) to High(FInputPorts) do with FInputPorts[i] do begin
    if IsEmpty then
      Exit;
  end;
  for i := Low(FOutputPorts) to High(FOutputPorts) do with FOutputPorts[i] do begin
    if IsFull then
      Exit;
  end;
  for i := Low(FBlocks) to High(FBlocks) do with FBlocks[i] do begin
    //WriteLn(FuncC('TCBlock.Execute'), 'Block[', i, '] = ', PtrInt(FBlocks[i]), ', DeviceName = ', DeviceName);
    Execute;
  end;
  //WriteLn(FuncE('TCBlock.Execute'), 'Name = ', FDeviceName, ', BlocksCount = ', Length(FBlocks));
end;

function TCConnector.GetIsEmpty: Boolean;
begin
  Result := FSamples.GetPendingQty <= 0;
end;

function TCConnector.GetIsFull: Boolean;
begin
  Result := FSamples.GetPendingQty > 0;
end;

constructor TCConnector.Create(AOwner: TComponent);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  //WriteLn(FuncB('TCConnector.Create'), 'AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  //WriteLn(FuncE('TCConnector.Create'), 'AOwner.ClassName = ', cn);
  FSamples := TCFifo.Create(128);
end;

function TCConnector.GetOutputPort:TIOutputPort;
begin
  Result := FOutputPort;
end;

function TCConnector.GetInputPort:TIInputPort;
begin
  Result := FInputPort;
end;

procedure TCConnector.SetOutputPort(Output:TIOutputPort);
begin
  //FOutputPort := Output as TCOutputPort;
  //Output.FConnector := Self;
end;

procedure TCConnector.SetOutputPort(Output:TCOutputPort);
begin
  FOutputPort := Output;
  Output.FConnector := Self;
end;

procedure TCConnector.SetInputPort(Input: TIInputPort);
begin
  //FInputPort := Input as TCInputPort;
  //Input.FConnector := Self;
end;

procedure TCConnector.SetInputPort(Input: TCInputPort);
begin
  FInputPort := Input;
  Input.FConnector := Self;
end;

procedure TCConnector.Push(Sample: Integer);
begin
  //WriteLn(FuncB('TCConnector.Push'), 'Sample = ', Sample);
  FSamples.Push(Pointer(Sample));
  //WriteLn(FuncE('TCConnector.Push'), 'Sample = ', Sample);
end;

procedure TCConnector.Pop(out Sample: Integer);
begin
  //WriteLn(FuncB('TCConnector.Pop'), 'Sample = ', Sample);
  Sample := Integer(FSamples.Pop);
  //WriteLn(FuncE('TCConnector.Pop'), 'Sample = ', Sample);
end;

end.

