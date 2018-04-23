unit BlockBasics;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, FifoBasics;

type
  TDeviceRunFlags = (drfTerminated, drfRunning, drfBlockedByInput, drfBlockedByOutput);
  TDeviceRunStatus = set of TDeviceRunFlags;
  IDevice = interface
    function GetName:string;
    property DeviceName: string read GetName;
  end;
  TCConnector = class;
  TCDevice = class(TComponent, IDevice)
  private
    function GetName: string;
  protected
    procedure ValidateContainer(AComponent: TComponent); override;
    procedure ValidateInsert(AComponent: TComponent); override; abstract;
  end;
  IPort = interface(IDevice)
    function GetConnector: TCConnector;
    procedure SetConnector(Value: TCConnector);
  end;
  IInputPort = interface(IPort)
    function GetIsEmpty: Boolean;
    function Pop(out Samples; Qty: Word): Boolean;
    function Pop(out Sample: Integer): Boolean;
    property IsEmpty: Boolean read GetIsEmpty;
  end;
  IOutputPort = interface(IPort)
    function GetIsFull: Boolean;
    function Push(const Samples; Qty: Word): Boolean;
    function Push(Sample: Integer): Boolean;
    property IsFull: Boolean read GetIsFull;
  end;
  TCPort = class(TCDevice, IPort)
  protected
    FConnector: TCConnector;
    function GetConnector: TCConnector;
    procedure SetConnector(Value: TCConnector); virtual;
    property Connector: TCConnector write SetConnector;
  end;
  TCInputPort = class(TCPort, IInputPort)
  protected
    function GetIsEmpty: Boolean;
  public
    function Pop(out P: Pointer): Boolean;
    function Pop(out Samples; Qty: Word): Boolean;
    function Pop(out Sample: Integer): Boolean; inline;
    property IsEmpty: Boolean read GetIsEmpty;
  end;
  TCOutputPort = class(TCPort, IOutputPort)
  protected
    function GetIsFull: Boolean;
  public
    function Push(P: Pointer): Boolean;
    function Push(const Samples; Qty: Word): Boolean;
    function Push(Sample: Integer): Boolean; inline;
    property IsFull: Boolean read GetIsFull;
  end;
  TCInputPortRef = class(TCInputPort, IOutputPort)
  private
    FInternalConnector: TCConnector;
    //function GetConnector: TCConnector; override;
    function GetIsFull: Boolean;
    function Push(P: Pointer): Boolean;
    function Push(const Samples; Qty: Word): Boolean;
    function Push(Sample: Integer): Boolean;
  protected
    procedure SetConnector(Value: TCConnector); override;
  end;
  TCOutputPortRef = class(TCOutputPort, IInputPort)
  private
    FInternalConnector: TCConnector;
    //function GetConnector: TCConnector; override;
    function GetIsEmpty: Boolean;
    function Pop(out P: Pointer): Boolean;
    function Pop(out Samples; Qty: Word): Boolean;
    function Pop(out Sample: Integer): Boolean;
  protected
    procedure SetConnector(Value: TCConnector); override;
  end;

  TCBlock = class(TCDevice)
  private
    FBlocks: array of TCBlock;
    FInputPorts: array of TCInputPort;
    FOutputPorts: array of TCOutputPort;
  protected
    FRunStatus: TDeviceRunStatus;
    procedure ValidateInsert(AComponent: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetInputQty: Integer;
    function GetOutputQty: Integer;
    function GetInputIdx(const InputName: string): Integer;
    function GetOutputIdx(const OutputName: string): Integer;
    procedure Execute; virtual;
    property RunStatus: TDeviceRunStatus read FRunStatus;
  end;

  TCConnector = class(TCDevice)
  private
    FOutputPort: TCOutputPort;
    FInputPort: TCInputPort;
    FSamples: TCFifo;
    FDepth: Integer;
    procedure SetOutputPort(Output: TCOutputPort);
    procedure SetInputPort(Input:TCInputPort);
    function GetIsEmpty: Boolean;
    function GetIsFull: Boolean;
    function Push(const Sample): Boolean;
    function Pop(out Sample): Boolean;
    procedure SetDepth(ADepth: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFull: Boolean read GetIsFull;
  published
    property OutputPort: TCOutputPort read FOutputPort write SetOutputPort;
    property InputPort: TCInputPort read FInputPort write SetInputPort;
    property Depth: Integer read FDepth write SetDepth;
  end;

  TFuncName = string[31];
  TFuncPrefix = string[63];

function FuncB(name: TFuncName): TFuncPrefix;
function FuncC(name: TFuncName): TFuncPrefix;
function FuncE(name: TFuncName): TFuncPrefix;

implementation

uses
  LResources;

type
  PPortData = ^TPortData;
  TPortData = record
    case Size: Word of
      1: (AsByte: Byte);
      2: (AsWord: Word);
      4: (AsLongWord: LongWord);
      6: (Raw: array[Word] of Byte);
  end;

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

function TCDevice.GetName: string;
begin
  Result := Name;
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

function TCPort.GetConnector: TCConnector;
begin
  Result := FConnector;
end;

procedure TCPort.SetConnector(Value: TCConnector);
begin
  FConnector := Value;
end;

function TCInputPort.GetIsEmpty: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsEmpty;
end;

function TCInputPort.Pop(out P: Pointer): Boolean;
begin
  //WriteLn(FuncB('TCInputPort.Pop(Pointer)'), 'Name = ', Owner.Name, '.', Name);
  if Assigned(FConnector) then begin
    //WriteLn(FuncC('TCInputPort.Pop'), 'Connector = ', FConnector.Owner.Name, '.', FConnector.Name);
    Result := FConnector.Pop(P);
  end else begin
    //WriteLn(FuncC('TCInputPort.Pop'), 'Connector not assigned');
    Result := False
  end;
  //WriteLn(FuncE('TCInputPort.Pop'), 'Name = ', Owner.Name, '.', Name, ' P = ', hexStr(P));
end;

function TCInputPort.Pop(out Samples; Qty: Word): Boolean;
var
  P: PPortData;
begin
  //WriteLn(FuncB('TCInputPort.Pop'), 'Name = ', Owner.Name, '.', Name);
  Result := Pop(P);
  if Result then with P^ do begin
    if Qty > Size then begin
      Qty := Size
    end;
    Move(Raw, Samples, Qty);
    Freemem(P);
  end;
  //WriteLn(FuncE('TCInputPort.Pop'), 'Name = ', Owner.Name, '.', Name);
end;

function TCInputPort.Pop(out Sample: Integer): Boolean;
begin
  Result := Pop(Sample, SizeOf(Sample));
end;

function TCOutputPort.GetIsFull: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsFull;
end;

function TCOutputPort.Push(P: Pointer): Boolean;
begin
  //WriteLn(FuncB('TCOutputPort.Push'), 'Name = ', Owner.Name, '.', Name, ' P = ', HexStr(P));
  if Assigned(FConnector) then begin
    //WriteLn(FuncC('TCOutputPort.Push'), 'Connector = ', FConnector.Owner.Name, '.', FConnector.Name);
    Result := FConnector.Push(P);
  end else begin
    //WriteLn(FuncC('TCOutputPort.Push'), 'Connector not assigned');
    Result := False;
  end;
  if Result then begin
  end;
  //WriteLn(FuncE('TCOutputPort.Push'));
end;

function TCOutputPort.Push(const Samples; Qty: Word): Boolean;
var
  P: PPortData;
begin
  //WriteLn(FuncB('TCOutputPort.Push'));
  P := GetMem(Qty + SizeOf(P^.Size));
  if not Assigned(P) then begin
    Exit(False)
  end;
  with P^ do begin
    Size := Qty;
    Move(Samples, Raw, Qty);
  end;
  Result := Push(P);
  //WriteLn(FuncE('TCOutputPort.Push'));
end;

function  TCOutputPort.Push(Sample: Integer): Boolean;
begin
  Result := Push(Sample, SizeOf(Sample));
end;

function TCInputPortRef.GetIsFull: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsFull;
end;

function TCInputPortRef.Push(P: Pointer): Boolean;
begin
  //WriteLn(FuncB('TCInputPortRef.Push'), 'Name = ', Owner.Name, '.', Name, ' P = ', hexStr(P));
  if Assigned(FInternalConnector) then begin
    //WriteLn(FuncC('TCInputPortRef.Push'), 'Connector = ', FConnector.Owner.Name, '.', FConnector.Name);
    Result := FInternalConnector.Push(P);
  end else begin
    //WriteLn(FuncC('TCInputPortRef.Push'), 'Connector not assigned');
    Result := False;
  end;
  if Result then begin
  end;
  //WriteLn(FuncE('TCInputPortRef.Push'));
end;

function TCInputPortRef.Push(const Samples; Qty: Word): Boolean;
var
  P: PPortData;
begin
  //WriteLn(FuncB('TCInputPortRef.Push'));
  P := GetMem(Qty + SizeOf(P^.Size));
  if not Assigned(P) then begin
    Exit(False)
  end;
  with P^ do begin
    Size := Qty;
    Move(Samples, Raw, Qty);
  end;
  Result := Push(P);
  //WriteLn(FuncE('TCInputPortRef.Push'));
end;

function  TCInputPortRef.Push(Sample: Integer): Boolean;
begin
  Result := Push(Sample, SizeOf(Sample));
end;

procedure TCInputPortRef.SetConnector(Value: TCConnector);
begin
  if Assigned(Value) then begin
    if Value.Owner = Owner then begin
      FInternalConnector := Value;
    end else begin
      FConnector := Value;
    end;
  end;
end;

function TCOutputPortRef.GetIsEmpty: Boolean;
begin
  Result := Assigned(FConnector) and FConnector.IsEmpty;
end;

function TCOutputPortRef.Pop(out P: Pointer): Boolean;
begin
  //WriteLn(FuncB('TCOutputPortRef.Pop(Pointer)'), 'Name = ', Owner.Name, '.', Name);
  if Assigned(FInternalConnector) then begin
    //WriteLn(FuncC('TCOutputPortRef.Pop'), 'Connector = ', FConnector.Owner.Name, '.', FConnector.Name);
    Result := FInternalConnector.Pop(P);
  end else begin
    //WriteLn(FuncC('TCOutputPortRef.Pop'), 'Connector not assigned');
    Result := False
  end;
  //WriteLn(FuncE('TCOutputPortRef.Pop'), 'Name = ', Owner.Name, '.', Name, ' P = ', hexStr(P));
end;

function TCOutputPortRef.Pop(out Samples; Qty: Word): Boolean;
var
  P: PPortData;
begin
  //WriteLn(FuncB('TCOutputPortRef.Pop'), 'Name = ', Owner.Name, '.', Name);
  Result := Pop(P);
  if Result then with P^ do begin
    if Qty > Size then begin
      Qty := Size
    end;
    Move(Raw, Samples, Qty);
    Freemem(P);
  end;
  //WriteLn(FuncE('TCOutputPortRef.Pop'), 'Name = ', Owner.Name, '.', Name);
end;

function TCOutputPortRef.Pop(out Sample: Integer): Boolean;
begin
  Result := Pop(Sample, SizeOf(Sample));
end;

procedure TCOutputPortRef.SetConnector(Value: TCConnector);
begin
  //WriteLn(FuncB('TCOutputPortRef.SetConnector'), 'Name = ', Owner.Name, '.', Name, ', Connector = ', HexStr(Value));
  if Assigned(Value) then begin
    //WriteLn(FuncC('TCOutputPortRef.SetConnector'), 'Connector = ', Value.Owner, '.', Value.Name);
    if Value.Owner = Owner then begin
      FInternalConnector := Value;
    end else begin
      FConnector := Value;
    end;
  end;
  //WriteLn(FuncE('TCOutputPortRef.SetConnector'), 'Name = ', Owner.Name, '.', Name);
end;

constructor TCBlock.Create(AOwner: TComponent);
//var
//  cn: string;
begin
  //if Assigned(AOwner) then begin
  //  cn := AOwner.ClassName;
  //end else begin
  //  cn := 'nil';
  //end;
  //WriteLn(FuncB('TCBlock.Create'), 'AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  if not InitComponentFromResource(Self, ClassType) then begin
    WriteLn('Failed to initilize component ', AOwner.Name, '.', Name, ': ', ClassName);
  end;
  //WriteLn(FuncE('TCBlock.Create'), 'AOwner.ClassName = ', cn);
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
    if FInputPorts[i].Name = InputName then begin
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
    if FOutputPorts[i].Name = OutputName then begin
      Exit(i);
    end;
  end;
end;

procedure TCBlock.ValidateInsert(AComponent: TComponent);
var
  l: Integer;
begin
  //WriteLn(FuncB('TCBlock.ValidateInsert'), 'Name = ', Name, ', ClassName = ', ClassName);
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'AComponent.ClassName = ', AComponent.ClassName);
  if AComponent is TCInputPort then begin
    l := Length(FInputPorts);
    SetLength(FInputPorts, l + 1);
    FInputPorts[l] := AComponent as TCInputPort;
    //WriteLn(FuncC('TCBlock.ValidateInsert'), 'AComponent = ', hexStr(AComponent), ', FInputPorts[', l, '] = ', HexStr(FInputPorts[l]));
  end else if AComponent is TCOutputPort then begin
    l := Length(FOutputPorts);
    SetLength(FOutputPorts, l + 1);
    FOutputPorts[l] := AComponent as TCOutputPort;
    //WriteLn(FuncC('TCBlock.ValidateInsert'), 'AComponent = ', hexStr(AComponent), ', FOutputPorts[', l, '] = ', HexStr(FOutputPorts[l]));
  end else if AComponent is TCBlock then begin
    l := Length(FBlocks);
    SetLength(FBlocks, l + 1);
    FBlocks[l] := AComponent as TCBlock;
    //WriteLn(FuncC('TCBlock.ValidateInsert'), 'AComponent = ', hexStr(AComponent), ', FBlocks[', l, '] = ', HexStr(FBlocks[l]));
  end;
  //WriteLn(FuncC('TCBlock.ValidateInsert'), 'InputCount = ', Length(FInputPorts), ', OutputCount', Length(FOutputPorts), ', Blocks = ', Length(FBlocks));
  //WriteLn(FuncE('TCBlock.ValidateInsert'), 'Name = ', Name, ', ClassName = ', ClassName);
end;

procedure TCBlock.Execute;
var
  i: Integer;
  BlockRunStatus: TDeviceRunStatus;
  RunBlock: Boolean;
  P: PPortData;
begin
  //WriteLn(FuncB('TCBlock.Execute'), 'Name = ', Name, ', BlocksCount = ', Length(FBlocks));
  BlockRunStatus := [drfTerminated];
  FRunStatus := [drfRunning];
  for i := Low(FInputPorts) to High(FInputPorts) do begin
    if FInputPorts[i] is TCInputPortRef then with FInputPorts[i] as TCInputPortRef do begin
      while Pop(P) do begin
        Push(P);
      end;
    end;
  end;
  for i := Low(FBlocks) to High(FBlocks) do with FBlocks[i] do begin
    RunBlock := False;
    if not (drfTerminated in RunStatus) then  begin
      Exclude(BlockRunStatus, drfTerminated);
      RunBlock := True;
    end;
    if drfRunning in RunStatus then begin
      RunBlock := False;
    end;
    if drfBlockedByInput in RunStatus then  begin
      Include(BlockRunStatus, drfBlockedByInput);
      RunBlock := False;
    end;
    if drfBlockedByOutput in RunStatus then  begin
      Include(BlockRunStatus, drfBlockedByOutput);
      RunBlock := False;
    end;
    //WriteLn(FuncC('TCBlock.Execute'), 'Block[', i, '].DeviceName = ', Name, ', Terminated = ', drfTerminated in RunStatus, ', BlockedByInput = ', drfBlockedByInput in RunStatus, ', BlockedByOutput = ', drfBlockedByOutput in RunStatus);
    if RunBlock then begin
      Execute;
    end;
  end;
  //WriteLn(FuncC('TCBlock.Execute'), 'Low(FOutputPorts) = ', Low(FOutputPorts), ', High(FOutputPorts) = ', High(FOutputPorts));
  //WriteLn(FuncC('TCBlock.Execute'), 'Length(FOutputPorts) = ', Length(FOutputPorts));
  for i := Low(FOutputPorts) to High(FOutputPorts) do begin
      //WriteLn(FuncC('TCBlock.Execute'), 'OutputPort[', i, '] = ', FOutputPorts[i].Name, ' is ', FOutputPorts[i].ClassName);
    if FOutputPorts[i] is TCOutputPortRef then with FOutputPorts[i] as TCOutputPortRef do begin
      //WriteLn(FuncC('TCBlock.Execute'), 'OutputPort[', i, '] = ', Name);
      while Pop(P) do begin
        Push(P);
      end;
    end;
  end;
  FRunStatus := BlockRunStatus;
  //WriteLn(FuncE('TCBlock.Execute'), 'Name = ', Name, ', BlocksCount = ', Length(FBlocks));
end;

function TCConnector.GetIsEmpty: Boolean;
begin
  Result := FSamples.GetPendingQty = 0;
end;

function TCConnector.GetIsFull: Boolean;
begin
  Result := FSamples.GetAvailableQty = 0;
end;

procedure TCConnector.SetDepth(ADepth: Integer);
var
  OldSamples: TCFifo;
  p: Pointer;
begin
  if FDepth <> ADepth then begin
    FDepth := ADepth;
    OldSamples := FSamples;
    FSamples := TCFifo.Create(FDepth);
    if Assigned(OldSamples) then begin
      while oldSamples.Pop(p) do begin
        FSamples.Push(p);
      end;
      OldSamples.Free;
    end;
  end;
end;

constructor TCConnector.Create(AOwner: TComponent);
//var
  //cn: string;
begin
  //if Assigned(AOwner) then begin
    //cn := AOwner.ClassName;
  //end else begin
    //cn := 'nil';
  //end;
  //WriteLn(FuncB('TCConnector.Create'), 'AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  //WriteLn(FuncC('TCConnector.Create'), Name, '.InputPort = $', HexStr(PtrInt(FInputPort), 8));
  //WriteLn(FuncC('TCConnector.Create'), Name, '.OutputPort = $', HexStr(PtrInt(FOutputPort), 8));
  Depth := 127;
  //WriteLn(FuncE('TCConnector.Create'), 'AOwner.ClassName = ', cn);
end;

procedure TCConnector.SetOutputPort(Output:TCOutputPort);
begin
  FOutputPort := Output;
  Output.Connector := Self;
end;

procedure TCConnector.SetInputPort(Input: TCInputPort);
begin
  FInputPort := Input;
  Input.Connector := Self;
end;

function TCConnector.Push(const Sample): Boolean;
begin
  //WriteLn(FuncB('TCConnector.Push'), Owner.Name, '.', Name, ', Sample = ', Sample, ', Samples.FreeQty = ', FSamples.GetAvailableQty);
  repeat
    Result := FSamples.Push(Pointer(Sample));
    //WriteLn(FuncC('TCConnector.Push'), 'Could push = ', Result);
    if Result then with InputPort.Owner as TCBlock do begin
      Exclude(FRunStatus, drfBlockedByInput);
    end else with FInputPort.Owner as TCBlock do begin
      //WriteLn(FuncC('TCConnector.Push'), Owner.Name, '.', Name, ' connected to ', FInputPort.Name, ' (Terminated = ', drfTerminated in RunStatus, ')');
      if drfTerminated in FRunStatus then with FOutputPort.Owner as TCBlock do begin
        Include(FRunStatus, drfTerminated);
        Break;
      end else begin
        Execute;
      end;
    end;
  until Result;
  //WriteLn(FuncE('TCConnector.Push'), Owner.Name, '.', Name, ', Sample = ', Sample, ', Samples.Qty = ', FSamples.GetPendingQty);
end;

function TCConnector.Pop(out Sample): Boolean;
begin
  //WriteLn(FuncB('TCConnector.Pop'), Owner.Name, '.', Name, ', Samples.Qty = ', FSamples.GetPendingQty);
  repeat
    Result := FSamples.Pop(Pointer(Sample));
    //WriteLn(FuncC('TCConnector.Pop'), 'Could pop = ', Result);
    if Result then with OutputPort.Owner as TCBlock do begin
      Exclude(FRunStatus, drfBlockedByOutput);
    end else with FOutputPort.Owner as TCBlock do begin
      //WriteLn(FuncC('TCConnector.Pop'), Owner.Name, '.', Name, ' connected to ', FOutputPort.Name, ' (Terminated = ', drfTerminated in RunStatus, ')');
      if drfTerminated in FRunStatus then with FInputPort.Owner as TCBlock do begin
        Include(FRunStatus, drfTerminated);
        Break;
      end else begin
        Execute;
      end;
    end;
  until Result;
  //WriteLn(FuncE('TCConnector.Pop'), Owner.Name, '.', Name, ', Sample = ', Sample, ', Samples.FreeQty = ', FSamples.GetAvailableQty);
end;

end.

