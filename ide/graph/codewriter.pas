unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree, GraphComponents;

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
function GetUserCodePosition(BlockName: string; Self: TCodeBuffer):TPoint;

implementation

uses
  CodeToolManager, DesignGraph;

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
  var
    BlockDeviceType: string;
  function UpdateUsesClause: Boolean;
  var
    i: Integer;
    Component: TComponent;
    p, e: Integer;
    usedBlocks: string;
  begin
    Result := True;
    usedBlocks := 'uses' + LineEnding + '  ';
    p := System.Pos(usedBlocks, Self.Source);
    if p = 0 then
      Exit(False);
    //WriteLn(Self.source);
    e := System.Pos('Blocks, Designs;' + LineEnding, Self.Source);
    //WriteLn('p = ', p, ', e = ', e);
    if e < p then
      Exit(False);
    for i := 0 to Block.ComponentCount - 1 do begin
      Component := Block.Components[i];
      if Component is TCGraphBlock then with Component as TCGraphBlock do begin
        usedBlocks += Name + ', ';
      end;
    end;
    //WriteLn('p = ', p, ', e = ', e);
    //WriteLn(usedBlocks);
    Self.Replace(p, e - p, usedBlocks);
  end;
  function UpdateBlocksIdentifiers: Boolean;
  var
    i: Integer;
    Component: TComponent;
  begin
    Result := True;
    for i := 0 to Block.ComponentCount - 1 do begin
      Component := Block.Components[i];
      if Component is TCGraphBlock then with Component as TCGraphBlock do begin
        CodeToolBoss.AddPublishedVariable(Self, BlockDeviceType, DeviceIdentifier, DeviceType);
      end;
    end;
  end;
  function UpdatePortsIdentifiers: Boolean;
  var
    i: Integer;
    Component: TComponent;
  begin
    Result := True;
    for i := 0 to Block.ComponentCount - 1 do begin
      Component := Block.Components[i];
      if Component is TCGraphPort then with Component as TCGraphPort do begin
        CodeToolBoss.AddPublishedVariable(Self, TCGraphDevice(Block).DeviceType, DeviceIdentifier, DeviceType);
      end;
    end;
  end;
  function UpdateConnectorsIdentifiers: Boolean;
  begin
    Result := True;
  end;
begin
    if Block is TCGraphDesign then with Block as TCGraphDesign do begin
      BlockDeviceType := DeviceType;
    end else if Block is TCGraphBlock then with Block as TCGraphBlock do begin
      BlockDeviceType := DeviceType;
    end else begin
      Exit(False);
    end;
  Result := UpdateUsesClause and UpdateBlocksIdentifiers and UpdatePortsIdentifiers and UpdateConnectorsIdentifiers;
end;

procedure WriteSimulatorSourceTemplate(Owner: TIGraphDevice; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'program Simulate' + Owner.DeviceIdentifier + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  ' + Owner.DeviceIdentifier + ';' + LineEnding +
      LineEnding +
      'var' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator: TCustom' + Owner.DeviceIdentifier + ';' + LineEnding +
      LineEnding +
      'begin' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator := TCustomDesign.Create(nil);' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator.Run;' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator.Free;' + LineEnding +
      'end.');
  end;
end;

procedure WriteDesignSourceTemplate(Owner: TIGraphDevice; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'unit ' + Owner.DeviceIdentifier + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' + LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Blocks, Designs;' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  TCustom' + Owner.DeviceIdentifier + ' = class(TDesign)' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Classes;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  RegisterClass(T' + Owner.DeviceIdentifier + ');' + LineEnding +
      LineEnding +
      'finalization' + LineEnding +
      LineEnding +
      'end.');
  end;
end;

procedure WriteBlockSourceTemplate(Owner: TIGraphDevice; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'unit ' + Owner.DeviceIdentifier + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' +
      LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Blocks;' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  ' + Owner.DeviceType + ' = class(' + Owner.DeviceAncestorType + ')' + LineEnding +
      '    procedure Execute; override;' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Classes;' + LineEnding +
      LineEnding +
      'procedure ' + Owner.DeviceType + '.Execute;' + LineEnding +
      'begin;' + LineEnding +
      '  {Write here your code}' + LineEnding +
      'end;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  RegisterClass(T' + Owner.DeviceIdentifier + ');' + LineEnding +
      LineEnding +
      'finalization' + LineEnding +
      'end.');
  end;
end;

function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
begin
  Result := CodeToolBoss.LoadFile(FileName, False, False);
  if not Assigned(Result) then begin
    Result := CodeToolBoss.CreateFile(FileName);
    case template of
      cttSimulator: WriteSimulatorSourceTemplate(Owner, Result);
      cttDesign: WriteDesignSourceTemplate(Owner, Result);
      cttBlock,
      cttSource,
      cttProbe: WriteBlockSourceTemplate(Owner, Result);
    end;
  end;
end;

function GetUserCodePosition(BlockName: string; Self: TCodeBuffer):TPoint;
var
  UserCodeFunction: string;
  p, l: Integer;
begin
  UserCodeFunction := 'procedure T' + BlockName + '.Execute;';
  Result.X := 0;
  with Self do begin
    p := Pos(UserCodeFunction, Source);
    l := 2;
    while p > 0 do begin
      if Source[p] = LineEnding then
        l += 1;
      p -= 1;
    end;
  end;
  Result.Y := l;
end;

end.

