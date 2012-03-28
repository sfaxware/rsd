unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree, GraphComponents;

function GetDeviceRandomCaption(AName: string): string;
function GetDeviceRandomName(AName: string): string;
function GetDeviceQty: Cardinal;
function InsertDevice(Device: TDevice; var Block: TBlock): Boolean;
function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
function GetCodeBuffer(template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
function GetUserCodePosition(BlockName: string; Self: TCodeBuffer):TPoint;
procedure IncrementDeviceQty;
procedure GuessNewDeviceNameAndType(var DeviceName, DeviceType, DeviceAncestorType: string);
procedure ResetDeviceQty;

implementation

uses
  CodeToolManager, Configuration;

var
  DeviceQty: Cardinal = 0;

function GetDeviceRandomCaption(AName: string): string;
begin
  Result := AName + IntToStr(DeviceQty);
end;

function GetDeviceRandomName(AName: string): string;
begin
  Result := AName + IntToStr(DeviceQty);
end;

function GetDeviceQty: Cardinal;
begin
  Result := DeviceQty;
end;

function InsertDevice(Device: TDevice; var Block: TBlock): Boolean;
begin
  with Block do begin
    if not Assigned(CodeBuffer[ctSource]) then begin
      CodeBuffer[ctSource] := GetCodeBuffer(Block.DeviceCodeTemplateType, Block);
    end;
    CodeBuffer[ctSource].LockAutoDiskRevert;
    with CodeToolBoss do begin
      AddUnitToMainUsesSection(CodeBuffer[ctSource], Device.DeviceUnitName, '');
      AddPublishedVariable(CodeBuffer[ctSource], DeviceType, Device.DeviceIdentifier, Device.DeviceType);
    end;
    CodeBuffer[ctSource].UnlockAutoDiskRevert;
  end;
end;

procedure WriteSimulatorSourceTemplate(Owner: TIGraphDevice; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'program Simulate' + Owner.DeviceIdentifier + ';' + LineEnding +
      '{$MODE OBJFPC}{$LONGSTRINGS ON}{$INTERFACES CORBA}' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  SysUtils, ' + Owner.DeviceIdentifier + ';' + LineEnding +
      LineEnding +
      'var' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator: ' + Owner.DeviceType + ';' + LineEnding +
      '  ' + 'progPath, ProgDir, SimDir: string;' + LineEnding +
      LineEnding +
      'begin' + LineEnding +
      '  ProgPath := ParamStr(0);' + LineEnding +
      '  ProgDir := ExtractFileDir(ProgPath);' + LineEnding +
      '  SimDir := ExtractFileDir(ProgDir) + ''/simulations'';' + LineEnding +
      '  if not DirectoryExists(SimDir) then begin' + LineEnding +
      '    MkDir(SimDir);' + LineEnding +
      '  end;' + LineEnding +
      '  ChDir(SimDir);' + LineEnding +
      '  ' + Owner.DeviceIdentifier + 'Simulator := ' + Owner.DeviceType + '.Create(nil);' + LineEnding +
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
      '{$MODE OBJFPC}{$LONGSTRINGS ON}{$INTERFACES CORBA}' + LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  ' + Owner.DeviceAncestorUnitName + ';' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  ' + Owner.DeviceType + ' = class(TDesign)' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Classes;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  RegisterClass(' + Owner.DeviceType + ');' + LineEnding +
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
      '{$MODE OBJFPC}{$LONGSTRINGS ON}{$INTERFACES CORBA}' + LineEnding +
      LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  ' + Owner.DeviceAncestorUnitName + ';' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  ' + Owner.DeviceType + ' = class(' + Owner.DeviceAncestorType + ')' + LineEnding +
      '  public' + LineEnding +
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
      '  {You may need to remove the following line}' + LineEnding +
      '  inherited Execute;' + LineEnding +
      'end;' + LineEnding +
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

function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
begin
  Result := CodeToolBoss.LoadFile(FileName, False, False);
  if not Assigned(Result) then begin
    if template <> cttNone then begin
      Result := CodeToolBoss.CreateFile(FileName);
    end;
    case template of
      cttSimulator: WriteSimulatorSourceTemplate(Owner, Result);
      cttDesign: WriteDesignSourceTemplate(Owner, Result);
      cttBlock,
      cttSource,
      cttProbe: WriteBlockSourceTemplate(Owner, Result);
    end;
  end;
end;

function GetCodeBuffer(template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
var
  CodeFileName: string;
begin
  case template of
  cttDescription: CodeFileName := ReSourceFileName(Owner.DeviceIdentifier);
  cttSimulator: CodeFileName := SourceFileName('Simulate' + Owner.DeviceIdentifier);
  else
    CodeFileName := SourceFileName(Owner.DeviceIdentifier);
  end;
  Result := GetCodeBuffer(CodeFileName, template, Owner);
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

procedure IncrementDeviceQty;
begin
  DeviceQty +=  1;
end;

procedure GuessNewDeviceNameAndType(var DeviceName, DeviceType, DeviceAncestorType: string);
var
  CodeFile: string;
  ACodeBuffer: TCodeBuffer;
begin
  if DeviceAncestorType = '' then begin
    if DeviceType = '' then begin
    end else begin
    end;
  end else begin
    if DeviceName = '' then begin
      if Pos('T', DeviceAncestorType) = 1 then begin
        DeviceName := Copy(DeviceAncestorType, 2, Length(DeviceAncestorType));
      end else begin
        DeviceName := 'A' + DeviceAncestorType;
      end;
      DeviceName := GetDeviceRandomName(DeviceName);
      if DeviceType = '' then begin
        DeviceType := 'T' + DeviceName;
      end;
    end else begin
      if DeviceType = '' then begin
        DeviceType := 'T' + DeviceName;
      end else begin
      end;
    end;
  end;
  if DeviceAncestorType = '' then begin
    CodeFile := SourceFileName(DeviceName);
    //codeFile[ctDescription] := DesignDir + BlockDescription.Name + '.lfm';
    ACodeBuffer := GetCodeBuffer(CodeFile, cttNone, nil);
    if Assigned(ACodeBuffer) then begin
      CodeToolBoss.FindFormAncestor(ACodeBuffer, DeviceType, DeviceAncestorType, True);
    end;
  end;
end;

procedure ResetDeviceQty;
begin
  DeviceQty := 0;
end;

end.

