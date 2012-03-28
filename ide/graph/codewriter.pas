unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree, GraphComponents;

function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
function GetCodeBuffer(template: TCodeTemplateType; Owner: TIGraphDevice): TCodeBuffer;
function GetUserCodePosition(BlockName: string; Self: TCodeBuffer):TPoint;

implementation

uses
  CodeToolManager, Configuration;

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
      '  ' + Owner.DeviceAncestorUnitName + ';' + LineEnding +
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

end.

