unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree;

type
  TCodeTemplateType = (cttNone, cttSimulator, cttDesign, cttBlock);

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TComponent): TCodeBuffer;
function GetUserCodePosition(BlockName: string; Self: TCodeBuffer):TPoint;

implementation

uses
  GraphComponents, CodeToolManager;

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
var
  i: Integer;
  Component: TComponent;
  p, e: Integer;
  usedBlocks: string;
begin
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
  usedBlocks := 'class(TDesign)' + LineEnding;
  p := System.Pos(usedBlocks, Self.Source);
  if p = 0 then
    Exit(False);
  e := System.Pos('end;' + LineEnding, Self.Source);
  //WriteLn('p = ', p, ', e = ', e);
  if e < p then
    Exit(False);
  //WriteLn('Block.Name = ', Block.Name, ', Block.ComponentCount = ', Block.ComponentCount);
  for i := 0 to Block.ComponentCount - 1 do begin
    Component := Block.Components[i];
    if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      usedBlocks += '    ' + Name + ': ' + 'T' + Name + ';' + LineEnding;
    end;
  end;
  //WriteLn('p = ', p, ', e = ', e);
  //WriteLn(usedBlocks);
  Self.Replace(p, e - p, usedBlocks + '  ');
  Result := True;
end;

procedure WriteBlockSourceTemplate(Owner: TComponent; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'unit ' + Owner.Name + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' +
      LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Blocks;' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  T' + Owner.Name + ' = class(TBlock)' + LineEnding +
      '    procedure Execute; override;' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Classes;' + LineEnding +
      LineEnding +
      'procedure T' + Owner.Name + '.Execute;' + LineEnding +
      'begin;' + LineEnding +
      '  {Write here your code}' + LineEnding +
      'end;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  RegisterClass(T' + Owner.Name + ');' + LineEnding +
      LineEnding +
      'finalization' + LineEnding +
      'end.');
  end;
end;

procedure WriteDesignSourceTemplate(Owner: TComponent; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'unit ' + Owner.Name + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' + LineEnding +
      'interface' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Blocks, Designs;' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  TCustom' + Owner.Name + ' = class(TDesign)' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  Classes;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  RegisterClass(T' + Owner.Name + ');' + LineEnding +
      LineEnding +
      'finalization' + LineEnding +
      LineEnding +
      'end.');
  end;
end;

procedure WriteSimulatorSourceTemplate(Owner: TComponent; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'program Simulate' + Owner.Name + ';' + LineEnding +
      '{$mode objfpc}{$H+}{$interfaces corba}' + LineEnding +
      LineEnding +
      'uses' + LineEnding +
      '  ' + Owner.Name + ';' + LineEnding +
      LineEnding +
      'var' + LineEnding +
      '  ' + Owner.Name + 'Simulator: TCustom' + Owner.Name + ';' + LineEnding +
      LineEnding +
      'begin' + LineEnding +
      '  ' + Owner.Name + 'Simulator := TCustomDesign.Create(nil);' + LineEnding +
      '  ' + Owner.Name + 'Simulator.Run;' + LineEnding +
      '  ' + Owner.Name + 'Simulator.Free;' + LineEnding +
      'end.');
  end;
end;

function GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TComponent): TCodeBuffer;
begin
  Result := CodeToolBoss.LoadFile(FileName, False, False);
  if not Assigned(Result) then begin
    Result := CodeToolBoss.CreateFile(FileName);
    case template of
      cttSimulator: WriteSimulatorSourceTemplate(Owner, Result);
      cttDesign: WriteDesignSourceTemplate(Owner, Result);
      cttBlock: WriteBlockSourceTemplate(Owner, Result);
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

