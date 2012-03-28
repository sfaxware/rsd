unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree;

type
  TCodeTemplateType = (cttNone, cttSimulator, cttDesign, cttBlock);

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
procedure GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TComponent; var Self: TCodeBuffer);

implementation

uses
  DesignGraph, GraphComponents;

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
  e := System.Pos('Blocks;' + LineEnding, Self.Source);
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
      'procedure T' + Owner.Name + '.Execute;' + LineEnding +
      'begin;' + LineEnding +
      '  {Write here your code}' + LineEnding +
      'end;' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
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
      '  Designs;' + LineEnding +
      LineEnding +
      'type' + LineEnding +
      '  TCustom' + Owner.Name + ' = class(TDesign)' + LineEnding +
      '  end;' + LineEnding +
      LineEnding +
      'var' + LineEnding +
      '  ' + Owner.Name + ': TCustom' + Owner.Name + ';' + LineEnding +
      LineEnding +
      'implementation' + LineEnding +
      LineEnding +
      'initialization' + LineEnding +
      '  {$R *.lfm}' + LineEnding +
      '  ' + Owner.Name + 'Simulator := TCustomDesign.Create(''' + Owner.Name + ''');' + LineEnding +
      LineEnding +
      'finalization' + LineEnding +
      '  ' + Owner.Name + 'Simulator.Free;' + LineEnding +
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
      'begin' + LineEnding +
      '  ' + Owner.Name + 'Simulator.Run;' + LineEnding +
      'end.');
  end;
end;

procedure GetCodeBuffer(FileName: string; template: TCodeTemplateType; Owner: TComponent; var Self: TCodeBuffer);
begin
  if not Assigned(Self) then begin
    Self := TCodeCache.Create.LoadFile(FileName);
  end;
  if not Assigned(Self) then begin
    Self := TCodeCache.Create.CreateFile(FileName);
    case template of
      cttSimulator: WriteSimulatorSourceTemplate(Owner, Self);
      cttDesign: WriteDesignSourceTemplate(Owner, Self);
      cttBlock: WriteBlockSourceTemplate(Owner, Self);
    end;
  end;
end;

end.

