unit CodeWriter;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls,CodeCache, CodeTree;

function UpdateUsedBlocks(Block: TComponent; Self: TCodeBuffer): Boolean;
procedure GetCodeBuffer(FileName: string; Owner: TComponent; var Self: TCodeBuffer);
procedure WriteSourceTemplate(Owner:TComponent; Self: TCodeBuffer);

implementation

uses
  GraphComponents;

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

procedure GetCodeBuffer(FileName: string; Owner: TComponent; var Self: TCodeBuffer);
begin
  if not Assigned(Self) then begin
    Self := TCodeCache.Create.LoadFile(FileName);
  end;
  if not Assigned(Self) then begin
    Self := TCodeCache.Create.CreateFile(FileName);
    with Self do begin
      WriteSourceTemplate(Owner, Self);
    end;
  end;
end;

procedure WriteSourceTemplate(Owner: TComponent; Self: TCodeBuffer);
begin
  with Self do begin
    Clear;
    Insert(SourceLength, 'unit ' + Owner.Name + ';' + LineEnding + LineEnding);
    Insert(SourceLength, '{$mode objfpc}{$H+}{$interfaces corba}' +
      LineEnding +
      'interface' + LineEnding +
      LineEnding);
    Insert(SourceLength, 'uses' + LineEnding + '  Blocks;' + LineEnding);
    Insert(SourceLength,   LineEnding +
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
      LineEnding +
      'end.');
  end;
end;

end.

