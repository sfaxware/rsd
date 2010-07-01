unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PProjectSettings = ^ TProjectSettings;
  TProjectSettings = record
    Name: string;
    Path: string;
    BuildDir: string;
    Units: record
      Count: Word;
      SourceExt: string;
      ResourceExt:string;
    end;
    Core: record
      Path: string;
    end;
  end;

var
  ProjectSettings: TProjectSettings;

function ReSourceFileName(BlockName: string): string;
function SourceFileName(BlockName: string): string;

implementation

function ReSourceFileName(BlockName: string): string;
begin
  with ProjectSettings do begin
    Result := Path + BlockName + '.' + Units.ResourceExt;
    //WriteLn('Path = ', Path, ', BlockName = ', BlockName, ', Units.SourceExt = ', Units.ResourceExt);
  end;
  //WriteLn('ReSourceFileName = ', Result);
end;

function SourceFileName(BlockName: string): string;
begin
  with ProjectSettings do begin
    Result := Path + BlockName + '.' + Units.SourceExt;
    //WriteLn('Path = ', Path, ', BlockName = ', BlockName, ', Units.SourceExt = ', Units.SourceExt);
  end;
  //WriteLn('SourceFileName = ', Result);
end;

end.

