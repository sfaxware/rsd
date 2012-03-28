unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  PProjectSettings = ^TProjectSettings;
  TProjectSettings = record
    Name: string;
    Path: string;
    BuildDir: string;
    Units: record
      Count: Word;
      SourceExt: string;
      ResourceExt:string;
    end;
  end;
  TAppCfg = record
    Prefix: string;
    Exec: record
      Name: string;
      Path: string;
    end;
    Lib: record
      Path: string;
    end;
    User: record
      Home: record
        Path: string
      end;
    end;
  end;

const
  AppCfg: TAppCfg = (
    Prefix: '';
    Exec: (Name: ''; Path: 'bin' + PathDelim);
    Lib: (Path: 'lib' + PathDelim);
    User: (Home: (Path: ''));
  );

var
  ProjectSettings: TProjectSettings;

function ReSourceFileName(BlockName: string): string;
function SourceFileName(BlockName: string): string;

implementation

uses
  FileUtil;

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

initialization
  with AppCfg do begin
    Exec.Name := ParamStr(0);
    Prefix := ExtractFileDir(ExtractFileDir(Exec.Name));
    Exec.Name := ExtractFileNameOnly(Exec.Name);
    Lib.Path += Exec.Name + PathDelim;
    ChDir(Prefix);
    User.Home.Path := AppendPathDelim(GetEnvironmentVariable('HOME'));
  end;
end.

