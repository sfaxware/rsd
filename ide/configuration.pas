unit Configuration;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, XMLConf;

type

  { TSplashWindow }

  TSplashWindow = class(TForm)
    UserConfig: TXMLConfig;
    procedure SaveUserConfiguration(Sender: TObject);
    procedure UpdateConfiguration(Sender: TObject);
  end;
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
  TAppCfgProject = record
    Name: string;
    Path: string;
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
      Lang: string;
      Projects: record
        Last: TAppCfgProject;
      end;
    end;
  end;

const
  AppCfg: TAppCfg = (
    Prefix: '';
    Exec: (Name: ''; Path: 'bin' + PathDelim);
    Lib: (Path: 'lib' + PathDelim);
    User: (
      Home: (Path: '');
      Lang: 'C';
      Projects: (
        Last: (Name: ''; Path: '')
      );
    );
  );

var
  SplashWindow: TSplashWindow;
  ProjectSettings: TProjectSettings;

function ReSourceFileName(BlockName: string): string;
function SourceFileName(BlockName: string): string;
function ExtractFileNameOnly(filePath: string): string;

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

function ExtractFileNameOnly(filePath: string): string;
begin
  Result := ChangeFileExt(ExtractFileName(filePath), '');
end;

{ TSplashWindow }

procedure TSplashWindow.UpdateConfiguration(Sender: TObject);
begin
  with AppCfg, UserConfig do begin
    Exec.Name := ParamStr(0);
    Prefix := ExtractFilePath(ExtractFileDir(Exec.Name));
    Exec.Name := ExtractFileNameOnly(Exec.Name);
    Lib.Path += Exec.Name + PathDelim;
    ChDir(Prefix);
    User.Home.Path := GetEnvironmentVariable('HOME');
    Filename := ConcatPaths([User.Home.Path, '.' + Exec.Name]);
    User.Projects.Last.Path := GetValue('AppCfg/User/Projects/Last/Path', '');
  end;
end;

procedure TSplashWindow.SaveUserConfiguration(Sender: TObject);
begin
  with AppCfg, UserConfig do begin
    SetValue('AppCfg/User/Lang', User.Lang);
    SetValue('AppCfg/User/Projects/Last/Path', User.Projects.Last.Path);
    Flush;
  end;
end;

initialization
  {$R *.lfm}
end.
