program rsd_ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  PackagesManager, ExtToolEditDlg,
  Forms, mainWindow, BlockProperties, Configuration;

var
  ProjectFileName: string;

begin
  if ParamCount > 0 then  begin
    ProjectFileName := ParamStr(1);
    if not (ProjectFileName[1] in AllowDirectorySeparators) then begin
      GetDir(0, ProjectFileName);
      ProjectFileName += DirectorySeparator + ParamStr(1);
    end;
  end;
  Application.Initialize;
  Application.CreateForm(TSplashWindow, SplashWindow);
  Application.CreateForm(TIdeMainWindow, IdeMainWindow);
  Application.CreateForm(TBlockPropertiesDialog, BlockPropertiesDialog);
  Application.CreateForm(TPackagesManagerForm, PackagesManagerForm);
  IdeMainWindow.LoadProject(projectFileName);
  Application.Run;
end.

