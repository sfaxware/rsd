program dtsl_ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, mainWindow, RunTimeTypeInfoControls;

begin
  Application.Initialize;
  Application.CreateForm(TdtslIdeMainWindow, dtslIdeMainWindow);
  Application.Run;
end.
