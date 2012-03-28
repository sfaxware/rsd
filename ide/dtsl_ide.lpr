program dtsl_ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainWindow;

begin
  Application.Initialize;
  Application.CreateForm(TdtslIdeMainWindow, dtslIdeMainWindow);
  Application.Run;
end.

