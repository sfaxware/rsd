program rsd_ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainWindow, BlockProperties;

begin
  Application.Initialize;
  Application.CreateForm(TRsdIdeMainWindow, RsdIdeMainWindow);
  Application.CreateForm(TBlockPropertiesDialog, BlockPropertiesDialog);
  Application.Run;
end.

