program dtsl_ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, mainWindow, BlockPropeties;

begin
  Application.Initialize;
  Application.CreateForm(TdtslIdeMainWindow, dtslIdeMainWindow);
  Application.CreateForm(TBlockPropertiesDialog, BlockPropertiesDialog);
  Application.Run;
end.

