program BlockUnitTest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { add your units here }, BlockBasics, blocks, designs, probes, sources;

begin
end.

