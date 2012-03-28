unit Designs;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Blocks;

type
  TDesign = class(TBlock)
    procedure Run;
  end;

implementation

uses
  Classes;

procedure TDesign.Run;
begin
  repeat
    Execute;
  until False;
end;

initialization
  RegisterClass(TDesign);

end.
