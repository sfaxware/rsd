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

procedure TDesign.Run;
begin
  repeat
    Execute;
  until False;
end;

end.
