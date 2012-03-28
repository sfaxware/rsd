unit Designs;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils, Blocks;

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
