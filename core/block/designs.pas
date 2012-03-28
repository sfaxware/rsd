unit Designs;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Blocks;

type
  TDesign = class(TBlock)
  public
    constructor Create(AOwner: TBlock); override;
    constructor Create(AOwner: TDesign); virtual;
    procedure Run;
  end;

implementation

uses
  Classes, LResources;

constructor TDesign.Create(AOwner: TBlock);
begin
  if AOwner is TDesign then begin
    Create(AOwner as TDesign);
  end;
end;

constructor TDesign.Create(AOwner: TDesign);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  WriteLn('>>TDesign.Create(AOwner: TDesign): Name = ', Name, ', AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  if not InitResourceComponent(Self, TDesign) then
    WriteLn('Failure');
  WriteLn('<<TDesign.Create(AOwner: TDesign): Name = ', Name, ', AOwner.ClassName = ', cn);
end;

procedure TDesign.Run;
begin
  repeat
    Execute;
  until True;//False;
end;

initialization
  RegisterClass(TDesign);

end.
