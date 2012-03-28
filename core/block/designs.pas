unit Designs;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, Blocks;

type
  TDesign = class(TBlock)
  public
    constructor Create(AOwner: TComponent); override;
    procedure Run;
  end;

implementation

uses
  BlockBasics;

constructor TDesign.Create(AOwner: TComponent);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  //WriteLn(FuncB('TDesign.Create(AOwner: TDesign)'), 'Name = ', Name, ', AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  //WriteLn(FuncE('TDesign.Create(AOwner: TDesign)'), 'Name = ', Name, ', AOwner.ClassName = ', cn);
end;

procedure TDesign.Run;
begin
  repeat
    Execute;
  until drfTerminated in RunStatus;
end;

initialization
  RegisterClass(TDesign);

end.
