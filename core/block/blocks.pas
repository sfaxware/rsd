unit Blocks;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes,BlockBasics;

type
  TInputPort = class(TCInputPort);
  TOutputPort = class(TCOutputPort);
  TConnector = class(TCConnector);
  TBlock = class(TCBlock)
  private
    FColor: Cardinal;
    FTop: Cardinal;
    FLeft: Cardinal;
    FWidth: Cardinal;
    FHeight: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Color: Cardinal read FColor write FColor;
    property Top: Cardinal read FTop write FTop;
    property Left: Cardinal read FLeft write FLeft;
    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;
  end;

implementation

constructor TBlock.Create(AOwner: TComponent);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  WriteLn(FuncB('TBlock.Create'), 'Name = ', Name, ', AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  WriteLn(FuncE('TBlock.Create'), 'Name = ', Name, ', AOwner.ClassName = ', cn);
end;

initialization
  RegisterClass(TInputPort);
  RegisterClass(TOutputPort);
  RegisterClass(TConnector);
  RegisterClass(TBlock);
end.

