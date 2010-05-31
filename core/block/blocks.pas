unit Blocks;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  BlockBasics;

type
  TInputPort = class(TCInputPort);
  TOutputPort = class(TCOutputPort);
  TConnector = class(TCConnector);
  TBlock = class(TCBlock)
  private
    FColor: Cardinal;
    FWidth: Cardinal;
    FHeight: Cardinal;
  public
    constructor Create(AOwner: TCBlock); override;
    constructor Create(AOwner: TBlock); virtual;
  published
    property Color: Cardinal read FColor write FColor;
    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;
  end;

implementation

uses
  Classes;

constructor TBlock.Create(AOwner: TCBlock);
begin
  if AOwner is TBlock then begin
    Create(AOwner as TBlock);
  end;
end;

constructor TBlock.Create(AOwner: TBlock);
var
  cn: string;
begin
  if Assigned(AOwner) then begin
    cn := AOwner.ClassName;
  end else begin
    cn := 'nil';
  end;
  WriteLn('>>TBlock.Create(AOwner: TBlock): Name = ', Name, ', AOwner.ClassName = ', cn);
  inherited Create(AOwner);
  WriteLn('<<TBlock.Create(AOwner: TBlock): Name = ', Name, ', AOwner.ClassName = ', cn);
end;

initialization
  RegisterClass(TInputPort);
  RegisterClass(TOutputPort);
  RegisterClass(TConnector);
  RegisterClass(TBlock);
end.

