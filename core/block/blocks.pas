unit Blocks;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, BlockBasics;

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
    constructor Create(AOwner: TComponent); override;
  published
    property Color: Cardinal read FColor write FColor;
    property Width: Cardinal read FColor write FColor;
    property Height: Cardinal read FHeight write FHeight;
  end;

implementation
uses
  LResources;

constructor TBlock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InitResourceComponent(Self, TBlock);
end;

initialization
  RegisterClass(TInputPort);
  RegisterClass(TOutputPort);
  RegisterClass(TConnector);
  RegisterClass(TBlock);
end.

