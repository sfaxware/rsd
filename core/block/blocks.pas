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
    constructor Create(AOwner: TBlock); virtual;
  published
    property Color: Cardinal read FColor write FColor;
    property Width: Cardinal read FWidth write FWidth;
    property Height: Cardinal read FHeight write FHeight;
  end;

implementation
uses
  LResources;

constructor TBlock.Create(AOwner: TBlock);
begin
  inherited Create(AOwner);
  WriteLn('>>TBlock.Create: ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount, ', Name = ', Name);
  if Assigned(AOwner) then begin
    WriteLn('AOwner.Name = ', AOwner.Name);
  end;
  if not InitResourceComponent(Self, TBlock) then
    WriteLn('Failure');
  WriteLn('<<TBlock.Create: ClassName = ', ClassName, ', DeviceName = ', DeviceName, ', ComponentCount = ', ComponentCount, ', Name = ', Name);
end;

initialization
  RegisterClass(TInputPort);
  RegisterClass(TOutputPort);
  RegisterClass(TConnector);
  RegisterClass(TBlock);
end.

