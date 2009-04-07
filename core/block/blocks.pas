unit Blocks;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils, BlockBasics; 

type
  TInputPort = class(TCInputPort);
  TOutputPort = class(TCOutputPort);
  TConnector = class(TIConnector);
  TBlock = class(TCBlock)
  private
  public
    procedure Execute; virtual; abstract;
  published
    name: string;
    Color: LongWord;
    left: integer;
    top: integer;
    width: integer;
    height: integer;
  end;

implementation

end.

