unit Blocks;

{$mode objfpc}{$H+}{$interfaces corba}

interface

uses
  Classes, SysUtils, BlockBasics;

type
  TInputPort = class(TCInputPort);
  TOutputPort = class(TCOutputPort);
  TConnector = class(TCConnector);
  TBlock = class(TCBlock)
  private
  public
  end;

implementation

end.

