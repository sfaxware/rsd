unit Probe;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BlocKs;

type
  TProbe = class(TBlock)
  private
    FFile: Text;
    FFileName:string;
    procedure SetFileName(AFileName: string);
  public
    procedure Execute; override;
    property FileName: string read FFileName write SetFileName;
  end;

implementation

procedure TProbe.SetFileName(AFileNAme: string);
begin
  if FFileName = AFileNAme then
    Exit;
  if FFileName <> '' then begin
    Close(FFile);
  end;
  Assign(FFile, FFileName);
  ReWrite(FFile);
  FFileName := AFileNAme;
end;

procedure TProbe.Execute;
begin
  WriteLn(FFile, 'Input[0]');
end;

end.

