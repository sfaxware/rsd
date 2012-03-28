unit Probes;

{$mode objfpc}{$H+}

interface

uses
  BlocKs;

type
  TProbe = class(TBlock)
  published
    Input: TInputPort;
  end;

  TFileDumpProbe = class(TProbe)
  private
    FFile: Text;
    FFileName:string;
    procedure SetFileName(AFileName: string);
  public
    procedure Execute; override;
    destructor Destroy; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses
  BlockBasics;

procedure TFileDumpProbe.SetFileName(AFileName: string);
begin
  //WriteLn(FuncB('TFileDumpProbe.SetFileName'), 'AFileNAme = ', AFileNAme);
  if FFileName = AFileNAme then
    Exit;
  if FFileName <> '' then begin
    Close(FFile);
  end;
  FFileName := AFileName;
  System.Assign(FFile, FFileName);
  ReWrite(FFile);
  //WriteLn(FuncE('TFileDumpProbe.SetFileName'), 'FFileName = ', FFileName);
end;

procedure TFileDumpProbe.Execute;
var
  Sample: Integer;
begin
  //WriteLn(FuncB('TFileDumpProbe.Execute'));
  with Input do while not IsEmpty do begin
    Pop(Sample);
    //WriteLn(FuncC('TFileDumpProbe.Execute'), 'IsEmpty = ', IsEmpty, ', Sample = ', Sample);
    WriteLn(FFile, Sample);
  end;
  //WriteLn(FuncE('TFileDumpProbe.Execute'));
end;

destructor TFileDumpProbe.Destroy;
begin
  Close(FFile);
end;

end.

