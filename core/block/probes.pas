unit Probes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Blocks;

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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; override;
  published
    property FileName: string read FFileName write SetFileName;
  end;

implementation

uses
  SysUtils, BlockBasics;

procedure TFileDumpProbe.SetFileName(AFileName: string);
var
  Size: LongInt;
begin
  //WriteLn(FuncB('TFileDumpProbe.SetFileName'), 'AFileNAme = ', AFileNAme);
  if FFileName = AFileNAme then
    Exit;
  if FFileName <> '' then begin
    Close(FFile);
    Size := FileSeek(GetFileHandle(FFile), 0, fsFromEnd);
    if Size <= 0 then begin
      Erase(FFile);
    end;
  end;
  FFileName := AFileName;
  System.Assign(FFile, FFileName);
  ReWrite(FFile);
  //WriteLn(FuncE('TFileDumpProbe.SetFileName'), 'FFileName = ', FFileName);
end;

constructor TFileDumpProbe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FileName := GetTempFileName;
end;

procedure TFileDumpProbe.Execute;
var
  Sample: Integer;
begin
  //WriteLn(FuncB('TFileDumpProbe.Execute'), 'IsEmpty = ', Input.IsEmpty);
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

initialization
  RegisterClass(TFileDumpProbe);

finalization

end.

