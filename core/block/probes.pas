unit Probes;

{$mode objfpc}{$H+}

interface

uses
  Classes, Blocks, BlockBasics;

type
  TProbe = class(TBlock)
  private
    FSampleQty: Cardinal;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Execute; override;
  published
    Input: TInputPort;
    property SampleQty: Cardinal write FSampleQty;
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
  SysUtils;

constructor TProbe.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSampleQty := MaxUIntValue;
end;

procedure TProbe.Execute;
begin
  FSampleQty -= 1;
end;

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
  //WriteLn(FuncB('TFileDumpProbe.Execute'), 'IsEmpty = ', Input.IsEmpty, ', SampleQty = ', FSampleQty);
  with Input do begin
    Pop(Sample);
    //WriteLn(FuncC('TFileDumpProbe.Execute'), 'IsEmpty = ', IsEmpty, ', SampleQty = ', FSampleQty, ', Sample = ', Sample);
  end;
  WriteLn(FFile, Sample);
  FSampleQty -= 1;
  if FSampleQty <= 0 then begin
    Include(FRunStatus, drfTerminated);
  end;
  //WriteLn(FuncE('TFileDumpProbe.Execute'), 'SampleQty = ', FSampleQty, ', Terminated = ', drfTerminated in FRunStatus);
end;

destructor TFileDumpProbe.Destroy;
begin
  Close(FFile);
end;

initialization
  RegisterClass(TFileDumpProbe);

finalization

end.

