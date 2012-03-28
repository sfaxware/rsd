unit PackagesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, XMLCfg;

type

  { TPackagesManagerForm }

  TPackagesManagerForm = class(TForm)
    CancelButton: TButton;
    ApplyButton1: TButton;
    PackagesListCheckGroup: TCheckGroup;
    PackageFiles: TXMLConfig;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    PackagesList: TFPList;
  public
    function PackageIsInstalled(PkgPath: string): boolean;
  end; 

var
  PackagesManagerForm: TPackagesManagerForm;

implementation

uses
  Configuration;

const
  PackagesQty = 2;
  PackagesList: array[1..PackagesQty] of string[32] = ('rsdcore', 'toto');

{ TPackagesManagerForm }

function Compare(Item1, Item2: Pointer): Integer;
var
  S1, S2: PString;
begin
  S1 := Item1;
  S2 := Item2;
  Result := CompareStr(S1^, S2^);
end;

procedure TPackagesManagerForm.FormCreate(Sender: TObject);
  function BuildPackagesList: TFPList;
  var
    ProgPath, ProgDir, LibDir: string;
    SearchRec: TSearchRec;
    SearchPath, PackagePath: string;
  begin
    ProgPath := ParamStr(0);
    ProgDir := ExtractFileDir(ProgPath);
    LibDir := ExtractFileDir(ProgDir) + '/lib/' + ExtractFileNameOnly(ProgPath);
    //WriteLn('LibDir = "', LibDir, '"');
    SearchPath := LibDir + '/*';
    Result := TFPList.Create;
    if FindFirst(SearchPath, faDirectory, SearchRec) = 0 then with Result do begin
      repeat
        with SearchRec do begin
          PackagePath := LibDir + '/' + Name + '/rsd' + Name + '.lpk';
          if(Attr and faDirectory) = faDirectory then begin
            if FileExists(PackagePath) then begin
              Add(NewStr(PackagePath));
            end;
          end;
        end;
      until FindNext(SearchRec) <> 0;
      Sort(@Compare);
    end;
    FindClose(SearchRec);
  end;
  procedure UpdateForm(PackagesList: TFPList);
  var
    n: Integer;
    PackagePath: PString;
  begin
    with PackagesList do  begin
      for n := 0 to Count - 1 do begin
        PackagePath := Items[n];
        PackagesListCheckGroup.Items.Add(ExtractFileNameOnly(PackagePath^));
      end;
    end;
  end;
var
  n: Integer;
  PackagePath: string;
begin
  PackagesList := BuildPackagesList;
  UpdateForm(PackagesList);
  with PackageFiles do begin
    PackagePath := FileName;
    PackagePath := GetEnvironmentVariable('HOME') + PackagePath;
    FileName := PackagePath;
  end;
end;

procedure TPackagesManagerForm.FormDestroy(Sender: TObject);
var
  n: Integer;
  PackagePath: PString;
begin
  with PackagesList do  begin
    for n := 0 to Count - 1 do begin
      PackagePath := Items[n];
      DisposeStr(PackagePath);
    end;
    Destroy;
  end;
end;

procedure TPackagesManagerForm.FormShow(Sender: TObject);
var
  n: Integer;
  PkgPath: PString;
begin
  with PackagesListCheckGroup, PackagesList do begin
    for n := 0 to Count - 1 do begin
      PkgPath := Items[n];
      Checked[n] := PackageIsInstalled(PkgPath^);
    end;
  end;
end;

function TPackagesManagerForm.PackageIsInstalled(PkgPath: string): boolean;
var
  n: Integer;
  XmlPath: string;
begin
  Result := False;
  //WriteLn('PkgPath = "', PkgPath, '"');
  with PackageFiles do begin
    for n := 1 to GetValue('UserPkgLinks/Count', 0) do begin
      XmlPath := GetValue('UserPkgLinks/Item' + IntToStr(n) + '/Filename/Value', '');
      //WriteLn('XmlPath = "', XmlPath, '"');
      if XmlPath = PkgPath then begin
        Exit(True)
      end;
    end;
  end;
end;

initialization
  {$R packagesmanager.lfm}
end.
