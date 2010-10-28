unit PackagesManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, StdCtrls, SysUtils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs;

type

  { TPackagesManagerForm }

  TPackagesManagerForm = class(TForm)
    CancelButton: TButton;
    ApplyButton: TButton;
    PackagesListCheckGroup: TCheckGroup;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpdateInstalledPackages(Sender: TObject);
  private
    PackagesList: TFPList;
  public
    function IndexOfPackage(PkgName: string): Integer;
    function PackageIsInstalled(PkgIndex: Integer): boolean;
    procedure InstallPackage(PkgIndex: Integer);
    procedure UninstallPackage(PkgIndex: Integer);
  end;

var
  PackagesManagerForm: TPackagesManagerForm;

implementation

uses
  Configuration, PackageLinks, PackageDefs, EnvironmentOpts, Laz_XMLCfg, CompilerOptions;

const
  PackagesQty = 2;
  PackagesList: array[1..PackagesQty] of string[32] = ('rsdcore', 'toto');

var
  PkgLinks: TPackageLinks;

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
begin
  PackagesList := BuildPackagesList;
  UpdateForm(PackagesList);
  EnvironmentOptions := TEnvironmentOptions.Create;
  GlobalBuildProperties := TGlobalBuildProperties.Create;
  with GlobalBuildProperties do begin
    AddStandardModes;
  end;
  PkgLinks:=TPackageLinks.Create;
  with PkgLinks do begin
    UpdateUserLinks;
    //DependencyOwnerGetPkgFilename:=@PkgLinksDependencyOwnerGetPkgFilename;
  end;
end;

procedure TPackagesManagerForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := False;
  UpdateInstalledPackages(Sender);
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
  end;
  FreeAndNil(PackagesList);
  FreeAndNil(PkgLinks);
  FreeAndNil(EnvironmentOptions);
  FreeAndNil(GlobalBuildProperties);
end;

procedure TPackagesManagerForm.FormShow(Sender: TObject);
var
  n: Integer;
begin
  with PackagesListCheckGroup, PackagesList do begin
    for n := 0 to Count - 1 do begin
      Checked[n] := PackageIsInstalled(n);
    end;
  end;
end;

function AddUserLink(const PkgFilename, PkgName: string): TPackageLink;
var
  NewPackage: TLazPackage;
  XmlConfig: TXMLConfig;
begin
  NewPackage := TLazPackage.Create;
  XmlConfig := TXMLConfig.Create(PkgFilename);
  with NewPackage do begin
    Filename := PkgFilename;
    LoadFromXMLConfig(XmlConfig, 'Package/');
  end;
  with PkgLinks do begin
    Result := AddUserLink(NewPackage);
  end;
  FreeAndNil(XmlConfig);
  FreeAndNil(NewPackage);
end;

procedure TPackagesManagerForm.UpdateInstalledPackages(Sender: TObject);
var
  n: Integer;
begin
  if Sender = ApplyButton then with PackagesListCheckGroup do begin
    for n := 0 to Items.Count - 1 do begin
      if Checked[n] then begin
        InstallPackage(n);
      end else begin
        UninstallPackage(n);
      end;
      with PkgLinks do begin
        UpdateUserLinks;
        SaveUserLinks;
      end;
    end;
  end else begin
  end;
  Visible := False;
end;

function TPackagesManagerForm.IndexOfPackage(PkgName: string): Integer;
var
  n: Integer;
  PkgPath: PString;
begin
  Result := -1;
  with PackagesList do begin
    for n := 0 to Count do begin
      PkgPath := Items[n];
      if ExtractFileNameOnly(PkgPath^) = PkgName then begin
        Exit(n)
      end;
    end;
  end;
end;

function TPackagesManagerForm.PackageIsInstalled(PkgIndex: Integer): boolean;
var
  PkgName: string;
  PkgPath: PString;
  PkgXmlPath: string;
  PkgLink: TPackageLink;
begin
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('PkgPath = "', PkgPath, '"');
  with PkgLinks do begin
    PkgLink := FindLinkWithPkgName(PkgName);
  end;
  if Assigned(PkgLink) then with PackagesList, PkgLink do begin
    PkgPath := Items[PkgIndex];
    Result := Filename = PkgPath^;
  end else begin
    Result := False;
  end;
end;

procedure TPackagesManagerForm.InstallPackage(PkgIndex: Integer);
var
  n: Integer;
  PkgName: string;
  PkgPath: PString;
  PkgXmlPath: string;
begin
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('PkgPath = "', PkgPath, '"');
  n := IndexOfPackage(PkgName);
  PkgPath := PackagesList.Items[PkgIndex];
  AddUserLink(PkgPath^, PkgName);
end;

procedure TPackagesManagerForm.UninstallPackage(PkgIndex: Integer);
var
  n: Integer;
  PkgName: string;
  PkgPath: PString;
  PkgXmlPath: string;
begin
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('PkgPath = "', PkgPath, '"');
  n := IndexOfPackage(PkgName);
  PkgLinks.RemoveLink(PkgLinks.FindLinkWithPkgName(PkgName), True);
end;

initialization
  {$R packagesmanager.lfm}
end.
