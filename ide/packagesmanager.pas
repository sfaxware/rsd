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
    procedure UpdatePackagesInstallState(Sender: TObject);
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
  Configuration, PackageLinks, PackageLinkIntf, PackageDefs, EnvironmentOpts,
  CompilerOptions, TransferMacros, CodeToolManager, DefineTemplates,
  Laz_XMLCfg, CodeCache, DialogProcs;

var
  PkgLinks: TLazPackageLinks;

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
    SearchRec: TSearchRec;
    SearchPath, PackagePath: string;
  begin
    with AppCfg do begin
      SearchPath := Prefix + Lib.Path + '*';
    end;
    //WriteLn('AppCfg.Lib.Path = "', AppCfg.Lib.Path, '"');
    Result := TFPList.Create;
    if FindFirst(SearchPath, faDirectory, SearchRec) = 0 then with Result do begin
      repeat
        with SearchRec, AppCfg do begin
          PackagePath := Prefix + Lib.Path + Name + PathDelim + 'rsd' + Name + '.lpk';
          //WriteLn('PackagePath = ', PackagePath);
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
  GlobalMacroList:=TTransferMacroList.Create;
  {GlobalBuildProperties := TGlobalBuildProperties.Create;
  with GlobalBuildProperties do begin
    AddStandardModes;
  end;}
  PkgLinks:=TLazPackageLinks.Create;
  with PkgLinks do begin
    UpdateUserLinks;
    //DependencyOwnerGetPkgFilename:=@PkgLinksDependencyOwnerGetPkgFilename;
  end;
  {Emulate an apply button pressed to update packages including adding units
   path of already installed packages to compiler units search path.}
  UpdatePackagesInstallState(Sender);
  UpdateInstalledPackages(ApplyButton);
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
  FreeAndNil(GlobalMacroList);
  FreeAndNil(EnvironmentOptions);
  //FreeAndNil(GlobalBuildProperties);
end;

procedure TPackagesManagerForm.UpdatePackagesInstallState(Sender: TObject);
var
  n: Integer;
begin
  with PackagesListCheckGroup, PackagesList do begin
    for n := 0 to Count - 1 do begin
      Checked[n] := PackageIsInstalled(n);
    end;
  end;
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
  PkgLink: TPackageLink;
begin
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('PkgName = "', PkgName, '"');
  with PkgLinks do begin
    PkgLink := FindLinkWithPkgName(PkgName);
  end;
  if Assigned(PkgLink) then with PackagesList, PkgLink do begin
    PkgPath := Items[PkgIndex];
    //WriteLn('PkgPath = ', PkgPath^);
    //WriteLn('Filename = ', Filename);
    Result := GetEffectiveFilename = PkgPath^;
  end else begin
    Result := False;
  end;
end;

procedure TPackagesManagerForm.InstallPackage(PkgIndex: Integer);
var
  PkgName: string;
  PkgPath: PString;
  PkgLink: TPackageLink;
  Directory: String;
  UnitPathTemplate: TDefineTemplate;
  UnitPath: string;
  PkgFileName: String;
  Pkg: TLazPackage;
  XMLConfig: TXMLConfig;
  Code: TCodeBuffer;
begin
  //WriteLn('[TPackagesManagerForm.InstallPackage] PkgIndex = ', PkgIndex);
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('[TPackagesManagerForm.InstallPackage] PkgName = "', PkgName, '"');
  PkgIndex := IndexOfPackage(PkgName);
  //WriteLn('[TPackagesManagerForm.InstallPackage] PkgIndex = ', PkgIndex);
  PkgPath := PackagesList.Items[PkgIndex];
  //WriteLn('[TPackagesManagerForm.InstallPackage] PkgPath = "', PkgPath^, '"');
  PkgLink := PkgLinks.AddUserLink(PkgPath^, PkgName);
  Directory := ExtractFilePath(PkgPath^);
  Directory := Directory.Replace('/lib/rsd/', '/src/rsd/');
  //WriteLn('Directory="',Directory,'"');
  PkgFileName := PkgLink.GetEffectiveFilename;
  XMLConfig:=TXMLConfig.Create(nil);
  Pkg:=TLazPackage.Create;
  Pkg.Filename:=PkgFileName;
  if LoadXMLConfigFromCodeBuffer(PkgFileName,XMLConfig, Code,[lbfUpdateFromDisk,lbfRevert], False) = mrOK then begin
    Pkg.LoadFromXMLConfig(XMLConfig,'Package/');
    Pkg.LPKSource:=Code;
    UnitPath := ';' + Pkg.CompilerOptions.OtherUnitFiles;
    //WriteLn('Directory="', UnitPath,'"');
    UnitPath := '$(' + UnitPathMacroName + ')' + UnitPath.Replace(';', ';' + Directory );
    {add a sub template to extend the include search path #UnitPath.}
    UnitPathTemplate:=TDefineTemplate.Create(PkgName, PkgName + ' package units search path', UnitPathMacroName, UnitPath, da_DefineRecurse);
    {add the include path template to the tree.}
    CodeToolBoss.DefineTree.Add(UnitPathTemplate);
  end;
  //writeln('Directory="',Directory,'"', ' UnitPath="',CodeToolBoss.GetUnitPathForDirectory(Directory),'"');
  Code.free;
  XMLConfig.Free;
  Pkg.Free;
end;

procedure TPackagesManagerForm.UninstallPackage(PkgIndex: Integer);
var
  PkgName: string;
  PkgLink: TPackageLink;
begin
  //WriteLn('[TPackagesManagerForm.UninstallPackage] PkgIndex = "', PkgIndex, '"');
  PkgName := PackagesListCheckGroup.Items[PkgIndex];
  //WriteLn('[TPackagesManagerForm.UninstallPackage] PkgName = "', PkgName, '"');
  PkgLink := PkgLinks.FindLinkWithPkgName(PkgName);
  if Assigned(PkgLink) then begin
    PkgLinks.RemoveUserLink(PkgLink);
  end;
end;

initialization
  {$R packagesmanager.lfm}
end.
