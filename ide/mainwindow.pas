unit mainWindow;

{$mode objfpc}{$H+}

interface

uses
  AsyncProcess, Classes, process, StdCtrls, SysUtils, LResources, Forms,
  Controls, Graphics, Dialogs, Menus, ExtCtrls, ComCtrls, SynHighlighterPas,
  SynCompletion, GraphComponents, SynEdit, XMLCfg, CodeCache, LFMTrees,
  DesignGraph;

type
  { TIdeMainWindow }

  TIdeMainWindow = class(TForm)
    AddBlockMenuItem: TMenuItem;
    AddInputPortMenuItem: TMenuItem;
    AddOutputPortMenuItem: TMenuItem;
    AddProbeMenuItem: TMenuItem;
    AddSourceMenuItem: TMenuItem;
    BuilderProcess: TAsyncProcess;
    DesignColorMenuItem: TMenuItem;
    BlockColorMenuItem: TMenuItem;
    DesignCaptionMenuItem: TMenuItem;
    BlockCaptionMenuItem: TMenuItem;
    DesignPropertiesMenuItem: TMenuItem;
    BlockPropertiesMenuItem: TMenuItem;
    BlocksSubMenu: TMenuItem;
    ColorDialog1: TColorDialog;
    ConnectPortsMenuItem: TMenuItem;
    DesignPopupMenu: TPopupMenu;
    BlockPopupMenu: TPopupMenu;
    IdeMainMenu: TMainMenu;
    IdeFileMenuItem: TMenuItem;
    IdeEditMenuItem: TMenuItem;
    IdeFileNewMenuItem: TMenuItem;
    IdeFileOpenMenuItem: TMenuItem;
    IdeFileExitMenuItem: TMenuItem;
    IdeCompileMenuItem: TMenuItem;
    IdeCompileProjectMenuItem: TMenuItem;
    IdeEditGraphDeleteBlockMenuItem: TMenuItem;
    DeleteConnectorMenuItem: TMenuItem;
    FileReadSourceMenuItem: TMenuItem;
    IdeOpenInLazarusMenuItem: TMenuItem;
    IdleTimer1: TIdleTimer;
    IdeHelpMenuItem: TMenuItem;
    IdeHelpLocalHelpMenuItem: TMenuItem;
    AboutMenuItem: TMenuItem;
    AddSubDesignMenuItem: TMenuItem;
    Label1: TLabel;
    MenuItem1: TMenuItem;
    IdeViewTopLayoutMenuItem: TMenuItem;
    ShowTopLayoutMenuItem: TMenuItem;
    IdeViewUpperLayoutMenuItem: TMenuItem;
    ShowUpperLayoutMenuItem: TMenuItem;
    MenuItem3: TMenuItem;
    RandomSourceMenuItem: TMenuItem;
    IdeViewLayoutMenuItem: TMenuItem;
    IdeViewSourceCodeMenuItem: TMenuItem;
    ConnectorPopupMenu: TPopupMenu;
    PortsSubMenu: TMenuItem;
    IdeViewMenuItem: TMenuItem;
    IdeEditGraphSubMenu: TMenuItem;
    IdeEditGraphInsertBlockMenuItem: TMenuItem;
    IdeEditCopyMenuItem: TMenuItem;
    IdeEditPastMenuItem: TMenuItem;
    IdeEditCutMenuItem: TMenuItem;
    IdeFileSaveMenuItem: TMenuItem;
    IdeConfigurationMenuItem: TMenuItem;
    IdeConfigurationInstalledPackagesMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    DesignLayout: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl: TTabControl;
    Project: TXMLConfig;
    DesignTreeView: TTreeView;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure AddInputPortMenuItemClick(Sender: TObject);
    procedure AddOutputPortMenuItemClick(Sender: TObject);
    procedure AddSubDesignMenuItemClick(Sender: TObject);
    procedure BuilderProcessTerminate(Sender: TObject);
    procedure CompileProject(Sender: TObject);
    procedure DeleteConnector(Sender: TObject);
    procedure DesignTreeViewClick(Sender: TObject);
    procedure DesignTreeViewSelectionChanged(Sender: TObject);
    procedure dtslEditGraphInsertFileReadSourceMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertRandomSourceMenuItemClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure IdeHelpLocalHelpMenuItemClick(Sender: TObject);
    procedure IdeOpenInLazarusMenuItemClick(Sender: TObject);
    procedure IdeViewLayoutMenuItemClick(Sender: TObject);
    procedure IdeViewSourceCodeMenuItemClick(Sender: TObject);
    procedure ShowTopLayout(Sender: TObject);
    procedure NewProject(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure SetBlockProperties(Sender: TObject);
    procedure ShowUpperLayout(Sender: TObject);
    procedure ViewDesign(Sender: TObject);
    procedure ViewFile(Sender: TObject);
    procedure SelectBlockColor(Sender: TObject);
    procedure SelectBlockName(Sender: TObject);
    procedure SetBlockColor(Sender: TObject);
    procedure UpdateInstalledPackages(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure IdeEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure IdeEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure ExitApplication(Sender: TObject);
  private
    FTopDesign: TDesign;
    FSavedWindowState: TWindowState;
    EditorCodeBuffer: TCodeBuffer;
    function SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
    procedure AddNewBlock(ADeviceType: string);
    procedure AddNewConnector(ADeviceType: string);
    procedure RunCompiler(ExecName: string);
    procedure HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleDragDropEvents(Sender, Source: TObject; X, Y: Integer);
    procedure HandleDragOverEvents(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure SetupChildrenEvents(Sender: TObject);
  public
    destructor Destroy; override;
    procedure LoadProject(ProjectFileName: string);
  end;

var
  IdeMainWindow: TIdeMainWindow;

implementation

uses
  StdCodeTools, CodeToolManager, LinkScanner, CodeWriter, Configuration, BlockProperties, PackagesManager;

resourcestring
  AppTitle = 'Rapid Simulator Designer (RSD)';
  AppAuthor = 'Mazen NEIFER';
  AppVersion = '0.0.1';
  AppAboutMessage = '%s' + LineEnding + 'Copyright (c) 2010 %s' + LineEnding + 'Version %s';

{ TIdeMainWindow }

procedure TIdeMainWindow.ExitApplication(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TIdeMainWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
end;

procedure TIdeMainWindow.IdeViewLayoutMenuItemClick(Sender: TObject);
begin
  TabControl.TabIndex := 0;
end;

procedure TIdeMainWindow.IdeViewSourceCodeMenuItemClick(Sender: TObject);
begin
  TabControl.TabIndex := 1;
end;

procedure TIdeMainWindow.ShowTopLayout(Sender: TObject);
begin
  ViewDesign(FTopDesign);
end;

procedure TIdeMainWindow.NewProject(Sender: TObject);
begin
  with ProjectSettings do begin
    Name := 'Design';
    Path := GetTempDir;
    BuildDir := 'build';
    Units.Count := 0;
    Units.SourceExt := 'pas';
    Units.ResourceExt := 'lfm';
    //WriteLn('Core.Path = "', Core.Path, '"');
    Self.Caption := 'R.S.D. IDE (' +  Name + ')';
  end;
  with Project do begin
    Filename := '';
  end;
  if Assigned(FTopDesign) then begin
    DesignTreeView.Items.Delete(FTopDesign.TreeNode);
    FreeAndNil(FTopDesign);
  end;
  CreateDevice(TDevice(FTopDesign), 'Design', 'TTopDesign', 'TDesign', DesignLayout);
  with FTopDesign do begin
    TreeNode := DesignTreeView.Items.AddChildObject(nil, Name, FTopDesign);
    OnChildrenCreate := @SetupChildrenEvents;
  end;
end;

procedure TIdeMainWindow.LoadProject(ProjectFileName: string);
begin
  NewProject(Self);
  if ProjectFileName <> '' then with Project, ProjectSettings do begin
    FileName := ProjectFileName;
    Path := ExtractFilePath(ProjectFileName);
    Units.Count := GetValue('ProjectOptions/Units/Count', 0);
    if Units.Count > 1 then begin
      Name := GetValue('ProjectOptions/Units/Unit1/UnitName/Value', 'Design');
    end;
    Self.Caption := 'D.T.S.L. IDE (' + Name + ')';
    BuildDir := GetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', 'build');
  end;
  with FTopDesign do begin
    Load;
  end;
  ViewFile(DesignLayout);
  TabControl.TabIndex := 0;
  ViewDesign(FTopDesign);
  DesignLayout.SetFocus;
end;

procedure TIdeMainWindow.LoadProject(Sender: TObject);
begin
  if OpenDialog1.Execute then begin
    LoadProject(OpenDialog1.FileName);
  end;
end;

procedure TIdeMainWindow.SaveProject(Sender: TObject);
var
  APath: string;
  BuildPath: string;
begin
  with Project, ProjectSettings do begin
    if Filename = '' then
      if SaveDialog1.Execute then
        FileName := SaveDialog1.FileName
      else
        Exit;
    APath := ExtractFilePath(FileName);
    Path := APath;
    if Name = '' then
      Name := ChangeFileExt(ExtractFileName(FileName), '');
    SetValue('ProjectOptions/PathDelim/Value', PathDelim);
    SetValue('ProjectOptions/General/SessionStorage/Value', 'InProjectDir');
    SetValue('ProjectOptions/General/MainUnit/Value', 0);
    SetValue('ProjectOptions/RequiredPackages/Count', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/PackageName/Value', 'rsdcore');
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Valid', True);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Valid', True);
    //SetValue('ProjectOptions/RequiredPackages/Item1/DefaultFilename/Value', );
    if Units.Count < 2 then
      Units.Count := 2;
    SetValue('ProjectOptions/Units/Count', Units.Count);
    SetValue('ProjectOptions/Units/Unit0/Filename/Value', 'Simulate' + Name + '.pas');
    SetValue('ProjectOptions/Units/Unit0/IsPartOfProject/Value', True);
    SetValue('ProjectOptions/Units/Unit0/UnitName/Value', 'Simulate' + Name);
    SetValue('ProjectOptions/Units/Unit0/EditorIndex/Value', 0);
    SetValue('ProjectOptions/Units/Unit0/Loaded/Value', True);
    SetValue('ProjectOptions/Units/Unit1/Filename/Value', Name + '.pas');
    SetValue('ProjectOptions/Units/Unit1/IsPartOfProject/Value', True);
    SetValue('ProjectOptions/Units/Unit1/UnitName/Value', Name);
    SetValue('ProjectOptions/Units/Unit1/EditorIndex/Value', 1);
    SetValue('ProjectOptions/Units/Unit1/Loaded/Value', True);
    //WriteLn('Core.Blocks.Path = ', Core.Blocks.Path);
    //WriteLn('DesignDir = ', DesignDir);
    SetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', BuildDir);
    Flush;
    BuildPath := Path + '/' + BuildDir;
    if not DirectoryExists(BuildPath) then begin
      if not CreateDir(BuildPath) then begin
        ShowMessage('Directory "' + BuildPath + '" could not be created');
      end;
    end;
  end;
  if Assigned(EditorCodeBuffer) then with SynEdit1 do begin
     EditorCodeBuffer.Source := Text;
     MarkTextAsSaved;
  end;
  FTopDesign.Save;
end;

procedure TIdeMainWindow.SetBlockProperties(Sender: TObject);
begin
  with BlockPropertiesDialog do begin
    if TDesign.GetViewed.PointedDevice is TBlock then begin
      Device := TDesign.GetViewed.PointedDevice as TBlock;
      Visible := True;
    end;
  end;
end;

procedure TIdeMainWindow.ShowUpperLayout(Sender: TObject);
begin
  ViewDesign(TDesign.GetViewed.Owner);
end;

procedure TIdeMainWindow.SelectBlockColor(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TIdeMainWindow.SelectBlockName(Sender: TObject);
begin
  with TDesign.GetViewed.PointedDevice as TBlock do begin
    Caption := InputBox('Change block name', 'Please type the new block name', Caption);
    TreeNode.Text := Caption;
    Invalidate;
  end;
end;

procedure TIdeMainWindow.SetBlockColor(Sender: TObject);
begin
  if Sender is TColorDialog then with Sender as TColorDialog do begin
    //WriteLn('Change Color from ', hexStr(DesignLayout.PointedDevice.Canvas.Brush.Color, 8), ' to ', hexStr(Color, 8));
    TDesign.GetViewed.PointedDevice.Canvas.Brush.Color := Color;
    Invalidate;
  end;
end;

procedure TIdeMainWindow.FormCreate(Sender: TObject);
begin
  with CodeToolBoss do begin
    OnSearchUsedUnit := @SearchUsedUnit;
  end;
  with OpenDialog1, AppCfg do begin
    InitialDir := User.Home.Path;
  end;
  with SaveDialog1, AppCfg do begin
    InitialDir := User.Home.Path;
  end;
  NewProject(Sender);
end;

procedure TIdeMainWindow.HandleDragDropEvents(Sender, Source: TObject; X, Y: Integer);
begin
  //WriteLn('TIdeMainWindow.FormDragDrop');
  with TDesign.GetViewed do begin
    SelectedInputPort := nil;
    if Assigned(SelectedOutputPort) then begin
      if Sender is TOutputPortRef then begin
        SelectedInputPort := Sender as TOutputPortRef;
        //WriteLn(SelectedInputPort.DeviceIdentifier);
      end else if Sender is TInputPort then begin
        SelectedInputPort := Sender as TInputPort;
        //WriteLn(SelectedInputPort.DeviceIdentifier);
      end;
    end;
    if Assigned(SelectedInputPort) then begin
      Self.AddNewConnector('TConnector');
    end;
  end;
end;

procedure TIdeMainWindow.HandleDragOverEvents(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  //WriteLn('TIdeMainWindow.FormDragOver( Sender.Name = ', TComponent(Sender).Name, ', Source.Name = ', TComponent(Source).Name, ')');
  Accept := (Sender is TInputPort) or (Sender is TOutputPortRef);
end;

procedure TIdeMainWindow.IdeHelpLocalHelpMenuItemClick(Sender: TObject);
var
  HelpPath: string;
  HelpBrowser: string;
begin
  HelpPath := ExpandFileName(AppCfg.Lib.Path + '../../share/' + AppCfg.Exec.Name + '/index.html');
  HelpBrowser := ExeSearch('x-www-browser', GetEnvironmentVariable('PATH'));
  //WriteLn(HelpBrowser);
  ExecuteProcess(HelpBrowser, HelpPath);
end;

procedure TIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
var
  PointedDevice: TDevice;
begin
  PointedDevice := TDesign.GetViewed.PointedDevice;
  if PointedDevice is TDesign then with PointedDevice as TDesign do begin
    AddNewPort('', 'TInputPortRef');
  end else if PointedDevice is TBlock then with PointedDevice as TBlock do begin
    AddNewPort('', 'TInputPort');
  end;
end;

procedure TIdeMainWindow.AboutMenuItemClick(Sender: TObject);
begin
  ShowMessage(Format(AppAboutMessage, [AppTitle, AppAuthor, AppVersion]));
end;

procedure TIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
var
  PointedDevice: TDevice;
begin
  PointedDevice := TDesign.GetViewed.PointedDevice;
  if PointedDevice is TDesign then with PointedDevice as TDesign do begin
    AddNewPort('', 'TOutputPortRef');
  end else if PointedDevice is TBlock then with PointedDevice as TBlock do begin
    AddNewPort('', 'TOutputPort');
  end;
end;

procedure TIdeMainWindow.AddSubDesignMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TDesign');
end;

procedure TIdeMainWindow.CompileProject(Sender: TObject);
begin
  RunCompiler('lazbuild');
end;

procedure TIdeMainWindow.IdeOpenInLazarusMenuItemClick(Sender: TObject);
begin
  RunCompiler('lazarus-ide');
end;

procedure TIdeMainWindow.RunCompiler(ExecName: string);
begin
  SaveProject(Self);
  with BuilderProcess do begin
    CommandLine := ExecName + ' ' + Project.FileName;
    try
      Execute;
    except
      ShowMessage('Could not find Lazarus builder, please ensure "' + ExecName + '" is in your ${PATH}');
      Exit;
    end;
    Active := True;
  end;
  FSavedWindowState := WindowState;
  WindowState := wsMinimized;
  IdleTimer1.Enabled := True;
  IdeFileSaveMenuItem.Enabled := False;
  IdeEditMenuItem.Enabled := False;
  IdeCompileMenuItem.Enabled := False;
end;

procedure TIdeMainWindow.BuilderProcessTerminate(Sender: TObject);
begin
  with BuilderProcess do begin
    if not Running then begin
      IdleTimer1.Enabled := False;
      WindowState := FSavedWindowState;
      case ExitStatus of
        0: ShowMessage('Design compiled successfully');
        2: ShowMessage('Design failed to compile, please check compiler error message');
      else
        ShowMessage('Design compilation failed with error code ' + IntToStr(ExitStatus));
      end;
      IdeCompileMenuItem.Enabled := True;
      IdeEditMenuItem.Enabled := True;
      IdeFileSaveMenuItem.Enabled := True;
    end;
  end;
end;

procedure TIdeMainWindow.DeleteConnector(Sender: TObject);
begin
  with TDesign.GetViewed do begin
    if Assigned(PointedDevice) and (PointedDevice is TConnector) then  begin
      DeleteConnector(TConnector(PointedDevice));
    end;
  end;
end;

procedure TIdeMainWindow.DesignTreeViewClick(Sender: TObject);
var
  TreeNode: TTreeNode;
  Block: TBlock;
begin
  TreeNode := DesignTreeView.Selected;
  if Assigned(TreeNode) then begin
    Block := TBlock(TreeNode.Data);
    if Assigned(Block) then begin
      if Block is TDesign then begin
        ViewDesign(Block);
        TabControl.TabIndex := 0;
      end else if Block is TBlock then begin
        ViewFile(Block);
      end;
    end;
  end;
end;

procedure TIdeMainWindow.DesignTreeViewSelectionChanged(Sender: TObject);
var
  Block: TBlock;
begin
  if Assigned(DesignTreeView.Selected) then begin
    Block := TBlock(DesignTreeView.Selected.Data);
    if Assigned(Block) then with Block do begin
      if not Selected and (Owner is TDesign) then with Owner as TDesign do begin
        SelectBlock(Block);
      end;
    end;
  end;
end;

procedure TIdeMainWindow.dtslEditGraphInsertFileReadSourceMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TFileReadSource');
end;

procedure TIdeMainWindow.dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TFileDumpProbe');
end;

procedure TIdeMainWindow.dtslEditGraphInsertRandomSourceMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TRandomSource');
end;

procedure TIdeMainWindow.TabControlChange(Sender: TObject);
begin
  with Sender as TTabControl do begin
    case TabIndex of
      0:begin
        SynEdit1.Visible := False;
        DesignLayout.Visible := True;
      end;
      1:begin
        DesignLayout.Visible := False;
        SynEdit1.Visible := True;
      end;
    end;
  end;
end;

function TIdeMainWindow.SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
var
  FileName: string;
  DirList: string;
begin
  //WriteLn('SrcFilename = ', SrcFilename);
  //WriteLn('TheUnitName = ', TheUnitName);
  //WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  DirList := AppCfg.Lib.Path + 'block' + PathSep + AppCfg.Lib.Path + 'fifo';
  //WriteLn('DirList = ', DirList);
  FileName := TheUnitInFilename;
  if FileName = '' then begin
    FileName := LowerCase(TheUnitName) + '.pas';
    //WriteLn('TheUnitInFilename := ', FileName);
  end;
  FileName := FileSearch(FileName, DirList);
  //WriteLn('FileName = ', FileName);
  if FileName = '' then
    Result := nil
  else
    Result := GetCodeBuffer(FileName, cttNone, nil);
end;

procedure TIdeMainWindow.AddNewBlock(ADeviceType: string);
var
  SameBuffer: Boolean;
begin
  SameBuffer := (EditorCodeBuffer = TDesign.GetViewed.CodeBuffer[ctSource]) and Assigned(EditorCodeBuffer);
  if SameBuffer and SynEdit1.Modified then begin
    EditorCodeBuffer.Source := SynEdit1.Text;
  end;
  with TDesign.GetViewed do begin
    SelectBlock(AddNewBlock('', '', ADeviceType));
  end;

  if SameBuffer then begin
    SynEdit1.Text := EditorCodeBuffer.Source;
  end;
end;

procedure TIdeMainWindow.AddNewConnector(ADeviceType: string);
begin
  TDesign.GetViewed.AddNewConnector('', ADeviceType);
end;

procedure TIdeMainWindow.HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('HandleMouseDownEvents(Sender.Name = ', TComponent(Sender).Name, ')');
  case Button of
    mbLeft: with TDesign.GetViewed do begin
      SelectedOutputPort := nil;
      if Sender is TInputPortRef then begin
        SelectedOutputPort := Sender as TInputPortRef;
        //WriteLn(SelectedOutputPort.DeviceIdentifier);
      end else if Sender is TOutputPort then begin
        SelectedOutputPort := Sender as TOutputPort;
        //WriteLn(SelectedOutputPort.DeviceIdentifier);
      end;
      if Assigned(SelectedOutputPort) then with Sender as TDevice do begin
        BeginDrag(False);
      end;
    end;
  end;
end;

procedure TIdeMainWindow.HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('HandleMouseUpEvents(Sender.Name = ', TComponent(Sender).Name, ')');
  with TDesign.GetViewed do begin
    SelectedOutputPort := nil;
    SelectedInputPort := nil;
  end;
end;

procedure TIdeMainWindow.SetupChildrenEvents(Sender: TObject);
begin
  if Sender is TBlock then with Sender as TBlock do begin
    if Sender is TDesign then begin
      OnDblClick := @ViewDesign;
    end else begin
      OnDblClick := @ViewFile;
    end;
    PopupMenu := BlockPopupMenu;
    OnChildrenCreate := @SetupChildrenEvents;
    if Sender is TSource then with Sender as TSource do begin
       SetupChildrenEvents(FindComponent('Output'));
    end else if Sender is TProbe then with Sender as TProbe do begin
       SetupChildrenEvents(FindComponent('Input'));
    end;
    TreeNode := DesignTreeView.Items.AddChildObject(TBlock(Owner).TreeNode, Name, Sender);
  end else if Sender is TConnector then with Sender as TConnector do begin
    PopupMenu := ConnectorPopupMenu;
  end else if Sender is TPort then with Sender as TPort do begin
    //WriteLn('SetupChildrenEvents for ', DeviceIdentifier, ': ', ClassName);
    OnMouseUp := @HandleMouseUpEvents;
    OnMouseDown := @HandleMouseDownEvents;
    OnDragOver := @HandleDragOverEvents;
    OnDragDrop := @HandleDragDropEvents;
  end;
end;

destructor TIdeMainWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TIdeMainWindow.ViewDesign(Sender: TObject);
begin
  with Sender as TDesign do begin
    SetViewed(True);
  end;
  ShowUpperLayoutMenuItem.Enabled := Sender <> FTopDesign;
  ShowTopLayoutMenuItem.Enabled := Sender <> FTopDesign;
  IdeViewUpperLayoutMenuItem.Enabled := Sender <> FTopDesign;
  IdeViewTopLayoutMenuItem.Enabled := Sender <> FTopDesign;
end;

procedure TIdeMainWindow.ViewFile(Sender: TObject);
var
  CodeFileName: string;
  GraphDevice: IDevice;
  CodeTemplate: TCodeTemplateType;
  CodeBuffer: TCodeBuffer;
  DeviceName: string;
begin
  if Sender = DesignLayout then begin
    Sender := TDesign.GetViewed;
  end;
  if Sender = FTopDesign then with Sender as TDesign do begin
    GraphDevice := Sender as TDevice;
    CodeTemplate := cttDesign;
  end else if Sender is TBlock then begin
    GraphDevice := Sender as TDevice;
    CodeTemplate := cttBlock;
  end else begin
    GraphDevice := nil;
  end;
  with SynEdit1 do begin
    if Assigned(EditorCodeBuffer) and Modified then begin
       EditorCodeBuffer.Source := Text;
    end;
    if Assigned(GraphDevice) then with Sender as TComponent do begin
      DeviceName := Name;
      CodeFileName := SourceFileName(DeviceName);
      CodeBuffer := GetCodeBuffer(CodeFileName, CodeTemplate, GraphDevice);
    end;
    if EditorCodeBuffer <> CodeBuffer then begin
      EditorCodeBuffer := CodeBuffer;
      Text := EditorCodeBuffer.Source;
      CaretXY := GetUserCodePosition(DeviceName, EditorCodeBuffer);
    end;
    TabControl.TabIndex := 1;
    EnsureCursorPosVisible;
  end;
end;

procedure TIdeMainWindow.IdeEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if Assigned(TDesign.GetViewed.SelectedBlock) then begin
    //WriteLn('Removing block');
    TDesign.GetViewed.DestroyBlock(TDesign.GetViewed.SelectedBlock);
  end;
end;

procedure TIdeMainWindow.IdeEditGraphInsertBlockMenuItemClick(Sender:TObject);
begin
  AddNewBlock('TBlock');
end;

procedure TIdeMainWindow.UpdateInstalledPackages(Sender: TObject);
begin
  with PackagesManagerForm do begin
    Show
  end;
end;

initialization
  {$R mainwindow.lfm}

end.
