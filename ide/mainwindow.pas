unit mainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, process, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
  SynEdit, RTTICtrls, XMLCfg, CodeCache, LFMTrees, DesignGraph;

type
  { TIdeMainWindow }

  TIdeMainWindow = class(TForm)
    AddBlockMenuItem: TMenuItem;
    AddInputPortMenuItem: TMenuItem;
    AddOutputPortMenuItem: TMenuItem;
    AddProbeMenuItem: TMenuItem;
    AddSourceMenuItem: TMenuItem;
    DesignColorMenuItem: TMenuItem;
    BlockColorMenuItem: TMenuItem;
    DesignNameMenuItem: TMenuItem;
    BlockNameMenuItem: TMenuItem;
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
    RandomSourceMenuItem: TMenuItem;
    IdeViewLayoutMenuItem: TMenuItem;
    IdeViewSourceCodeMenuItem: TMenuItem;
    ConnectorPopupMenu: TPopupMenu;
    PortsSubMenu: TMenuItem;
    BuilderProcess: TProcess;
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
    Design: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl: TTabControl;
    Project: TXMLConfig;
    procedure AddInputPortMenuItemClick(Sender: TObject);
    procedure AddOutputPortMenuItemClick(Sender: TObject);
    procedure CompileProject(Sender: TObject);
    procedure ConnectPorts(Sender: TObject);
    procedure DeleteConnector(Sender: TObject);
    procedure dtslEditGraphInsertFileReadSourceMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertRandomSourceMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IdeViewLayoutMenuItemClick(Sender: TObject);
    procedure IdeViewSourceCodeMenuItemClick(Sender: TObject);
    procedure NewProject(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure SetBlockProperties(Sender: TObject);
    procedure ViewFile(Sender: TObject);
    procedure SelectBlockColor(Sender: TObject);
    procedure SelectBlockName(Sender: TObject);
    procedure SetBlockColor(Sender: TObject);
    procedure UpdateInstalledPackages(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure IdeEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure IdeEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure IdeFileExitMenuItemClick(Sender: TObject);
  private
    EditorCodeBuffer: TCodeBuffer;
    function SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
    procedure AddNewBlock(ADeviceType: string);
    procedure AddNewConnector(ADeviceType: string);
    procedure HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetupChildrenEvents(Sender: TObject);
  public
    destructor Destroy; override;
  end;

var
  IdeMainWindow: TIdeMainWindow;

implementation

uses
  StdCodeTools, CodeToolManager, LinkScanner, CodeWriter, Configuration, BlockProperties, PackagesManager;

{ TIdeMainWindow }

procedure TIdeMainWindow.IdeFileExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TIdeMainWindow.IdeViewLayoutMenuItemClick(Sender: TObject);
begin
  TabControl.TabIndex := 0;
end;

procedure TIdeMainWindow.IdeViewSourceCodeMenuItemClick(Sender: TObject);
begin
  TabControl.TabIndex := 1;
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
    Core.Path := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../../core/');
    //WriteLn('Core.Path = "', Core.Path, '"');
    Self.Caption := 'R.S.D. IDE (' +  Name + ')';
  end;
  with Project do begin
    Filename := '';
  end;
  Design.Cleanup;
end;

procedure TIdeMainWindow.LoadProject(Sender: TObject);
var
  p: Integer;
begin
  with Project, ProjectSettings do begin
    if OpenDialog1.Execute then
      FileName := OpenDialog1.FileName
    else
      Exit;
    Path := ExtractFilePath(FileName);
    Units.Count := GetValue('ProjectOptions/Units/Count', 0);
    if Units.Count > 1 then begin
      Name := GetValue('ProjectOptions/Units/Unit1/UnitName/Value', 'Design');
    end;
    Self.Caption := 'D.T.S.L. IDE (' + Name + ')';
    WriteLn('Core.Path = "', Core.Path, '"');
    BuildDir := GetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', 'build');
  end;
  with Design do begin
    Cleanup;
    Load;
  end;
  ViewFile(Design);
  TabControl.TabIndex := 0;
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
    SetValue('ProjectOptions/General/MainUnit/Value', 0);
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
    SetValue('ProjectOptions/RequiredPackages/Count', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/PackageName/Value', 'rsdcore');
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Valid', True);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Valid', True);
    //SetValue('ProjectOptions/RequiredPackages/Item1/DefaultFilename/Value', );
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
  Design.Save;
end;

procedure TIdeMainWindow.SetBlockProperties(Sender: TObject);
begin
  with BlockPropertiesDialog do begin
    if Design.PointedDevice is TBlock then begin
      Device := Design.PointedDevice as TBlock;
      Visible := True;
    end;
  end;
end;

procedure TIdeMainWindow.SelectBlockColor(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TIdeMainWindow.SelectBlockName(Sender: TObject);
begin
  with Design.PointedDevice do begin
    Caption := InputBox('Change block name', 'Please type the new block name', Caption);
    Invalidate;
  end;
end;

procedure TIdeMainWindow.SetBlockColor(Sender: TObject);
begin
  if Sender is TColorDialog then with Sender as TColorDialog do begin
    //WriteLn('Change Color from ', hexStr(Design.PointedDevice.Canvas.Brush.Color, 8), ' to ', hexStr(Color, 8));
    Design.PointedDevice.Canvas.Brush.Color := Color;
    Invalidate;
  end;
end;

procedure TIdeMainWindow.FormCreate(Sender: TObject);
begin
  with CodeToolBoss do begin
    OnSearchUsedUnit := @SearchUsedUnit;
  end;
  with Design do begin
    OnChildrenCreate := @SetupChildrenEvents;
  end;
  NewProject(Sender);
end;

procedure TIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
begin
  //WriteLn('Sender.ClassName = ', Sender.ClassName);
  if Design.PointedDevice is TBlock then with Design.PointedDevice as TBlock do begin
    AddNewPort(TInputPort, '');
  end;
end;

procedure TIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
begin
  if Design.PointedDevice is TBlock then with Design.PointedDevice as TBlock do begin
    AddNewPort(TOutputPort, '');
  end;
end;

procedure TIdeMainWindow.CompileProject(Sender: TObject);
begin
  SaveProject(Sender);
  with BuilderProcess do begin
    CommandLine := 'lazbuild ' + Project.FileName;
    try
      Execute;
    except
      ShowMessage('Could not find Lazarus builder, please ensure "lazbuild" is in your ${PATH}');
      Exit;
    end;
    case ExitStatus of
      0: ShowMessage('Design compiled successfully');
      2: ShowMessage('Design failed to compile, please check compiler error message');
    else
      ShowMessage('Design compilation failed with error code ' + IntToStr(ExitStatus));
    end;
  end;
end;

procedure TIdeMainWindow.ConnectPorts(Sender: TObject);
begin
  AddNewConnector('TConnector');
end;

procedure TIdeMainWindow.DeleteConnector(Sender: TObject);
begin
  with Design do begin
    if Assigned(PointedDevice) and (PointedDevice is TConnector) then  begin
      DeleteConnector(TConnector(PointedDevice));
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
        Design.Visible := True;
      end;
      1:begin
        Design.Visible := False;
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
  DirList := ProjectSettings.Core.Path + 'block' + PathSep + ProjectSettings.Core.Path + 'fifo';
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
  SameBuffer := (EditorCodeBuffer = Design.CodeBuffer[ctSource]) and Assigned(EditorCodeBuffer);
  if SameBuffer and SynEdit1.Modified then begin
    EditorCodeBuffer.Source := SynEdit1.Text;
  end;
  with Design do begin
    SelectBlock(AddNewBlock('', '', ADeviceType));
  end;

  if SameBuffer then begin
    SynEdit1.Text := EditorCodeBuffer.Source;
  end;
end;

procedure TIdeMainWindow.AddNewConnector(ADeviceType: string);
begin
  Design.AddNewConnector('', ADeviceType);
end;

procedure TIdeMainWindow.HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('TPort.HandleMouseDownEvents');
  case Button of
    mbLeft:with Design do begin
      if Sender is TOutputPort then
       SelectedOutputPort := Sender as TOutputPort
      else
       SelectedOutputPort := nil;
    end;
  end;
end;

procedure TIdeMainWindow.HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  //WriteLn('TPort.HandleMouseUpEvents');
  case Button of
    mbLeft:with Design do begin
      if (Sender is TInputPort) and (Assigned(SelectedOutputPort)) then begin
        SelectedInputPort := Sender as TInputPort;
        //WriteLn('SelectedOutputPort = ', SelectedOutputPort.Top, ', ', SelectedOutputPort.Left);
        //WriteLn('SelectedInputPort = ', SelectedInputPort.Top, ', ', SelectedInputPort.Left);
        ConnectPorts(Self);
      end else
        SelectedInputPort := nil;
    end;
  end;
end;

procedure TIdeMainWindow.SetupChildrenEvents(Sender: TObject);
begin
  if Sender is TBlock then with Sender as TBlock do begin
    OnDblClick := @ViewFile;
    PopupMenu := BlockPopupMenu;
    OnChildrenCreate := @SetupChildrenEvents;
    if Sender is TSource then with Sender as TSource do begin
       SetupChildrenEvents(FindComponent('Output'));
    end else if Sender is TProbe then with Sender as TProbe do begin
       SetupChildrenEvents(FindComponent('Input'));
    end;
  end else if Sender is TConnector then with Sender as TConnector do begin
    OnDblClick := @ViewFile;
    PopupMenu := ConnectorPopupMenu;
  end else if Sender is TInputPort then with Sender as TInputPort do begin
    OnMouseUp := @HandleMouseUpEvents;
  end else if Sender is TOutputPort then with Sender as TOutputPort do begin
    OnMouseDown := @HandleMouseDownEvents;
  end;
end;

destructor TIdeMainWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TIdeMainWindow.ViewFile(Sender: TObject);
var
  CodeFileName: string;
  GraphDevice: TIGraphDevice;
  CodeTemplate: TCodeTemplateType;
  CodeBuffer: TCodeBuffer;
  DeviceName: string;
begin
  with SynEdit1 do begin
    if Assigned(EditorCodeBuffer) and Modified then begin
       EditorCodeBuffer.Source := Text;
    end;
    if Sender is TBlock then begin
      GraphDevice := Sender as TBlock;
      CodeTemplate := cttBlock;
    end else if Sender is TDesign then with Sender as TDesign do begin
      GraphDevice := Sender as TDesign;
      CodeTemplate := cttDesign;
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
    SetFocus;
  end;
end;

procedure TIdeMainWindow.IdeEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if Assigned(Design.SelectedBlock) then begin
    //WriteLn('Removing block');
    Design.DestroyBlock(Design.SelectedBlock);
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

