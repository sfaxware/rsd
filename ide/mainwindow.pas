unit mainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, process, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  Menus, ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
  SynEdit, RTTICtrls, XMLCfg, CodeCache, LFMTrees, DesignGraph;

type
  { TdtslIdeMainWindow }

  TdtslIdeMainWindow = class(TForm)
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
    dtslIdeMainMenu: TMainMenu;
    dtslIdeFileMenuItem: TMenuItem;
    dtslIdeEditMenuItem: TMenuItem;
    dtslIdeFileNewMenuItem: TMenuItem;
    dtslIdeFileOpenMenuItem: TMenuItem;
    dtslIdeFileExitMenuItem: TMenuItem;
    CompileMenuItem: TMenuItem;
    CompileProjectMenuItem: TMenuItem;
    dtslEditGraphDeleteBlockMenuItem: TMenuItem;
    DeleteConnectorMenuItem: TMenuItem;
    FileReadSourceMenuItem: TMenuItem;
    RandomSourceMenuItem: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    ConnectorPopupMenu: TPopupMenu;
    PortsSubMenu: TMenuItem;
    BuilderProcess: TProcess;
    ViewMenuItem: TMenuItem;
    dtslEditGraphSubMenu: TMenuItem;
    dtslEditGraphInsertBlockMenuItem: TMenuItem;
    dtslEditCopyMenuItem: TMenuItem;
    dtslEditPastMenuItem: TMenuItem;
    dtslEditCutMenuItem: TMenuItem;
    dtslIdeFileSaveMenuItem: TMenuItem;
    dtslIdeConfigurationMenuItem: TMenuItem;
    dtslIdeConfigurationPathsMenuItem: TMenuItem;
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
    procedure NewProject(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure ViewFile(Sender: TObject);
    procedure SelectBlockColor(Sender: TObject);
    procedure SelectBlockName(Sender: TObject);
    procedure SetBlockColor(Sender: TObject);
    procedure SetCoreBlocksPath(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure dtslIdeFileExitMenuItemClick(Sender: TObject);
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
  dtslIdeMainWindow: TdtslIdeMainWindow;

implementation

uses
  StdCodeTools, CodeToolManager, LinkScanner, CodeWriter, Configuration;

{ TdtslIdeMainWindow }

procedure TdtslIdeMainWindow.dtslIdeFileExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TdtslIdeMainWindow.NewProject(Sender: TObject);
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
    Self.Caption := 'D.T.S.L. IDE (' +  Name + ')';
  end;
  with Project do begin
    Filename := '';
  end;
  Design.Cleanup;
end;

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
var
  p: Integer;
begin
  with Project, ProjectSettings do begin
    if OpenDialog1.Execute then
      FileName := OpenDialog1.FileName
    else
      Exit;
    BuilderProcess.CommandLine := 'lazbuild ' + FileName;
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

procedure TdtslIdeMainWindow.SaveProject(Sender: TObject);
var
  APath: string;
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
    SetValue('ProjectOptions/RequiredPackages/Item1/PackageName/Value', 'dtslcore');
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MaxVersion/Valid', True);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Minor', 1);
    SetValue('ProjectOptions/RequiredPackages/Item1/MinVersion/Valid', True);
    SetValue('CompilerOptions/SearchPaths/UnitOutputDirectory/Value', BuildDir);
    Flush;
  end;
  if Assigned(EditorCodeBuffer) then with SynEdit1 do begin
     EditorCodeBuffer.Source := Text;
     MarkTextAsSaved;
  end;
  Design.Save;
end;

procedure TdtslIdeMainWindow.SelectBlockColor(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TdtslIdeMainWindow.SelectBlockName(Sender: TObject);
begin
  with Design.PointedDevice do begin
    Caption := InputBox('Change block name', 'Please type the new block name', Caption);
    Invalidate;
  end;
end;

procedure TdtslIdeMainWindow.SetBlockColor(Sender: TObject);
begin
  if Sender is TColorDialog then with Sender as TColorDialog do begin
    //WriteLn('Change Color from ', hexStr(Design.PointedDevice.Canvas.Brush.Color, 8), ' to ', hexStr(Color, 8));
    Design.PointedDevice.Canvas.Brush.Color := Color;
    Invalidate;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  with CodeToolBoss do begin
    OnSearchUsedUnit := @SearchUsedUnit;
  end;
  with Design do begin
    OnChildrenCreate := @SetupChildrenEvents;
  end;
  NewProject(Sender);
end;

procedure TdtslIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
begin
  //WriteLn('Sender.ClassName = ', Sender.ClassName);
  if Design.PointedDevice is TBlock then with Design.PointedDevice as TBlock do begin
    AddNewPort(TInputPort, '');
  end;
end;

procedure TdtslIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
begin
  if Design.PointedDevice is TBlock then with Design.PointedDevice as TBlock do begin
    AddNewPort(TOutputPort, '');
  end;
end;

procedure TdtslIdeMainWindow.CompileProject(Sender: TObject);
begin
  SaveProject(Sender);
  with BuilderProcess do begin
    try
      Execute;
    except
      ShowMessage('Could not find Lazarus builder, please ensure "lazbuild" is in your ${PATH}');
    end;
    case ExitStatus of
      0: ShowMessage('Design compiled successfully');
      2: ShowMessage('Design failed to compile, please check compiler error message');
    else
      ShowMessage('Design compilation failed with error code ' + IntToStr(ExitStatus));
    end;
  end;
end;

procedure TdtslIdeMainWindow.ConnectPorts(Sender: TObject);
begin
  AddNewConnector('TConnector');
end;

procedure TdtslIdeMainWindow.DeleteConnector(Sender: TObject);
begin
  with Design do begin
    if Assigned(PointedDevice) and (PointedDevice is TConnector) then  begin
      DeleteConnector(TConnector(PointedDevice));
    end;
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertFileReadSourceMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TFileReadSource');
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TFileDumpProbe');
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertRandomSourceMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TRandomSource');
end;

procedure TdtslIdeMainWindow.TabControlChange(Sender: TObject);
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

function TdtslIdeMainWindow.SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
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

procedure TdtslIdeMainWindow.AddNewBlock(ADeviceType: string);
var
  SameBuffer: Boolean;
begin
  SameBuffer := (EditorCodeBuffer = Design.CodeBuffer[ctSource]) and Assigned(EditorCodeBuffer);
  if SameBuffer and SynEdit1.Modified then begin
    EditorCodeBuffer.Source := SynEdit1.Text;
  end;
  with Design do begin
    if Assigned(SelectedBlock) then
      SelectedBlock.Selected := False;
    SelectedBlock := AddNewBlock('', ADeviceType);
  end;

  if SameBuffer then begin
    SynEdit1.Text := EditorCodeBuffer.Source;
  end;
end;

procedure TdtslIdeMainWindow.AddNewConnector(ADeviceType: string);
begin
  Design.AddNewConnector('', ADeviceType);
end;

procedure TdtslIdeMainWindow.HandleMouseDownEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TdtslIdeMainWindow.HandleMouseUpEvents(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TdtslIdeMainWindow.SetupChildrenEvents(Sender: TObject);
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

destructor TdtslIdeMainWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
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

procedure TdtslIdeMainWindow.dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if Assigned(Design.SelectedBlock) then begin
    //WriteLn('Removing block');
    Design.DestroyBlock(Design.SelectedBlock);
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertBlockMenuItemClick(Sender:TObject);
begin
  AddNewBlock('TBlock');
end;

procedure TdtslIdeMainWindow.SetCoreBlocksPath(Sender: TObject);
begin
  with ProjectSettings, SelectDirectoryDialog1 do begin
    if Execute then begin
      Core.Path := FileName;
    end;
    //WriteLn('Core.Blocks.Path = ', Core.Path);
  end;
end;

initialization
  {$R mainwindow.lfm}

end.

