unit mainWindow;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
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
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    ConnectorPopupMenu: TPopupMenu;
    PortsSubMenu: TMenuItem;
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
    procedure DeleteConnector(Sender: TObject);
    procedure dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertSourceMenuItemClick(Sender: TObject);
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
    procedure AddNewBlock(BlockType: string);
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
  with Project, ProjectSettings do begin
    Path := GetTempDir;
    Name := 'Design';
    Units.Count := 0;
    Units.SourceExt := 'pas';
    Units.ResourceExt := 'lfm';
    Self.Caption := 'D.T.S.L. IDE (' +  Name + ')';
    Core.Path := ExpandFileName(ExtractFilePath(ParamStr(0)) + '../../core/');
    WriteLn('Core.Path = "', Core.Path, '"');
  end;
  Design.Cleanup;
end;

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
var
  APath: string;
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
    APath := GetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', '');
    p := Pos(';', APath);
    if p > 0 then begin
      Delete(APath, p, Length(APath));
    end;
    Core.Path := ExpandFileName(ExtractFilePath(Path + APath));
    WriteLn('Core.Path = "', Core.Path, '"');
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
//    WriteLn('Core.Blocks.Path = ', Core.Blocks.Path);
//    WriteLn('DesignDir = ', DesignDir);
    Apath := ExtractRelativepath(APath, Core.Path) + 'block;' + ExtractRelativepath(APath, Core.Path) + 'fifo;$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)';
//    WriteLn('realtive path = ', path);
    SetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', APath);
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
  WriteLn('Sender.ClassName = ', Sender.ClassName);
  if Design.PointedDevice is TCGraphBlock then with Design.PointedDevice as TCGraphBlock do begin
    AddNewPort(TCGraphInputPort);
  end;
end;

procedure TdtslIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
begin
  if Design.PointedDevice is TCGraphBlock then with Design.PointedDevice as TCGraphBlock do begin
    AddNewPort(TCGraphOutputPort);
  end;
end;

procedure TdtslIdeMainWindow.CompileProject(Sender: TObject);
begin

end;

procedure TdtslIdeMainWindow.DeleteConnector(Sender: TObject);
begin
  with Design do begin
    if Assigned(PointedDevice) and (PointedDevice is TCGraphConnector) then  begin
      DeleteConnector(TCGraphConnector(PointedDevice));
    end;
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertProbeMenuItemClick(Sender: TObject);
begin
  AddNewBlock('TFileDumpProbe');
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertSourceMenuItemClick(Sender: TObject);
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
  WriteLn('DirList = ', DirList);
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

procedure TdtslIdeMainWindow.AddNewBlock(BlockType: string);
begin
  if Assigned(Design.SelectedBlock) then
    Design.SelectedBlock.Selected := False;
  Design.SelectedBlock := Design.AddNewBlock('', BlockType);
end;

procedure TdtslIdeMainWindow.SetupChildrenEvents(Sender: TObject);
begin
  if Sender is TCGraphBlock then with Sender as TCGraphBlock do begin
    OnDblClick := @ViewFile;
    PopupMenu := BlockPopupMenu;
  end else if Sender is TCGraphConnector then with Sender as TCGraphConnector do begin
    OnDblClick := @ViewFile;
    PopupMenu := ConnectorPopupMenu;
  end;
end;

destructor TdtslIdeMainWindow.Destroy;
begin
  inherited Destroy;
end;

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
var
  CodeFileName: string;
begin
  with SynEdit1 do begin
    if Assigned(EditorCodeBuffer) then begin
       EditorCodeBuffer.Source := Text;
    end;
    if Sender is TCGraphBlock then with Sender as TCGraphBlock do begin
      try
        CodeFileName := ProjectSettings.Path + PathDelim + Name + '.pas';
        CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttBlock, Sender as TCGraphBlock);
        UpdateUsedBlocks(Sender as TCGraphBlock, CodeBuffer[ctSource]);
        EditorCodeBuffer := CodeBuffer[ctSource];
      except
        ShowMessage('Unable to save file')
      end
    end else if Sender is TCGraphDesign then with Sender as TCGraphDesign do begin
      try
        CodeFileName := ProjectSettings.Path + '/' + Name + '.pas';
        CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttDesign, Sender as TCGraphDesign);
        UpdateUsedBlocks(Sender as TCGraphDesign, CodeBuffer[ctSource]);
        EditorCodeBuffer := CodeBuffer[ctSource];
      except
        ShowMessage('Unable to save file')
      end;
    end;
    Text := EditorCodeBuffer.Source;
    TabControl.TabIndex := 1;
    with Sender as TComponent do begin
      CaretXY := GetUserCodePosition(Name, EditorCodeBuffer);
    end;
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
    WriteLn('Core.Blocks.Path = ', Core.Path);
  end;
end;

initialization
  {$I mainwindow.lrs}

end.

