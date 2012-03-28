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
    ColorDialog1: TColorDialog;
    dtslIdeMainMenu: TMainMenu;
    dtslIdeFileMenuItem: TMenuItem;
    dtslIdeEditMenuItem: TMenuItem;
    dtslIdeFileNewMenuItem: TMenuItem;
    dtslIdeFileOpenMenuItem: TMenuItem;
    dtslIdeFileExitMenuItem: TMenuItem;
    AddOutputPortMenuItem: TMenuItem;
    ConnectPortsMenuItem: TMenuItem;
    BlockColorMenuItem: TMenuItem;
    BlocksSubMenu: TMenuItem;
    AddBlockMenuItem: TMenuItem;
    CompileMenuItem: TMenuItem;
    CompileProjectMenuItem: TMenuItem;
    PortsSubMenu: TMenuItem;
    dtslEditGraphDeleteBlockMenuItem: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    BlockPropertiesMenuItem: TMenuItem;
    BlockNameMenuItem: TMenuItem;
    AddInputPortMenuItem: TMenuItem;
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
    PopupMenu1: TPopupMenu;
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
    _ProjectSettings: pointer;
    EditorCodeBuffer: TCodeBuffer;
    function SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
  public
    destructor Destroy; override;
  end;

var
  dtslIdeMainWindow: TdtslIdeMainWindow;

implementation

uses
  StdCodeTools, CodeToolManager, LinkScanner, CodeWriter;

type
  PProjectSettings = ^ TProjectSettings;
  TProjectSettings = record
    Name: string;
    Units: record
      Count: Word;
    end;
    Core: record
      Blocks: record
        Path: string;
      end;
    end;
  end;

{ TdtslIdeMainWindow }

procedure TdtslIdeMainWindow.dtslIdeFileExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TdtslIdeMainWindow.NewProject(Sender: TObject);
var
  ProjectSettings: PProjectSettings;
begin
  ProjectSettings := _ProjectSettings;
  with Project, ProjectSettings^ do begin
    DesignDir := GetTempDir;
    Name := 'Untiteled';
    Units.Count := 0;
    Self.Caption := 'D.T.S.L. IDE (' +  Name + ')';
    Core.Blocks.Path := ExtractFilePath(ParamStr(0)) + '../../core/block';
    //WriteLn('Core.Blocks.Path = "', Core.Blocks.Path, '"');
  end;
  Design.Cleanup;
end;

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
var
  Path: string;
  p: Integer;
  ProjectSettings: PProjectSettings;
begin
  ProjectSettings := _ProjectSettings;
  with Project, ProjectSettings^ do begin
    if OpenDialog1.Execute then
      FileName := OpenDialog1.FileName
    else
      Exit;
    DesignDir := ExtractFilePath(FileName);
    Units.Count := GetValue('ProjectOptions/Units/Count', 0);
    if Units.Count > 1 then begin
      Name := GetValue('ProjectOptions/Units/Unit1/UnitName/Value', 'Design');
    end;
    Self.Caption := 'D.T.S.L. IDE (' + Name + ')';
    Path := GetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', '');
    p := Pos(';', Path);
    if p > 0 then begin
      Delete(Path, p, Length(Path));
    end;
    Core.Blocks.Path := DesignDir + Path;
    //WriteLn('Core.Blocks.Path = "', Core.Blocks.Path, '"');
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
  Path: string;
  ProjectSettings: PProjectSettings;
begin
  ProjectSettings := _ProjectSettings;
  with Project, ProjectSettings^ do begin
    if Filename = '' then
      if SaveDialog1.Execute then
        FileName := SaveDialog1.FileName
      else
        Exit;
    DesignDir := ExtractFilePath(FileName);
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
    path := ExtractRelativepath(DesignDir, Core.Blocks.Path) + ';$(LazarusDir)/lcl/units/$(TargetCPU)-$(TargetOS)';
//    WriteLn('realtive path = ', path);
    SetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', Path);
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
  with Design.SelectedBlock do begin
    Caption := InputBox('Change block name', 'Please type the new block name', Caption);
    Invalidate;
  end;
end;

procedure TdtslIdeMainWindow.SetBlockColor(Sender: TObject);
begin
  if Sender is TColorDialog then with Sender as TColorDialog do begin
    //WriteLn('Change Color from ', hexStr(Design.SelectedBlock.Canvas.Brush.Color, 8), ' to ', hexStr(Color, 8));
    Design.SelectedBlock.Canvas.Brush.Color := Color;
    Invalidate;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
var
  ProjectSettings: PProjectSettings;
begin
  New(ProjectSettings);
  _ProjectSettings := ProjectSettings;
  with CodeToolBoss do begin
    OnSearchUsedUnit := @SearchUsedUnit;
  end;
  with Design do begin
    OnDblClick := @ViewFile;
  end;
  NewProject(Sender);
end;

procedure TdtslIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
var
  Port: TCGraphInputPort;
begin
  Port := TCGraphInputPort.Create(Design.SelectedBlock);
  with Port do begin
    Parent := Design;
  end;
end;

procedure TdtslIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
var
  Port: TCGraphOutputPort;
begin
  Port := TCGraphOutputPort.Create(Design.SelectedBlock);
  with Port do begin
    Parent := Design;
  end;
end;

procedure TdtslIdeMainWindow.CompileProject(Sender: TObject);
begin

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
  ProjectSettings: PProjectSettings;
  DirList: string;
begin
  ProjectSettings := _ProjectSettings;
  //WriteLn('SrcFilename = ', SrcFilename);
  //WriteLn('TheUnitName = ', TheUnitName);
  //WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  DirList := ProjectSettings^.Core.Blocks.Path;
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
    Result := GetCodeBuffer(FileName, cttNone, Self);
end;

destructor TdtslIdeMainWindow.Destroy;
var
  ProjectSettings: PProjectSettings;
begin
  ProjectSettings := _ProjectSettings;
  with ProjectSettings^ do begin
    Core.Blocks.Path := '';
  end;
  Dispose(ProjectSettings);
  _ProjectSettings := nil;
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
        CodeFileName := DesignDir + PathDelim + Name + '.pas';
        CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttBlock, Self);
        UpdateUsedBlocks(Self, CodeBuffer[ctSource]);
        EditorCodeBuffer := CodeBuffer[ctSource];
      except
        ShowMessage('Unable to save file')
      end
    end else if Sender is TCGraphDesign then with Sender as TCGraphDesign do begin
      try
        CodeFileName := DesignDir + '/' + Name + '.pas';
        CodeBuffer[ctSource] := GetCodeBuffer(CodeFileName, cttDesign, Self);
        UpdateUsedBlocks(Self, CodeBuffer[ctSource]);
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
  if Assigned(Design.SelectedBlock) then
    Design.SelectedBlock.Selected := False;
  Design.SelectedBlock := Design.CreateNewBlock;
end;

procedure TdtslIdeMainWindow.SetCoreBlocksPath(Sender: TObject);
var
  ProjectSettings: PProjectSettings;
begin
  ProjectSettings := _ProjectSettings;
  with ProjectSettings^, SelectDirectoryDialog1 do begin
    if Execute then begin
      Core.Blocks.Path := FileName;
    end;
    WriteLn('Core.Blocks.Path = ', Core.Blocks.Path);
  end;
end;

initialization
  {$I mainwindow.lrs}

end.

