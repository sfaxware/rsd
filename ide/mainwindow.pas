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
    dtslIdeMainMenu: TMainMenu;
    dtslIdeFileMenuItem: TMenuItem;
    dtslIdeEditMenuItem: TMenuItem;
    dtslIdeFileNewMenuItem: TMenuItem;
    dtslIdeFileOpenMenuItem: TMenuItem;
    dtslIdeFileExitMenuItem: TMenuItem;
    AddOutputPortMenuItem: TMenuItem;
    ConnectPortsMenuItem: TMenuItem;
    PortsSubMenu: TMenuItem;
    dtslEditGraphDeleteBlockMenuItem: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    AddInputPortMenuItem: TMenuItem;
    MenuItem5: TMenuItem;
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
    ScrollBox1: TScrollBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl: TTabControl;
    Project: TXMLConfig;
    procedure AddInputPortMenuItemClick(Sender: TObject);
    procedure AddOutputPortMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure SetCoreBlocksPath(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure dtslIdeFileExitMenuItemClick(Sender: TObject);
  private
    _ProjectSettings: pointer;
    function SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
  public
    procedure ViewFile(Sender: TObject);
  end; 

var
  dtslIdeMainWindow: TdtslIdeMainWindow;

implementation

uses
  StdCodeTools, CodeToolManager, LinkScanner;
  
type
  PProjectSettings = ^ TProjectSettings;
  TProjectSettings = record
    Name: string;
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

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
var
  Path: string;
begin
  CodeToolBoss.OnSearchUsedUnit := @SearchUsedUnit;
  with Project, TProjectSettings(_ProjectSettings^) do begin
    if OpenDialog1.Execute then
      FileName := OpenDialog1.FileName
    else
      Exit;
    Name := GetValue('name', 'Unnamed design');
    Path := 'settings/core/blocks/';
    Core.Blocks.Path := GetValue(Path + 'path', '');
    WriteLn('Core.Blocks.Path = "', Core.Blocks.Path, '"');
    Path :=  'design/blocks/';
    ScrollBox1.Load(Path, Project);
  end;
end;

procedure TdtslIdeMainWindow.SaveProject(Sender: TObject);
var
  Path: string;
begin
  with Project, TProjectSettings(_ProjectSettings^) do begin
    if Filename = '' then
      if SaveDialog1.Execute then
        FileName := SaveDialog1.FileName
      else
        Exit;
    Clear;
    Path := 'settings/core/blocks/';
    SetValue(Path + 'path', Core.Blocks.Path);
    ScrollBox1.Save(Name, Project);
    Flush;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  New(PProjectSettings(_ProjectSettings));
  with ScrollBox1 do begin
    OnDblClick := @ViewFile;
  end;
end;

procedure TdtslIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
var
  Port: TCGraphInputPort;
begin
  Port := TCGraphInputPort.Create(ScrollBox1.SelectedBlock);
  with Port do begin
    Parent := ScrollBox1;
  end;
end;

procedure TdtslIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
var             
  Port: TCGraphOutputPort;
begin
  Port := TCGraphOutputPort.Create(ScrollBox1.SelectedBlock);
  with Port do begin
    Parent := ScrollBox1;
    end;
end;

procedure TdtslIdeMainWindow.TabControlChange(Sender: TObject);
begin
  with Sender as TTabControl do begin
    case TabIndex of
      0:begin
        SynEdit1.Visible := False;
        ScrollBox1.Visible := True;
      end;
      1:begin
        ScrollBox1.Visible := False;
        SynEdit1.Visible := True;
      end;
    end;
  end;
end;

function TdtslIdeMainWindow.SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
var
  FileName: string;
  ProjectSettings: PProjectSettings absolute _ProjectSettings;
begin
  //WriteLn('SrcFilename = ', SrcFilename);
  //WriteLn('TheUnitName = ', TheUnitName);
  //WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  FileName := ProjectSettings^.Core.Blocks.Path + LowerCase(TheUnitName) + '.pas';
  //WriteLn('FileName = ', FileName);
  Result := CodeToolBoss.LoadFile(FileName, True, False);
end;

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
var
  LFMTree: TLFMTree;
  SrcFile: string;
begin
  if Sender is TCGraphBlock then begin
    with Sender as TCGraphBlock do begin
      Save;
      SrcFile := CodeBuffer[ctSource].FileName;
      LFMTree := GetDescription;
    end;
    if Assigned(LFMTree) then with SynEdit1 do begin
      Lines.LoadFromFile(srcFile);
      TabControl.TabIndex := 1;
      CaretXY := LFMTree.PositionToCaret(25);
      EnsureCursorPosVisible;
    end else
      ShowMessage('False');
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if ScrollBox1.SelectedBlock = nil then
    //WriteLn('No selected block')
  else begin
    WriteLn('Removing block');
    ScrollBox1.RemoveBlock(ScrollBox1.SelectedBlock);
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertBlockMenuItemClick(Sender:TObject);
begin
  if Assigned(ScrollBox1.SelectedBlock) then
    ScrollBox1.SelectedBlock.Selected := False;
  ScrollBox1.SelectedBlock := ScrollBox1.CreateNewBlock;
  ScrollBox1.InsertBlock(ScrollBox1.SelectedBlock);
end;

procedure TdtslIdeMainWindow.SetCoreBlocksPath(Sender: TObject);
var
  ProjectSettings: PProjectSettings absolute _ProjectSettings;
begin
  with SelectDirectoryDialog1 do
    if Execute then
      ProjectSettings^.Core.Blocks.Path := FileName;
end;

initialization
  {$I mainwindow.lrs}

end.

