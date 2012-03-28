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
    procedure FormCreate(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
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
    DesignDir := ExtractFileDir(FileName);
    Name := GetValue('ProjectOptions/Units/Unit1/UnitName/Value', 'Design');
    Self.Caption := 'D.T.S.L. IDE (' + Name + ')';
    Path := GetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', '');
    Core.Blocks.Path := DesignDir + PathDelim + Path;
    //WriteLn('Core.Blocks.Path = "', Core.Blocks.Path, '"');
    Path :=  'design/blocks/';
    Design.Load(Path);
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
    DesignDir := ExtractFileDir(FileName) + PathDelim;
    if Name = '' then
      Name := ChangeFileExt(ExtractFileName(FileName), '');
    SetValue('ProjectOptions/PathDelim/Value', PathDelim);
    SetValue('ProjectOptions/General/MainUnit/Value', 0);
    SetValue('ProjectOptions/Units/Count', 2);
    SetValue('ProjectOptions/Units/Unit0/Filename/Value', 'Simulate' + Name + '.pas');
    SetValue('ProjectOptions/Units/Unit0/IsPartOfProject/Value', True);
    SetValue('ProjectOptions/Units/Unit0/UnitName/Value', 'Simulate' + Name);
    SetValue('ProjectOptions/Units/Unit0/EditorIndex/Value', 0);
    SetValue('ProjectOptions/Units/Unit0/Loaded/Value', True);
    SetValue('ProjectOptions/Units/Unit1/Filename/Value', Name + '.pas');
    SetValue('ProjectOptions/Units/Unit1/IsPartOfProject/Value', True);
    SetValue('ProjectOptions/Units/Unit1/UnitName/Value', Name);
    SetValue('ProjectOptions/Units/Unit1/EditorIndex/Value', 0);
    SetValue('ProjectOptions/Units/Unit1/Loaded/Value', True);
//    WriteLn('Core.Blocks.Path = ', Core.Blocks.Path);
//    WriteLn('DesignDir = ', DesignDir);
    path := ExtractRelativepath(DesignDir, Core.Blocks.Path);
//    WriteLn('realtive path = ', path);
    SetValue('CompilerOptions/SearchPaths/OtherUnitFiles/Value', Path);
    if Assigned(EditorCodeBuffer) then begin
       EditorCodeBuffer.Source := Text;
    end;                        
    Design.Save(Name);
    Flush;
  end;
end;

procedure TdtslIdeMainWindow.SelectBlockColor(Sender: TObject);
begin
  ColorDialog1.Execute;
end;

procedure TdtslIdeMainWindow.SelectBlockName(Sender: TObject);
begin
  with Design.SelectedBlock do begin
    Caption := InputBox('Change block name', 'Please type the new block name', Caption);
  end;
end;

procedure TdtslIdeMainWindow.SetBlockColor(Sender: TObject);
begin
  if Sender is TColorDialog then with Sender as TColorDialog do begin
    //WriteLn('Change Color from ', hexStr(Design.SelectedBlock.Canvas.Brush.Color, 8), ' to ', hexStr(Color, 8));
    Design.SelectedBlock.Canvas.Brush.Color := Color;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  New(PProjectSettings(_ProjectSettings));
  with Design do begin
    OnDblClick := @ViewFile;
  end;
  with TProjectSettings(_ProjectSettings^) do begin
    Name := Design.Name;
  end;
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
  ProjectSettings: PProjectSettings absolute _ProjectSettings;
begin
  //WriteLn('SrcFilename = ', SrcFilename);
  //WriteLn('TheUnitName = ', TheUnitName);
  //WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  if TheUnitName = 'Blocks' then
    FileName := ProjectSettings^.Core.Blocks.Path + '../../ide/graph/' + LowerCase(TheUnitName) + '.pas'
  else
    FileName := ProjectSettings^.Core.Blocks.Path + LowerCase(TheUnitName) + '.pas';
  //WriteLn('FileName = ', FileName);
  Result := CodeToolBoss.LoadFile(FileName, True, False);
end;

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
var
  LFMTree: TLFMTree;
begin
  if Sender is TCGraphBlock then begin
    with Sender as TCGraphBlock do begin
      Save;
      LFMTree := GetDescription;
      if Assigned(LFMTree) then with SynEdit1 do begin
        if Assigned(EditorCodeBuffer) then begin
           EditorCodeBuffer.Source := Text;
        end;
        EditorCodeBuffer := CodeBuffer[ctSource];
        Text := EditorCodeBuffer.Source;
        TabControl.TabIndex := 1;
        CaretXY := LFMTree.PositionToCaret(25);
        EnsureCursorPosVisible;
      end else
        ShowMessage('False');
    end;
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
  ProjectSettings: PProjectSettings absolute _ProjectSettings;
begin
  with SelectDirectoryDialog1 do
    if Execute then
      ProjectSettings^.Core.Blocks.Path := FileName;
end;

initialization
  {$I mainwindow.lrs}

end.

