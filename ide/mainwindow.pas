unit mainWindow; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
  SynEdit, RTTICtrls, XMLCfg, CodeCache, LFMTrees;

type
  { TdtslIdeMainWindow }

  TdtslIdeMainWindow = class(TForm)
    dtslIdeMainMenu: TMainMenu;
    dtslIdeFileMenuItem: TMenuItem;
    dtslIdeEditMenuItem: TMenuItem;
    dtslIdeFileNewMenuItem: TMenuItem;
    dtslIdeFileOpenMenuItem: TMenuItem;
    dtslIdeFileExitMenuItem: TMenuItem;
    MenuItem1: TMenuItem;
    dtslEditGraphDeleteBlockMenuItem: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    dtslEditGraphSubMenu: TMenuItem;
    dtslEditGraphInsertBlockMenuItem: TMenuItem;
    dtslEditCopyMenuItem: TMenuItem;
    dtslEditPastMenuItem: TMenuItem;
    dtslEditCutMenuItem: TMenuItem;
    dtslIdeFileSaveMenuItem: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl: TTabControl;
    Project: TXMLConfig;
    procedure FormCreate(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
    procedure dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure dtslIdeFileExitMenuItemClick(Sender: TObject);
    procedure SelectBlock(Sender: TObject);
    function GetBlockDescription(Block: TCGraphBlock): TLFMTree;
  private
    _Blocks:TFPList;
    _SelectedBlock:TCGraphBlock;
    _ProjectSettings: pointer;
    function SearchUsedUnit(const SrcFilename: string; const TheUnitName, TheUnitInFilename: string): TCodeBuffer;
  public
    procedure InsertBlock(Block:TCGraphBlock);
    procedure RemoveBlock(Block:TCGraphBlock);
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

function TdtslIdeMainWindow.GetBlockDescription(Block: TCGraphBlock): TLFMTree;
var
  SrcFile: string;
begin
  with Block do begin
    Save;
    SrcFile := '/tmp/' + Name + '.pas';
    SynEdit1.Lines.LoadFromFile(srcFile);
    with CodeToolBoss do begin
      OnSearchUsedUnit := @SearchUsedUnit;
      GetCodeToolForSource(CodeBuffer[ctSource], true, false);
      if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], Result, False, False) then
        Result := nil;
    end;
  end;
end;

procedure TdtslIdeMainWindow.InsertBlock(Block:TCGraphBlock);
begin
  _blocks.Add(Block);
end;

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
var
  Path, BlockPath: string;
  BlocksCount: integer;
  BlockDescription: TLFMTree;
  PropertyNode: TLFMPropertyNode;
begin
  with Project, TProjectSettings(_ProjectSettings^) do begin
    if OpenDialog1.Execute then
      FileName := OpenDialog1.FileName
    else
      Exit;
    Name := GetValue('name', 'Unnamed design');
    Path := 'settings/core/blocks/';
    Core.Blocks.Path := GetValue(Path + 'path', '');
    Path :=  'design/blocks/';
    for BlocksCount := 1 to GetValue(Path + 'count', 0) do begin
      BlockPath := 'Block' + IntToStr(BlocksCount);
      if Assigned(_selectedBlock) then
        _selectedBlock.Selected := False;
      _selectedBlock := TCGraphBlock.Create(ScrollBox1);
      with _selectedBlock do begin
        Parent := ScrollBox1;
        try
          Name := 'Block' + IntToStr(BlocksCount);
        except
          ShowMessage('Invalid block name "' + BlockPath + '"');
          _selectedBlock.Destroy;
          _selectedBlock := nil;
          continue;
        end;
        BlockDescription := GetBlockDescription(_SelectedBlock);
        PropertyNode := BlockDescription.FindProperty(BlockPath + '.Left', nil);
        Left := Random(ScrollBox1.Width - Width);
        Top := Random(ScrollBox1.Height - Height);
        Color := clRed;
        Caption := GetValue(Path + BlockPath + '/name', BlockPath);
        OnClick := @SelectBlock;
        OnDblClick := @ViewFile;
        Selected := True;
      end;
      InsertBlock(_selectedBlock);
    end;
  end;
end;

procedure TdtslIdeMainWindow.RemoveBlock(Block:TCGraphBlock);
begin
  _blocks.Remove(Block);
end;

procedure StreamBlock(data, arg: pointer);
var
  Path: string;
begin
  with TXMLConfig(arg), TCGraphBlock(data) do begin
    Path := 'design/blocks/' + Name + '/';
    SetValue(Path + 'name', Caption);
  end;
end;

procedure TdtslIdeMainWindow.SaveProject( Sender: TObject);
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
    SetValue('design/name', Name);
    Path := 'design/sttings/core/blocks/';
    SetValue(Path + 'path', Core.Blocks.Path);
    SetValue('design/blocks/count', _Blocks.Count);
    _Blocks.ForEachCall(@StreamBlock, Project);
    Flush;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  _blocks := TFPList.Create;
  New(PProjectSettings(_ProjectSettings));
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
begin
  WriteLn('SrcFilename = ', SrcFilename);
  WriteLn('TheUnitName = ', TheUnitName);
  WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  FileName := ExtractFileDir(ParamStr(0)) + '/../core/block/' + LowerCase(TheUnitName) + '.pas';
  WriteLn('FileName = ', FileName);
  Result := CodeToolBoss.LoadFile(FileName, True, False);
end;

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
var
  LFMTree: TLFMTree;
  SrcFile: string;
begin
  if Sender is TCGraphBlock then
    LFMTree := GetBlockDescription(Sender as TCGraphBlock);
    if Assigned(LFMTree) then begin
      TabControl.TabIndex := 1;
      SynEdit1.CaretXY := LFMTree.PositionToCaret(25);
      SynEdit1.EnsureCursorPosVisible;
    end else
      ShowMessage('False');
end;

procedure TdtslIdeMainWindow.dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if _selectedBlock = nil then
    WriteLn('No selected block')
  else begin
    WriteLn('Removing block');
    RemoveBlock(_selectedBlock);
    WriteLn('Destroying block');
    _selectedBlock.Destroy;
    _selectedBlock := nil;
  end;
end;

procedure TdtslIdeMainWindow.dtslEditGraphInsertBlockMenuItemClick(Sender:TObject);
var
  BlockQuantity: integer = 0;
begin
  if Assigned(_selectedBlock) then
    _selectedBlock.Selected := False;
  _selectedBlock := TCGraphBlock.Create(ScrollBox1);
  with _selectedBlock do begin
    Parent := ScrollBox1;
    Left := Random(ScrollBox1.Width - Width);
    Top := Random(ScrollBox1.Height - Height);
    Color := clRed;
    BlockQuantity += 1;
    repeat
      try
        Name := 'Block' + IntToStr(BlockQuantity);
        break;
      except
        BlockQuantity += 1;
      end;
    until false;
    Caption := 'Block ' + IntToStr(BlockQuantity);
    OnClick := @SelectBlock;
    OnDblClick := @ViewFile;
    Selected := True;
  end;
  InsertBlock(_selectedBlock);
end;

procedure TdtslIdeMainWindow.SelectBlock(Sender: TObject);
begin
  if Sender is TCGraphBlock then begin
     if Assigned(_selectedBlock) then
       _selectedBlock.Selected := False;
    _selectedBlock := TCGraphBlock(Sender);
    _selectedBlock.Selected := True;
  end;
end;

initialization
  {$I mainwindow.lrs}

end.

