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
    procedure ConnectPortsMenuItemClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure LoadProject(Sender: TObject);
    procedure SaveProject(Sender: TObject);
    procedure SetCoreBlocksPath(Sender: TObject);
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
begin
  with Block do
    if Load then with CodeToolBoss do begin
      OnSearchUsedUnit := @SearchUsedUnit;
      GetCodeToolForSource(CodeBuffer[ctSource], true, false);
      if not CheckLFM(CodeBuffer[ctSource], CodeBuffer[ctDescription], Result, False, False) then
        Result := nil;
    end;
end;

procedure TdtslIdeMainWindow.InsertBlock(Block:TCGraphBlock);
begin
  _blocks.Add(Block);
end;

procedure TdtslIdeMainWindow.LoadProject(Sender: TObject);
  function GetPropertyValue(BlockDescription: TLFMTree; PropertyName: string): string;
  var
    PropertyNode: TLFMPropertyNode;
    ValueNode: TLFMTreeNode;
    c: char;
    p: integer;
  begin
    Result := '';
    PropertyNode := BlockDescription.FindProperty(PropertyName, nil);
    ValueNode := PropertyNode.Next;
    if ValueNode.TheType = lfmnValue then with ValueNode as TLFMValueNode do
      case ValueType of
        lfmvString: Result := ReadString;
        lfmvInteger: begin
          p := StartPos;
          c := Tree.LFMBuffer.Source[p];
          while c in ['0'..'9'] do begin
            Result += c;
            p += 1;
            c := Tree.LFMBuffer.Source[p]
          end;
        end;
      end;
    WriteLn('GetPropertyValue(', PropertyName, ') = "', Result, '"');
  end;
var
  Path, BlockPath: string;
  BlocksCount: integer;
  BlockDescription: TLFMTree;
begin
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
    for BlocksCount := 1 to GetValue(Path + 'count', 0) do begin
      BlockPath := 'Block' + IntToStr(BlocksCount);
      WriteLn('Loading "', BlockPath, '"');
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
        if not Assigned(BlockDescription) then begin
          WriteLn('BlockDescription = nil');
          _SelectedBlock.Free;
          continue;
        end;
        Left := StrToInt(GetPropertyValue(BlockDescription, BlockPath + '.Left'));
        Top := StrToInt(GetPropertyValue(BlockDescription, BlockPath + '.Top'));
        Color := clRed;
        Caption := GetPropertyValue(BlockDescription, BlockPath + '.Name');
        OnClick := @SelectBlock;
        OnDblClick := @ViewFile;
        Selected := True;
        PopupMenu := PopupMenu1;
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
    Save;
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
    Path := 'settings/core/blocks/';
    SetValue(Path + 'path', Core.Blocks.Path);
    SetValue('design/name', Name);
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

procedure TdtslIdeMainWindow.AddInputPortMenuItemClick(Sender: TObject);
var
  Port: TCGraphInputPort;
begin
  Port := TCGraphInputPort.Create(_SelectedBlock);
  with Port do begin
    Parent := ScrollBox1;
  end;
end;

procedure TdtslIdeMainWindow.AddOutputPortMenuItemClick(Sender: TObject);
var             
  Port: TCGraphOutputPort;
begin
  Port := TCGraphOutputPort.Create(_SelectedBlock);
  with Port do begin
    Parent := ScrollBox1;
    end;
end;

procedure TdtslIdeMainWindow.ConnectPortsMenuItemClick(Sender: TObject);
var
  Connector: TCGraphConnector;
  AOutputPort: TCGraphOutputPort;
  AInputPort:TCGraphInputPort;
begin
  AOutputPort := TCGraphOutputPort(TCGraphBlock(_Blocks.Items[0]).Components[0]);
  AInputPort := TCGraphInputPort(TCGraphBlock(_Blocks.Items[1]).Components[0]);
  Connector := TCGraphConnector.Create(ScrollBox1);
  with Connector do begin
    Parent := ScrollBox1;
    Connect(AOutputPort, AInputPort);
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
  WriteLn('SrcFilename = ', SrcFilename);
  WriteLn('TheUnitName = ', TheUnitName);
  WriteLn('TheUnitInFilename = ', TheUnitInFilename);
  FileName := ProjectSettings^.Core.Blocks.Path + LowerCase(TheUnitName) + '.pas';
  WriteLn('FileName = ', FileName);
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
    end;
    LFMTree := GetBlockDescription(Sender as TCGraphBlock);
    if Assigned(LFMTree) then begin
      SynEdit1.Lines.LoadFromFile(srcFile);
      SynEdit1.CaretXY := LFMTree.PositionToCaret(25);
      SynEdit1.EnsureCursorPosVisible;
      TabControl.TabIndex := 1;
    end else
      ShowMessage('False');
  end;
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
    PopupMenu := PopupMenu1;
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

