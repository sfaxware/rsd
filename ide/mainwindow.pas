unit mainWindow; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
  SynEdit, RTTICtrls, XMLCfg;

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
    procedure SaveProject ( Sender: TObject ) ;
    procedure TabControlChange(Sender: TObject);
    procedure dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
    procedure dtslEditGraphInsertBlockMenuItemClick(Sender: TObject);
    procedure dtslIdeFileExitMenuItemClick(Sender: TObject);
    procedure SelectBlock(Sender: TObject);
  private
    _Blocks:TFPList;
    _SelectedBlock:TCGraphBlock;
  public
    procedure InsertBlock(Block:TCGraphBlock);
    procedure RemoveBlock(Block:TCGraphBlock);
    procedure ViewFile(Sender: TObject);
  end; 

var
  dtslIdeMainWindow: TdtslIdeMainWindow;

implementation

uses
  LFMTrees, StdCodeTools, CodeCache, CodeToolManager, LinkScanner;

var
  CodeTool: TStandardCodeTool;
  
{ TdtslIdeMainWindow }

procedure TdtslIdeMainWindow.dtslIdeFileExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TdtslIdeMainWindow.InsertBlock(Block:TCGraphBlock);
begin
  _blocks.Add(Block);
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
    Path := 'design/blocks/' + name + '/';
    SetValue(Path + 'name', Caption);
    SetValue(Path + 'type', Typ);
    SetValue(Path + 'left', Left);
    SetValue(Path + 'top', Top);
    SetValue(Path + 'width', Width);
    SetValue(Path + 'height', Height);
  end;
end;

procedure TdtslIdeMainWindow.SaveProject ( Sender: TObject ) ;
begin
  with Project do begin
    if Filename = '' then
      if SaveDialog1.Execute then
        FileName := SaveDialog1.FileName
      else
        Exit;
    Clear;
    _Blocks.ForEachCall(@StreamBlock, Project);
    Flush;
  end;
end;

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  _blocks := TFPList.Create;
  CodeTool := TStandardCodeTool.Create;
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

procedure TdtslIdeMainWindow.ViewFile(Sender: TObject);
var
  f: System.Text;
  LFMCodeCache, PASCodeCache: TCodeCache;
  LFMBuf, PASBuf: TCodeBuffer;
  LFMTree: TLFMTree;
  SrcFile, DescrFile: string;
begin
  if Sender is TCGraphBlock then
    with Sender as TCGraphBlock do begin
      SrcFile := '/tmp/' + Name + '.pas';
      if not FileExists(SrcFile) then begin
        System.Assign(f, SrcFile);
        ReWrite(f);
        WriteLn(f, 'unit ', Name, ';');
        WriteLn(f, 'interface');
        WriteLn(f, 'uses');
        WriteLn(f, '  Blocks;');
        WriteLn(f);
        WriteLn(f, 'type');
        WriteLn(f, '  T', Name, ' = class(TBlock)');
        WriteLn(f, '    procedure Execute;');
        WriteLn(f, '  end;');
        WriteLn(f);
        WriteLn(f, 'implementation');
        WriteLn(f, 'procedure T', Name, '.Execute;');
        WriteLn(f, 'begin;');
        WriteLn(f, '  {Write here your code}');
        WriteLn(f, 'end;');
        WriteLn(f);
        WriteLn(f, 'initialization');
        WriteLn(f);
        WriteLn(f, 'finalization');
        WriteLn(f);
        WriteLn(f, 'end.');
        System.Close(f);
      end;
      SynEdit1.Lines.LoadFromFile(srcFile);
      DescrFile := '/tmp/' + Name + '.lfm';
      if not FileExists(DescrFile) then begin
        System.Assign(f, DescrFile);
        ReWrite(f);
        WriteLn(f, 'object ', Name, ': T' + Name);
        WriteLn(f, '  Name = ', Name);
        WriteLn(f, '  Typ = ', Typ);
        WriteLn(f, '  Left = ', Left);
        WriteLn(f, '  Top = ', Top);
        WriteLn(f, '  Width = ', Width);
        WriteLn(f, '  Height = ', Height);
        WriteLn(f, 'end');
        System.Close(f);
      end;
    end;
  LFMCodeCache := TCodeCache.Create;
  LFMBuf := LFMCodeCache.LoadFile(DescrFile);
  PASCodeCache := TCodeCache.Create;
  PASBuf := PASCodeCache.LoadFile(srcFile);
  CodeToolBoss.GetCodeToolForSource(PASBuf, true, false);
  if CodeToolBoss.CheckLFM(PASBuf, LFMBuf, LFMTree, False, False) then begin
    TabControl.TabIndex := 1;
    SynEdit1.CaretXY := LFMTree.PositionToCaret(25);
    SynEdit1.EnsureCursorPosVisible;
  end else
    ShowMessage('False');
end;

procedure TdtslIdeMainWindow.dtslEditGraphDeleteBlockMenuItemClick(Sender: TObject);
begin
  if _selectedBlock = Nil then
    WriteLn('No selected block')
  else begin
    WriteLn('Removing block');
    RemoveBlock(_selectedBlock);
    WriteLn('Destroying block');
    _selectedBlock.Destroy;
    _selectedBlock := Nil;
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

