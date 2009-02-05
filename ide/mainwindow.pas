unit mainWindow; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion, GraphComponents,
  SynEdit, RTTICtrls;

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
    PopupMenu1: TPopupMenu;
    ScrollBox1: TScrollBox;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl: TTabControl;
    procedure FormCreate(Sender: TObject);
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

procedure TdtslIdeMainWindow.FormCreate(Sender: TObject);
begin
  _blocks := TFPList.Create;
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
begin
  if Sender is TCGraphBlock then
    with Sender as TCGraphBlock do begin
      if not FileExists(Caption) then begin
        System.Assign(f, Caption + '.pas');
        ReWrite(f);
        WriteLn(f, 'unit ', Caption, ';');
        WriteLn(f, 'interface');
        WriteLn(f, 'type');
        WriteLn(f, '  T', Caption, ' = Calss(TBlock)');
        WriteLn(f, '    procedure Execute;');
        WriteLn(f, '  end;');
        WriteLn(f);
        WriteLn(f, 'implementation');
        WriteLn(f, 'procedure T', Caption, '.Execute;');
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
      SynEdit1.Lines.LoadFromFile(Caption + '.pas');
    end;
  TabControl.TabIndex := 1;
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
begin
  if Assigned(_selectedBlock) then
    _selectedBlock.Selected := False;
  _selectedBlock := TCGraphBlock.Create(ScrollBox1);
  with _selectedBlock do begin
    Parent := ScrollBox1;
    Left := Random(ScrollBox1.Width - Width);
    Top := Random(ScrollBox1.Height - Height);
    Color := clRed;
    Caption := 'Block';
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

