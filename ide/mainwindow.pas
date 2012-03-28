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
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
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
    _blocks:TFPList;
    _selectedBlock:TCGraphBlock;
  public
    procedure InsertBlock(Block:TCGraphBlock);
    procedure RemoveBlock(Block:TCGraphBlock);
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
  with TTabControl(Sender) do begin
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

