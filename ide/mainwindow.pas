unit mainWindow; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynHighlighterMulti, SynCompletion,
  SynEdit, Grids, RTTICtrls;

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
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    StatusBar1: TStatusBar;
    SynAutoComplete1: TSynAutoComplete;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    TabControl2: TTabControl;
    procedure Notebook1ChangeBounds(Sender: TObject);
    procedure TabControl2Change(Sender: TObject);
    procedure dtslIdeFileExitMenuItemClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  dtslIdeMainWindow: TdtslIdeMainWindow;

implementation

{ TdtslIdeMainWindow }

procedure TdtslIdeMainWindow.dtslIdeFileExitMenuItemClick(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TdtslIdeMainWindow.Notebook1ChangeBounds(Sender: TObject);
begin

end;

procedure TdtslIdeMainWindow.TabControl2Change(Sender: TObject);
begin
  with TTabControl(Sender) do begin
    case TabIndex of
         0:begin
             PaintBox1.Visible := False;
             SynEdit1.Visible := True;
           end;
         1:begin
             SynEdit1.Visible := False;
             PaintBox1.Visible := True;
           end;
    end;
  end;
end;

initialization
  {$I mainwindow.lrs}

end.

