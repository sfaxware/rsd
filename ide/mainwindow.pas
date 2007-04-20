unit mainWindow; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, ComCtrls, SynHighlighterPas, SynCompletion,
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
    TabControl: TTabControl;
    procedure Notebook1ChangeBounds(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure TabControlChange(Sender: TObject);
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

procedure TdtslIdeMainWindow.PaintBox1Paint(Sender: TObject);
begin
  if Sender is TPaintBox then
  with TPaintBox(Sender).Canvas do begin
    Color := clRed;
    FillRect(50, 50, 150, 150);
    FillRect(250, 250, 300, 300);
    Line(150, 50, 200, 50);
    Line(200, 50, 200, 250);
    Line(200, 250, 300, 250);
  end;
end;

procedure TdtslIdeMainWindow.TabControlChange(Sender: TObject);
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

