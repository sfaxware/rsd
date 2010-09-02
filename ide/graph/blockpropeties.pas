unit BlockPropeties;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, Grids, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs;

type
  TApplyChange = function(Sender: TObject): Boolean of object;
  { TBlockPropertiesDialog }

  TBlockPropertiesDialog = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    StringGrid1: TStringGrid;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
  private
    { private declarations }
  public
    { public declarations }
    ApplyChange: TApplyChange;
  end; 

var
  BlockPropertiesDialog: TBlockPropertiesDialog;

implementation

{ TBlockPropertiesDialog }

procedure TBlockPropertiesDialog.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (aRow > 0) and (aCol > 1);
end;

procedure TBlockPropertiesDialog.ApplyButtonClick(Sender: TObject);
begin
  Visible := not (Assigned(ApplyChange) and ApplyChange(Self));
end;

procedure TBlockPropertiesDialog.CancelButtonClick(Sender: TObject);
begin
  Visible := False;
end;

initialization
  {$I blockpropeties.lrs}

end.

