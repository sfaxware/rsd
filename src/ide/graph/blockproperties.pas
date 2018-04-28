unit BlockProperties;

{$mode objfpc}{$H+}

interface

uses
  Buttons, Classes, Grids, SysUtils, FileUtil, LResources, Forms,
  Controls, Graphics, Dialogs, GraphComponents, StdCtrls;

type
  { TBlockPropertiesDialog }

  TBlockPropertiesDialog = class(TForm)
    ApplyButton: TButton;
    CancelButton: TButton;
    StringGrid1: TStringGrid;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
    procedure ValidateInput(Sender: TObject);
  private
    FDevice: TBlock;
  protected
    procedure SetDevice(ADevice: TBlock);
  public
    { public declarations }
    property Device: TBlock write SetDevice;
  end; 

var
  BlockPropertiesDialog: TBlockPropertiesDialog;

implementation

{ TBlockPropertiesDialog }

procedure TBlockPropertiesDialog.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);
begin
  CanSelect := (aRow > 0) and (aCol > 1);
end;

procedure TBlockPropertiesDialog.ValidateInput(Sender: TObject);
var
  PropIdx: Integer;
  PropTyp, PropVal: string;
  v, e: Integer;
  f: Real;
begin
  with StringGrid1 do begin
    PropIdx := Row;
    PropTyp := Rows[PropIdx].Strings[1];
    PropVal := Rows[PropIdx].Strings[2];
    if PropTyp = 'Integer' then begin
      Val(PropVal, v, e);
    end else if PropTyp = 'Real' then begin

    {end else if PropTyp = 'Symbol' then begin

    end else if PropTyp = 'Set' then begin

    end else if PropTyp = 'List' then begin

    end else if PropTyp = 'Collection' then begin

    end else if PropTyp = 'Binary' then begin}
      Val(PropVal, f, e);
    end else begin
      e := 0;
    end;
    if e <> 0 then begin
      ShowMessage('Invalid input "' + PropVal + '" for property type ' + PropTyp);
    end;
  end;
end;

procedure TBlockPropertiesDialog.ApplyButtonClick(Sender: TObject);
var
  DevicePropQty: Integer;
  i: Integer;
begin
  Visible := not Assigned(FDevice);
  with FDevice, StringGrid1 do begin
    DevicePropQty := PropQty;
    for i := 0 to DevicePropQty - 1 do with Rows[i + 1] do begin
      PropVal[i] := Strings[2];
    end;
  end;
end;

procedure TBlockPropertiesDialog.CancelButtonClick(Sender: TObject);
begin
  Visible := False;
end;

procedure TBlockPropertiesDialog.SetDevice(ADevice: TBlock);
var
  DevicePropQty: Integer;
  i: Integer;
begin
  FDevice := ADevice;
  with FDevice, StringGrid1 do begin
    DevicePropQty := PropQty;
    RowCount := DevicePropQty + 1;
    for i := 0 to DevicePropQty - 1 do with Rows[i + 1] do begin
      Strings[0] := PropName[i];
      Strings[1] := PropType[i];
      Strings[2] := PropVal[i];
    end;
  end;
end;

initialization
  {$R blockproperties.lfm}

end.

