unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, XMLCfg,GraphComponents;

type
  TCGraphDesign = class(TScrollBox)
  private
    _Blocks:TFPList;
    procedure StreamBlock(data, arg: pointer);
  public
    SelectedBlock:TCGraphBlock;
    SelectedInputPort: TCGraphInputPort;
    SelectedOutputPort: TCGraphOutputPort;
    constructor Create(AOwner: TComponent); override;
    function BlockCount: Integer; inline;
    function CreateNewBlock: TCGraphBlock; virtual;
    procedure ConnectPorts(Sender: TObject);
    procedure InsertBlock(Block:TCGraphBlock);
    procedure RemoveBlock(Block:TCGraphBlock);
    procedure SelectBlock(Sender: TObject);
    procedure Stream(DesignName: string; var Project: TXMLConfig);
  end;
  TScrollBox = class(TCGraphDesign);

implementation

constructor TCGraphDesign.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  _blocks := TFPList.Create;
  //WriteLn('Created new TCGraphDesign class instance');
end;

function TCGraphDesign.BlockCount: Integer; inline;
begin
  Result := _Blocks.Count;
end;

procedure TCGraphDesign.ConnectPorts(Sender: TObject);
var
  Connector: TCGraphConnector;
begin
  Connector := TCGraphConnector.Create(Self);
  with Connector do begin
    Parent := Self;
    Connect(SelectedOutputPort, SelectedInputPort);
  end;
end;

function TCGraphDesign.CreateNewBlock:TCGraphBlock;
var
  BlockQuantity: integer = 0;
begin
  Result := TCGraphBlock.Create(Self);
  with Result do begin
    Parent := Self;
    Left := Random(Self.Width - Width);
    Top := Random(Self.Height - Height);
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
    OnDblClick := Self.OnDblClick;
    PopupMenu := Self.PopupMenu;
    Selected := True;
  end;
end;

procedure TCGraphDesign.InsertBlock(Block:TCGraphBlock);
begin
  _blocks.Add(Block);
end;

procedure TCGraphDesign.RemoveBlock(Block:TCGraphBlock);
begin
  _blocks.Remove(Block);
end;

procedure TCGraphDesign.SelectBlock(Sender: TObject);
begin
  if Sender is TCGraphBlock then begin
     if Assigned(SelectedBlock) then
       SelectedBlock.Selected := False;
    SelectedBlock := Sender as TCGraphBlock;
    SelectedBlock.Selected := True;
  end;
end;

procedure TCGraphDesign.StreamBlock(data, arg: pointer);
var
  Path: string;
begin
  with TXMLConfig(arg), TCGraphBlock(data) do begin
    Save;
    Path := 'design/blocks/' + Name + '/';
    SetValue(Path + 'name', Caption);
  end;
end;

procedure TCGraphDesign.Stream(DesignName: string; var Project: TXMLConfig);
begin
  with Project do begin
    SetValue('design/name', DesignName);
    SetValue('design/blocks/count', _Blocks.Count);
  end;
  _Blocks.ForEachCall(@StreamBlock, Project);
end;

{procedure Register;
begin
  RegisterComponents('GraphDesign', [TCGraphDesign]);
end;

initialization
  RegisterClass(TCGraphDesign);}
end.

