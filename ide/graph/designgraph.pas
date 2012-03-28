unit DesignGraph;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, XMLCfg,GraphComponents;

type
  TCGraphDesign = class(TScrollBox)
  private
    _Blocks:TFPList;
    procedure StreamBlock(Block: TCGraphBlock;var Project: TXMLConfig);
  public
    SelectedBlock:TCGraphBlock;
    SelectedInputPort: TCGraphInputPort;
    SelectedOutputPort: TCGraphOutputPort;
    constructor Create(AOwner: TComponent); override;
    function BlockCount: Integer; inline;
    function CreateNewBlock: TCGraphBlock; virtual;
    function Load(Path: string; const Project: TXMLConfig): Boolean;
    function Save(DesignName: string; var Project: TXMLConfig): Boolean;
    procedure ConnectPorts(Sender: TObject);
    procedure InsertBlock(Block:TCGraphBlock);
    procedure RemoveBlock(var Block:TCGraphBlock);
    procedure SelectBlock(Sender: TObject);
  end;
  TScrollBox = class(TCGraphDesign);

implementation
uses
  Graphics, LFMTrees;

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

procedure TCGraphDesign.RemoveBlock(var Block:TCGraphBlock);
begin
  _blocks.Remove(Block);
  Block.Destroy;
  Block := nil;
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

procedure TCGraphDesign.StreamBlock(Block: TCGraphBlock;var Project: TXMLConfig);
var
  Path: string;
begin
  with Project, Block do begin
    Save;
    Path := 'design/blocks/' + Name + '/';
    SetValue(Path + 'name', Caption);
  end;
end;

function TCGraphDesign.Load(Path: string; const Project: TXMLConfig): Boolean;
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
  BlockPath: string;
  BlocksCount: integer;
  BlockDescription: TLFMTree;
begin
  with Project do begin
    for BlocksCount := 1 to GetValue(Path + 'count', 0) do begin
      BlockPath := 'Block' + IntToStr(BlocksCount);
      WriteLn('Loading "', BlockPath, '"');
      if Assigned(SelectedBlock) then
        SelectedBlock.Selected := False;
      SelectedBlock := TCGraphBlock.Create(Self);
      with SelectedBlock do begin
        Parent := Self;
        try
          Name := 'Block' + IntToStr(BlocksCount);
        except
          WriteLn('Invalid block name "', BlockPath, '"');
          Destroy;
          SelectedBlock := nil;
          continue;
        end;
        BlockDescription := GetDescription;
        if not Assigned(BlockDescription) then begin
          WriteLn('BlockDescription = nil');
          SelectedBlock.Free;
          continue;
        end;
        Left := StrToInt(GetPropertyValue(BlockDescription, BlockPath + '.Left'));
        Top := StrToInt(GetPropertyValue(BlockDescription, BlockPath + '.Top'));
        Color := clRed;
        Caption := GetPropertyValue(BlockDescription, BlockPath + '.Name');
        OnClick := @SelectBlock;
        OnDblClick := Self.OnDblClick;
        Selected := True;
        PopupMenu := Self.PopupMenu;
      end;
      InsertBlock(SelectedBlock);
    end;
  end;
end;

function TCGraphDesign.Save(DesignName: string; var Project: TXMLConfig): Boolean;
var
  Component: TComponent;
  i: Integer;
  f: System.Text;
begin
  with Project do begin
    SetValue('design/name', DesignName);
    SetValue('design/blocks/count', _Blocks.Count);
  end;
  System.Assign(f, '/tmp/Design.lfm');
  ReWrite(f);
  for i := 0 to ComponentCount - 1 do begin
    Component := Components[i];
    if Component is TCGraphConnector then with Component as TCGraphConnector do begin
      Save(f);
    end else if Component is TCGraphBlock then with Component as TCGraphBlock do begin
      //Save();
      StreamBlock(Component as TCGraphBlock, Project);
    end; 
  end;
  Close(f);
  Result := True;
end;

{procedure Register;
begin
  RegisterComponents('GraphDesign', [TCGraphDesign]);
end;

initialization
  RegisterClass(TCGraphDesign);}
end.

