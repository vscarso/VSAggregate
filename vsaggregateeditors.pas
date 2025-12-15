unit VSAggregateEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, CheckLst, Dialogs, PropEdits, DB,
  VSAggregate;

type
  { Editor de propriedade para Sources }
  TVSAggregateSourcesEditor = class(TPropertyEditor)
  private
    FAgg: TVSAggregate;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: ansistring; override;
    procedure Edit; override;
  end;

  { Formulário de edição }
  TFrmSourcesEditor = class(TForm)
  private
    btnLoadFields: TButton;
    chkFields: TCheckListBox;
    btnApply: TButton;
    btnExport: TButton;
    btnImport: TButton;
    btnClose: TButton;
    FAgg: TVSAggregate;
    procedure LoadNumericFields(ADataSet: TDataSet);
    procedure BtnLoadFieldsClick(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnExportClick(Sender: TObject);
    procedure BtnImportClick(Sender: TObject);
  public
    constructor CreateEditor(AOwner: TComponent; AAgg: TVSAggregate); reintroduce;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VSComponents', [TVSAggregate]);
  RegisterPropertyEditor(TypeInfo(TSumSources), TVSAggregate, 'Sources', TVSAggregateSourcesEditor);
end;

{ TVSAggregateSourcesEditor }

function TVSAggregateSourcesEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

function TVSAggregateSourcesEditor.GetValue: ansistring;
begin
  Result := Format('(%d itens)', [TVSAggregate(GetComponent(0)).Sources.Count]);
end;

procedure TVSAggregateSourcesEditor.Edit;
var
  F: TFrmSourcesEditor;
begin
  FAgg := TVSAggregate(GetComponent(0));
  F := TFrmSourcesEditor.CreateEditor(Application, FAgg);
  try
    F.ShowModal;
    Modified;
  finally
    F.Free;
  end;
end;

{ TFrmSourcesEditor }

constructor TFrmSourcesEditor.CreateEditor(AOwner: TComponent; AAgg: TVSAggregate);
begin
  inherited CreateNew(AOwner, 0);
  Caption := 'Editor de fontes de agregação';
  Width := 560;
  Height := 440;
  Position := poScreenCenter;
  BorderStyle := bsDialog;

  FAgg := AAgg;

  btnLoadFields := TButton.Create(Self);
  btnLoadFields.Parent := Self;
  btnLoadFields.Caption := 'Carregar campos';
  btnLoadFields.Left := 16; btnLoadFields.Top := 12; btnLoadFields.Width := 120;
  btnLoadFields.OnClick := @BtnLoadFieldsClick;

  chkFields := TCheckListBox.Create(Self);
  chkFields.Parent := Self;
  chkFields.Left := 16; chkFields.Top := 48; chkFields.Width := 516; chkFields.Height := 300;

  btnApply := TButton.Create(Self);
  btnApply.Parent := Self;
  btnApply.Caption := 'Aplicar seleção';
  btnApply.Left := 16; btnApply.Top := 360; btnApply.Width := 140;
  btnApply.OnClick := @BtnApplyClick;

  btnExport := TButton.Create(Self);
  btnExport.Parent := Self;
  btnExport.Caption := 'Exportar CSV';
  btnExport.Left := 168; btnExport.Top := 360; btnExport.Width := 120;
  btnExport.OnClick := @BtnExportClick;

  btnImport := TButton.Create(Self);
  btnImport.Parent := Self;
  btnImport.Caption := 'Importar CSV';
  btnImport.Left := 296; btnImport.Top := 360; btnImport.Width := 120;
  btnImport.OnClick := @BtnImportClick;

  btnClose := TButton.Create(Self);
  btnClose.Parent := Self;
  btnClose.Caption := 'Fechar';
  btnClose.Left := 424; btnClose.Top := 360; btnClose.Width := 108;
  btnClose.ModalResult := mrClose;
end;

procedure TFrmSourcesEditor.LoadNumericFields(ADataSet: TDataSet);
var
  i: Integer;
  F: TField;
  SL: TStringList;
begin
  chkFields.Items.Clear;
  if not Assigned(ADataSet) then Exit;
  if not ADataSet.Active then
    try ADataSet.Open; except end;

  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupIgnore;

    for i := 0 to ADataSet.Fields.Count - 1 do
    begin
      F := ADataSet.Fields[i];
      case F.DataType of
        ftSmallint, ftInteger, ftWord, ftLargeint,
        ftFloat, ftCurrency, ftBCD, ftFMTBcd:
          SL.Add(F.FieldName);
      end;
    end;

    chkFields.Items.Assign(SL);

    // marca os já existentes em Sources
    for i := 0 to chkFields.Items.Count - 1 do
      if Assigned(FAgg) and (FAgg.Sources.Count > 0) then
        if FAgg.Total(chkFields.Items[i]) <> Null then
          chkFields.Checked[i] := True;
  finally
    SL.Free;
  end;
end;

procedure TFrmSourcesEditor.BtnLoadFieldsClick(Sender: TObject);
begin
  if Assigned(FAgg.DataSet) then
    LoadNumericFields(FAgg.DataSet)
  else
    ShowMessage('Defina o DataSet na propriedade do componente antes de carregar os campos.');
end;

procedure TFrmSourcesEditor.BtnApplyClick(Sender: TObject);
var
  i: Integer;
  Item: TSumSource;
begin
  if not Assigned(FAgg.DataSet) then Exit;

  // limpa fontes antigas
  FAgg.Sources.Clear;

  for i := 0 to chkFields.Items.Count - 1 do
    if chkFields.Checked[i] then
    begin
      Item := FAgg.Sources.Add;
      Item.DataSet := FAgg.DataSet;
      Item.FieldName := chkFields.Items[i];
    end;
end;

procedure TFrmSourcesEditor.BtnExportClick(Sender: TObject);
var
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(Self);
  try
    dlg.Filter := 'CSV (*.csv)|*.csv';
    dlg.DefaultExt := 'csv';
    if dlg.Execute then
      FAgg.Sources.ExportToCSV(dlg.FileName);
  finally
    dlg.Free;
  end;
end;

procedure TFrmSourcesEditor.BtnImportClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'CSV (*.csv)|*.csv';
    if dlg.Execute then
      FAgg.Sources.ImportFromCSV(dlg.FileName);
  finally
    dlg.Free;
  end;
end;

end.

