📘 Documentação – TVSAggregate
O TVSAggregate é um componente para Lazarus/Free Pascal que calcula totais de campos numéricos em um TDataSet. Ele facilita operações de soma, média, contagem, máximo e mínimo sobre os dados.

🚀 Instalação
Adicione as units VSAggregate.pas e VSAggregateEditors.pas ao seu pacote no Lazarus.

Compile e instale o pacote.

O componente aparecerá na aba VSComponents.

⚙️ Propriedades
DataSet → Define o TDataSet principal usado pelo componente.

Sources → Coleção de campos numéricos selecionados para agregação.

AggregateKind → Tipo de agregação (akSum, akAvg, akCount, akMax, akMin).

AutoRecalc → Se verdadeiro, recalcula automaticamente ao modificar o DataSet.

DecimalPlaces → Número de casas decimais para exibição.

🛠️ Métodos
CalcularTotal → Executa o cálculo manualmente. Útil quando AutoRecalc = False.

Total(FieldName: string): Variant → Retorna o total calculado para um campo específico.

🎯 Eventos
OnAfterCalculate → Disparado após o cálculo dos totais.

📊 Como acessar os valores
TotalValue → Soma geral de todos os campos configurados.

Total('Campo') → Soma específica de um campo.

Exemplo:
pascal
procedure TForm1.FormShow(Sender: TObject);
begin
  ZQueryProdutos.Open;
  VSAggregate1.DataSet := ZQueryProdutos;
  VSAggregate1.AutoRecalc := True;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  ShowMessage('Total Valor: ' + VarToStr(VSAggregate1.Total('VALOR')));
  ShowMessage('Total Quantidade: ' + VarToStr(VSAggregate1.Total('QUANTIDADE')));
end;

procedure TForm1.VSAggregate1AfterCalculate(Sender: TObject);
begin
  LabelTotal.Caption := 'Total geral: ' + FloatToStr(VSAggregate1.TotalValue);
end;
📂 Exportação/Importação
Sources.ExportToCSV(FileName) → Exporta os campos selecionados para CSV.

Sources.ImportFromCSV(FileName) → Importa campos de um arquivo CSV.

⚠️ Observações
O DataSet deve estar ativo antes de calcular.

Se AutoRecalc = True, não é necessário chamar CalcularTotal manualmente.

Alterar o DataSet limpa os campos selecionados em Sources.
