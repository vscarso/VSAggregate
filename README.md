📘 Documentação do Componente TVSAggregate
1. Instalação no Lazarus

Instalar o pacote

Vá em Pacotes → Instalar/Desinstalar Pacotes….

Mova VSComponents para a lista de instalados.

Clique em Salvar e reconstruir IDE.

Reinicie o Lazarus.

👉 Após isso, o componente TVSAggregate aparecerá na paleta VSComponents.

2. Dependências
LCL → Biblioteca de componentes visuais do Lazarus.

FCL-DB → Biblioteca de acesso a dados (necessária para TDataSet).

ZeosLib (opcional) → Se você usa TZQuery ou outros componentes Zeos, o TVSAggregate funciona normalmente, pois herda de TDataSet.

3. Propriedades
Propriedade	Tipo	Descrição
Sources	TSumSources	Coleção de datasets/campos de origem. Cada item tem DataSet + FieldName.
TargetDataSet	TDataSet	Dataset de destino (ex.: cabeçalho da nota).
TargetFieldName	string	Campo do dataset de destino onde o total será gravado.
AutoRecalc	Boolean	Se True, recalcula automaticamente quando os datasets mudam.
AggregateKind	TAggregateKind	Tipo de agregação: akSum, akAvg, akCount, akMax, akMin.
DecimalPlaces	Integer	Número de casas decimais no resultado.
TotalValue	Double	Valor calculado (somente leitura).
4. Métodos
CalcularTotal

Percorre todos os datasets configurados em Sources.

Aplica o tipo de agregação (AggregateKind).

Atualiza TotalValue.

Dispara o evento OnAfterCalculate.

GravarTotal

Grava o valor de TotalValue no campo definido em TargetFieldName do TargetDataSet.

Se o dataset não estiver em edição, entra em Edit e dá Post.

5. Eventos
OnAfterCalculate

Disparado sempre que o cálculo termina.

Útil para atualizar a interface ou executar lógica adicional.

Exemplo:

pascal
procedure TForm1.VSaggregateAfterCalculate(Sender: TObject);
begin
  LabelTotal.Caption := FloatToStr(VSaggregate.TotalValue);
end;
6. Exemplo de uso
Configuração no Object Inspector
Sources[0].DataSet = ZQueryItens

Sources[0].FieldName = Valor

Sources[1].DataSet = ZQueryImpostos

Sources[1].FieldName = Aliquota

TargetDataSet = ZQueryNota

TargetFieldName = TotalNota

AggregateKind = akSum

DecimalPlaces = 2

AutoRecalc = True

Código
pascal
procedure TForm1.FormCreate(Sender: TObject);
begin
  VSaggregate.OnAfterCalculate := @VSaggregateAfterCalculate;
end;

procedure TForm1.ButtonCalcularClick(Sender: TObject);
begin
  VSaggregate.CalcularTotal;
  ShowMessage('Total calculado: ' + FloatToStr(VSaggregate.TotalValue));
end;

procedure TForm1.ButtonGravarClick(Sender: TObject);
begin
  VSaggregate.GravarTotal;
end;

procedure TForm1.VSaggregateAfterCalculate(Sender: TObject);
begin
  LabelTotal.Caption := FormatFloat('0.00', VSaggregate.TotalValue);
end;
7. Observações importantes
Ordem origem → destino: configure primeiro os datasets e campos em Sources, depois o destino (TargetDataSet + TargetFieldName).

AutoRecalc: se ativado, o componente intercepta eventos dos datasets (AfterPost, AfterDelete, AfterScroll) e recalcula automaticamente.

Eventos originais preservados: o componente encadeia os handlers originais, não sobrescreve.

Performance: em datasets grandes, o cálculo percorre todos os registros. Se precisar de mais performance, use SQL com SUM diretamente no banco.

DecimalPlaces: controla arredondamento do resultado.
