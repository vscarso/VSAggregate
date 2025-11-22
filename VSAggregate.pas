unit VSAggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Math; // Math necessário para Infinity

type
  TAggregateKind = (akSum, akAvg, akCount, akMax, akMin);

  // Item da coleção: dataset + campo a ser agregado
  TSumSource = class(TCollectionItem)
  private
    FDataSet: TDataSet;
    FFieldName: string;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TSumSources = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSumSource;
    procedure SetItem(Index: Integer; AValue: TSumSource);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TSumSource read GetItem write SetItem; default;
  end;

  TVSAggregate = class(TComponent)
  private
    FSources: TSumSources;
    FTargetDataSet: TDataSet;
    FTargetFieldName: string;
    FTotalValue: Double;
    FAutoRecalc: Boolean;
    FAggregateKind: TAggregateKind;
    FDecimalPlaces: Integer;
    FOnAfterCalculate: TNotifyEvent;

    procedure SetSources(AValue: TSumSources);
    procedure SetTargetDataSet(ADataSet: TDataSet);
    procedure SetAutoRecalc(AValue: Boolean);
    procedure DataSetChanged(DataSet: TDataSet);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure CalcularTotal;
    procedure GravarTotal;

    property TotalValue: Double read FTotalValue;

  published
    property Sources: TSumSources read FSources write SetSources;
    property TargetDataSet: TDataSet read FTargetDataSet write SetTargetDataSet;
    property TargetFieldName: string read FTargetFieldName write FTargetFieldName;
    property AutoRecalc: Boolean read FAutoRecalc write SetAutoRecalc;

    property AggregateKind: TAggregateKind read FAggregateKind write FAggregateKind default akSum;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces default 2;

    property OnAfterCalculate: TNotifyEvent read FOnAfterCalculate write FOnAfterCalculate;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('VSComponents', [TVSAggregate]);
end;

{ TSumSources }

constructor TSumSources.Create(AOwner: TPersistent);
begin
  inherited Create(TSumSource);
  FOwner := AOwner;
end;

function TSumSources.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TSumSources.GetItem(Index: Integer): TSumSource;
begin
  Result := TSumSource(inherited Items[Index]);
end;

procedure TSumSources.SetItem(Index: Integer; AValue: TSumSource);
begin
  Items[Index].Assign(AValue);
end;

{ TVSAggregate }

constructor TVSAggregate.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSources := TSumSources.Create(Self);
  FTotalValue := 0;
  FAutoRecalc := False;
  FAggregateKind := akSum;
  FDecimalPlaces := 2;
end;

destructor TVSAggregate.Destroy;
begin
  FSources.Free;
  inherited Destroy;
end;

procedure TVSAggregate.Notification(AComponent: TComponent; Operation: TOperation);
var
  i: Integer;
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) then
  begin
    if AComponent = FTargetDataSet then
      FTargetDataSet := nil;

    for i := 0 to FSources.Count - 1 do
      if FSources[i].DataSet = AComponent then
        FSources[i].DataSet := nil;
  end;
end;

procedure TVSAggregate.SetSources(AValue: TSumSources);
begin
  if FSources = AValue then Exit;
  FSources.Assign(AValue);
end;

procedure TVSAggregate.SetTargetDataSet(ADataSet: TDataSet);
begin
  if FTargetDataSet = ADataSet then Exit;
  FTargetDataSet := ADataSet;
  if Assigned(FTargetDataSet) then
    FTargetDataSet.FreeNotification(Self);
end;

procedure TVSAggregate.SetAutoRecalc(AValue: Boolean);
begin
  FAutoRecalc := AValue;
end;

procedure TVSAggregate.DataSetChanged(DataSet: TDataSet);
begin
  CalcularTotal;
end;

procedure TVSAggregate.CalcularTotal;
var
  i, Count: Integer;
  DS: TDataSet;
  FieldName: string;
  Value: Double;
  MaxVal, MinVal: Double;
begin
  FTotalValue := 0;
  Count := 0;
  MaxVal := -Infinity;
  MinVal := Infinity;

  for i := 0 to FSources.Count - 1 do
  begin
    DS := FSources[i].DataSet;
    FieldName := FSources[i].FieldName;

    if Assigned(DS) and (FieldName <> '') then
    begin
      DS.DisableControls;
      try
        DS.First;
        while not DS.EOF do
        begin
          Value := DS.FieldByName(FieldName).AsFloat;
          case FAggregateKind of
            akSum: FTotalValue := FTotalValue + Value;
            akCount: Inc(Count);
            akAvg: begin
              FTotalValue := FTotalValue + Value;
              Inc(Count);
            end;
            akMax: if Value > MaxVal then MaxVal := Value;
            akMin: if Value < MinVal then MinVal := Value;
          end;
          DS.Next;
        end;
      finally
        DS.EnableControls;
      end;
    end;
  end;

  case FAggregateKind of
    akAvg: if Count > 0 then FTotalValue := FTotalValue / Count;
    akCount: FTotalValue := Count;
    akMax: if MaxVal <> -Infinity then FTotalValue := MaxVal;
    akMin: if MinVal <> Infinity then FTotalValue := MinVal;
  end;

  // Arredondar casas decimais
  FTotalValue := RoundTo(FTotalValue, -FDecimalPlaces);

  if Assigned(FOnAfterCalculate) then
    FOnAfterCalculate(Self);
end;

procedure TVSAggregate.GravarTotal;
begin
  if Assigned(FTargetDataSet) and (FTargetFieldName <> '') then
  begin
    if not (FTargetDataSet.State in [dsEdit, dsInsert]) then
      FTargetDataSet.Edit;
    FTargetDataSet.FieldByName(FTargetFieldName).AsFloat := FTotalValue;
    FTargetDataSet.Post;
  end;
end;

end.
