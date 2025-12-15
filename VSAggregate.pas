unit VSAggregate;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DB, Math, Variants;

type
  TAggregateKind = (akSum, akAvg, akCount, akMax, akMin);

  TSumSource = class(TCollectionItem)
  private
    FDataSet: TDataSet;
    FFieldName: string;
    FTotalValue: Double;
    FFieldType: TFieldType;
    procedure SetDataSet(ADataSet: TDataSet);
  protected
    function GetDisplayName: string; override;
  public
    property TotalValue: Double read FTotalValue;
    property FieldType: TFieldType read FFieldType;
  published
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TSumSources = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TSumSource;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TSumSource; reintroduce;
    procedure ExportToCSV(const FileName: string);
    procedure ImportFromCSV(const FileName: string);
    property Items[Index: Integer]: TSumSource read GetItem; default;
  end;

  TVSAggregate = class(TComponent)
  private
    FSources: TSumSources;
    FTotalValue: Double;
    FAutoRecalc: Boolean;
    FAggregateKind: TAggregateKind;
    FDecimalPlaces: Integer;
    FOnAfterCalculate: TNotifyEvent;
    FDataSet: TDataSet;   // 🔹 nova propriedade
    procedure SetSources(AValue: TSumSources);
    procedure SetAutoRecalc(AValue: Boolean);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CalcularTotal;
    function Total(const FieldName: string): Variant;
    property TotalValue: Double read FTotalValue;
  published
    property DataSet: TDataSet read FDataSet write FDataSet;   // 🔹 publicada
    property Sources: TSumSources read FSources write SetSources;
    property AutoRecalc: Boolean read FAutoRecalc write SetAutoRecalc;
    property AggregateKind: TAggregateKind read FAggregateKind write FAggregateKind default akSum;
    property DecimalPlaces: Integer read FDecimalPlaces write FDecimalPlaces default 2;
    property OnAfterCalculate: TNotifyEvent read FOnAfterCalculate write FOnAfterCalculate;
  end;

implementation

{ TSumSource }

procedure TSumSource.SetDataSet(ADataSet: TDataSet);
begin
  FDataSet := ADataSet;
end;

function TSumSource.GetDisplayName: string;
begin
  if Assigned(FDataSet) then
  begin
    if FFieldName <> '' then
      Result := FDataSet.Name + '.' + FFieldName
    else
      Result := FDataSet.Name + ' (sem campo)';
  end
  else
    Result := '(sem dataset).' + FFieldName;
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

function TSumSources.Add: TSumSource;
begin
  Result := TSumSource(inherited Add);
end;

function TSumSources.GetItem(Index: Integer): TSumSource;
begin
  Result := TSumSource(inherited Items[Index]);
end;

procedure TSumSources.ExportToCSV(const FileName: string);
var
  SL: TStringList;
  i: Integer;
begin
  SL := TStringList.Create;
  try
    for i := 0 to Count - 1 do
      SL.Add(Items[i].DataSet.Name + ';' + Items[i].FieldName);
    SL.SaveToFile(FileName);
  finally
    SL.Free;
  end;
end;

procedure TSumSources.ImportFromCSV(const FileName: string);
var
  SL: TStringList;
  i: Integer;
  Line, Field: string;
  p: SizeInt;
  Item: TSumSource;
begin
  SL := TStringList.Create;
  try
    SL.LoadFromFile(FileName);
    Clear;
    for i := 0 to SL.Count - 1 do
    begin
      Line := SL[i];
      p := Pos(';', Line);
      if p > 0 then
        Field := Copy(Line, p + 1, Length(Line) - p)
      else
        Field := Line;
      Item := Add;
      Item.FieldName := Field;
    end;
  finally
    SL.Free;
  end;
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
    if AComponent = FDataSet then
      FDataSet := nil;
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

procedure TVSAggregate.SetAutoRecalc(AValue: Boolean);
begin
  FAutoRecalc := AValue;
end;

procedure TVSAggregate.CalcularTotal;
var
  i: Integer;
  DS: TDataSet;
  FieldName: string;
  Value: Double;
begin
  FTotalValue := 0;
  for i := 0 to FSources.Count - 1 do
  begin
    DS := FSources[i].DataSet;
    FieldName := FSources[i].FieldName;
    FSources[i].FTotalValue := 0;

    if not (Assigned(DS) and DS.Active) then
      Continue;

    DS.First;
    while not DS.EOF do
    begin
      Value := DS.FieldByName(FieldName).AsFloat;
      FSources[i].FTotalValue := FSources[i].FTotalValue + Value;
      FTotalValue := FTotalValue + Value;
      FSources[i].FFieldType := DS.FieldByName(FieldName).DataType;
      DS.Next;
    end;
  end;

  if Assigned(FOnAfterCalculate) then
    FOnAfterCalculate(Self);
end;

function TVSAggregate.Total(const FieldName: string): Variant;
var
  i: Integer;
begin
  Result := Null;
  for i := 0 to FSources.Count - 1 do
    if SameText(FSources[i].FieldName, FieldName) then
    begin
      case FSources[i].FieldType of
        ftSmallint, ftInteger, ftWord, ftLargeint:
          Result := Round(FSources[i].TotalValue);
        ftCurrency:
          Result := Currency(FSources[i].TotalValue);
        ftFloat, ftBCD, ftFMTBcd:
          Result := FSources[i].TotalValue;
      else
        Result := FSources[i].TotalValue;
      end;
      Exit;
    end;
end;

end.

