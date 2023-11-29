unit u_plotter_charts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  u_plotter_types, u_plotter_points;

type

  { TPlotterChartsList }

  TPlotterChartsList = class(TFPList)
  private
    FPointsMax:   Integer;
    FMax:         Integer;
    FActiveLines: Integer;
    function GetItem(Index: Integer): TPlotterPoints;

  public
    constructor Create(AMax: Integer);
    destructor Destroy; override;

    procedure Clear(ACaption: String);
    procedure Clear(AIndex: Integer);
    procedure ClearAll;
    procedure Reset;

    procedure AddingStart;
    procedure AddingFinish(X: Double; Y: Double = NaN);

    function Add(ACaption: String): Integer;
    function Add(ACaption: String; X, Y: Double): Integer;
    function Add(ACaption: String; AAlternateIndex: Integer; X, Y: Double): Integer;
    function Add(AIndex: Integer; X, Y: Double): Integer;

    property Item[Index: Integer]: TPlotterPoints read GetItem; default;
    property ActiveLines: Integer read FActiveLines;

  end;


implementation


{ TPlotterChartsList }

function TPlotterChartsList.GetItem(Index: Integer): TPlotterPoints;
  begin
    if Index < Count then
      Result := TPlotterPoints(Items[Index])
    else
      Result := nil;
  end;

constructor TPlotterChartsList.Create(AMax: Integer);
  var
    i: Integer;
  begin
    inherited Create;

    FMax         := AMax;
    FActiveLines := 0;

    for i := 0 to FMax - 1 do
      begin
      inherited Add(TPlotterPoints.Create);
      end;
  end;

destructor TPlotterChartsList.Destroy;
  var
    i: Integer;
  begin
    if Count <> 0 then
      for i := 0 to Count - 1 do
        TPlotterPoints(Items[i]).Destroy;

    inherited Destroy;
  end;

procedure TPlotterChartsList.Clear(ACaption: String);
  begin
    Item[Add(ACaption)].Clear;
  end;

procedure TPlotterChartsList.Clear(AIndex: Integer);
  begin
    if AIndex >= Count then Exit;
    Item[AIndex].Clear;
  end;

procedure TPlotterChartsList.ClearAll;
  var
    i: Integer;
  begin
    for i := 0 to Count - 1 do
      Clear(i);
  end;

procedure TPlotterChartsList.Reset;
  begin
    FActiveLines := 0;
    ClearAll;
  end;

procedure TPlotterChartsList.AddingStart;
  begin
    if Count <> 0 then
      FPointsMax := Item[0].Count
    else
      FPointsMax := 0;
  end;

procedure TPlotterChartsList.AddingFinish(X: Double; Y: Double);
  var
    i: Integer;
  begin
    if Count <> 0 then
      for i := 0 to Count - 1 do
        if Item[i].Count = FPointsMax then
          Item[i].AddPoint(X, Y);
  end;

function TPlotterChartsList.Add(ACaption: String): Integer;
  begin
    if Count = 0 then Exit(-1);

    for Result := 0 to Count - 1 do
      if Item[Result].Caption = ACaption then
        Exit
      else
      if Item[Result].Caption = '' then
        begin
        Item[Result].SetCaption(ACaption);
        FActiveLines += 1;
        Exit;
        end;

    Result := -1;
  end;

function TPlotterChartsList.Add(ACaption: String; X, Y: Double): Integer;
  begin
    Result := Add(ACaption);

    if Result >= 0 then
      Item[Result].AddPoint(X, Y);
  end;

function TPlotterChartsList.Add(ACaption: String; AAlternateIndex: Integer; X,
  Y: Double): Integer;
  begin
    if ACaption <> '' then
      Result := Add(ACaption, X, Y)
    else
      Result := Add(AAlternateIndex, X, Y);
  end;

function TPlotterChartsList.Add(AIndex: Integer; X, Y: Double): Integer;
  begin
    if AIndex >= Count then Exit(-1);

    Result := AIndex;
    Item[Result].AddPoint(X, Y);

    if Item[Result].Caption = '' then
      begin
      FActiveLines += 1;
      Item[Result].SetCaption('@' + (AIndex + 1).ToString);
      end;
  end;

end.
