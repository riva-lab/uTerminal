unit u_plotter_points;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas, u_plotter_types, Graphics;

type

  { TPlotterPoints }

  TPlotterPoints = class

  private

    FPoints:         TDoublePointArray;
    FCaption:        String;
    FCount:          Integer;
    FDefaultCaption: Boolean;
    FWidth:          Integer;
    FStyle:          TPlotterPenStyle;
    FColor:          TColor;
    FPointSize:      Integer;
    FCustomColor:    Boolean;
    procedure SetColor(AValue: TColor);

  public
    procedure AddPoint(X, Y: Double);
    procedure SetCaption(ACaption: String);
    procedure Clear;

    constructor Create;
    destructor Destroy; override;

    property Points: TDoublePointArray read FPoints;
    property Caption: String read FCaption;
    property DefaultCaption: Boolean read FDefaultCaption write FDefaultCaption;
    property Count: Integer read FCount;
    property Width: Integer read FWidth write FWidth;
    property Style: TPlotterPenStyle read FStyle write FStyle;
    property Color: TColor read FColor write SetColor;
    property CustomColor: Boolean read FCustomColor;
    property PointSize: Integer read FPointSize write FPointSize;
  end;


implementation


  { TPlotterPoints }

procedure TPlotterPoints.SetColor(AValue: TColor);
  begin
    if FColor = AValue then Exit;
    FColor       := AValue;
    FCustomColor := True;
  end;

procedure TPlotterPoints.AddPoint(X, Y: Double);
  //var
  //  a: SizeInt;
  begin
    while Length(FPoints) <= FCount do
      SetLength(FPoints, Length(FPoints) + 32);

    //a := Length(FPoints);

    FPoints[FCount].X := X;
    FPoints[FCount].Y := Y;
    FCount            += 1;
  end;

procedure TPlotterPoints.SetCaption(ACaption: String);
  begin
    if ACaption = FCaption then Exit;
    FCaption := ACaption;

    if Length(FCaption) > 0 then
      FDefaultCaption := FCaption[1] = '@';
  end;

procedure TPlotterPoints.Clear;
  begin
    FCount := 0;
    SetLength(FPoints, 32);
  end;

constructor TPlotterPoints.Create;
  begin
    FCaption       := '';
    DefaultCaption := False;
    FWidth         := 1;
    FStyle         := ppsSolid;
    FColor         := clBlack;
    FCustomColor   := False;
    FPointSize     := -1;
    Clear;
  end;

destructor TPlotterPoints.Destroy;
  begin
    SetLength(FPoints, 0);
    inherited Destroy;
  end;

end.
