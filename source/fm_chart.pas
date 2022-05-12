unit fm_chart;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, TAGraph, TASeries, DividerBevel,
  Forms, Controls,
  ExtCtrls, ActnList, Buttons, StdCtrls, Spin, u_utilities, u_strings;

type

  { TfmChart }

  TfmChart = class(TForm)
    acMagnetLeft:       TAction;
    acMagnetRight:      TAction;
    acMagnetUp:         TAction;
    acMagnetDown:       TAction;
    alChart:            TActionList;
    chChart:            TChart;
    chChartLineSeries1: TLineSeries;
    cbGraphDataType:    TComboBox;
    gbMagnet:           TGroupBox;
    gbOther:            TGroupBox;
    imChart:            TImageList;
    pSettings:          TPanel;
    rgGraphDataType:    TRadioGroup;
    sbtnMagnetUp:       TSpeedButton;
    sbtnMagnetDown:     TSpeedButton;
    sbtnMagnetLeft:     TSpeedButton;
    sbtnMagnetRight:    TSpeedButton;
    shDivider:          TShape;
    seThickness:        TSpinEdit;
    tmrChart:           TTimer;

    procedure acMagnetExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure tmrChartTimer(Sender: TObject);

  PRIVATE
    FGraphData: String;
    FMainForm:  TForm;

    procedure SetGraphData(AValue: String);

  PUBLIC
    procedure UpdateGraph;
    procedure UpdateFormPosition;

    property GraphData: String read FGraphData write SetGraphData;
    property MainForm: TForm read FMainForm write FMainForm;
  end;

var
  fmChart: TfmChart;

implementation

{$R *.lfm}

const
  CHART_RAW_8  = 0;
  CHART_RAW_16 = 1;
  CHART_RAW_24 = 2;
  CHART_RAW_32 = 3;
  CHART_HEX    = 4;
  CHART_DEC    = 5;

  set10: set of Char = ['0'..'9', '+', '-', '.', ','];
  set16: set of Char = ['0'..'9', 'A'..'F', 'a'..'f'];

type
  TDoubleArr = array of Double;
  TDigitType = (dt10, dt16);


function ParseChars(s: String; ABytes: Integer): TDoubleArr;
  var
    i, j, len: Integer;
    x:         Longint;

  begin
    len := s.Length;
    len := len div ABytes;

    SetLength(Result, len);

    for i := 1 to len do
      begin
      x := 0;
      for j := 1 to ABytes do
        begin
        x *= $100;
        x += Ord(s[(i - 1) * ABytes + j]);
        end;

      Result[i - 1] := x;
      end;
  end;

function ParseDigits(s: String; ADigitType: TDigitType): TDoubleArr;
  var
    i, j, len: Integer;
    c:         Char;
    tmp:       String = '';

  begin
    len := s.Length;
    SetLength(Result, len);

    j := 0;
    for i := 1 to len do
      begin
      c := s[i];

      if ((ADigitType = dt10) and (c in set10)) or
        ((ADigitType = dt16) and (c in set16)) then
        tmp += c
      else
        if tmp <> '' then
          begin
            try
            case ADigitType of
              dt10: Result[j] := tmp.ToDouble;
              dt16: Result[j] := ('$' + tmp).ToInteger;
              end;

            j += 1;
            except
            end;
          tmp := '';
          end;
      end;

    SetLength(Result, j);
  end;


{ TfmChart }

procedure TfmChart.FormCreate(Sender: TObject);
  begin
    FGraphData := '';
  end;

procedure TfmChart.FormHide(Sender: TObject);
  begin
    tmrChart.Enabled := False;
  end;

procedure TfmChart.FormShow(Sender: TObject);
  begin
    tmrChart.Enabled := True;
    UpdateFormPosition;
    UpdateGraph;
  end;

procedure TfmChart.FormKeyPress(Sender: TObject; var Key: Char);
  begin
    case Ord(Key) of
      27:
        Close;
      end;
  end;


procedure TfmChart.tmrChartTimer(Sender: TObject);
  begin
    if fmChart.Visible then
      UpdateGraph;
  end;

procedure TfmChart.SetGraphData(AValue: String);
  begin
    if FGraphData = AValue then Exit;
    FGraphData := AValue;
  end;


procedure TfmChart.acMagnetExecute(Sender: TObject);
  begin
    Width  := 0;
    Height := 0;
    UpdateFormPosition;
  end;


procedure TfmChart.UpdateGraph;
  begin
    chChartLineSeries1.Clear;
    chChartLineSeries1.LinePen.Width := seThickness.Value;

    if FGraphData.Length > 0 then
      case cbGraphDataType.ItemIndex of
        CHART_RAW_8:
          chChartLineSeries1.AddArray(ParseChars(FGraphData, 1));

        CHART_RAW_16:
          chChartLineSeries1.AddArray(ParseChars(FGraphData, 2));

        CHART_RAW_24:
          chChartLineSeries1.AddArray(ParseChars(FGraphData, 3));

        CHART_RAW_32:
          chChartLineSeries1.AddArray(ParseChars(FGraphData, 4));

        CHART_HEX:
          chChartLineSeries1.AddArray(ParseDigits(FGraphData, dt16));

        CHART_DEC:
          chChartLineSeries1.AddArray(ParseDigits(FGraphData, dt10));
        end;
  end;

procedure TfmChart.UpdateFormPosition;
  var
    RM, RG: TRect;
  begin
    if fmChart.Visible then
      begin
      GetWindowRect(FMainForm.Handle, RM);
      GetWindowRect(fmChart.Handle, RG);

      case FMainForm.WindowState of

        // нормальное состояние - окно предпросмотра примагничено
        wsNormal:
          begin

          if not (acMagnetDown.Checked or acMagnetUp.Checked or
            acMagnetLeft.Checked or acMagnetRight.Checked) then
            begin
            Width  := 0;
            Height := 0;
            GetWindowRect(fmChart.Handle, RG);
            fmChart.Top  := RM.Bottom - RG.Bottom + RG.Top;
            fmChart.Left := RM.Right - RG.Right + RG.Left;
            if FMainForm.FormStyle = fsStayOnTop then
              fmChart.FormStyle := fsSystemStayOnTop else
              fmChart.FormStyle := fsStayOnTop;
            end;

          fmChart.FormStyle := FMainForm.FormStyle;

          // окно примагничено к левой стороне главного
          if acMagnetLeft.Checked then
            begin
            fmChart.Top    := FMainForm.Top;
            fmChart.Left   := FMainForm.Left + RG.Left - RG.Right;
            fmChart.Height := FMainForm.Height;
            end;

          // окно примагничено к правой стороне главного
          if acMagnetRight.Checked then
            begin
            fmChart.Top    := FMainForm.Top;
            fmChart.Left   := FMainForm.Left - RM.Left + RM.Right;
            fmChart.Height := FMainForm.Height;
            end;

          // окно примагничено к верху главного
          if acMagnetUp.Checked then
            begin
            fmChart.Top   := FMainForm.Top - RG.Bottom + RG.Top;
            fmChart.Left  := FMainForm.Left;
            fmChart.Width := FMainForm.Width;
            end;

          // окно примагничено к низу главного
          if acMagnetDown.Checked then
            begin
            fmChart.Top   := FMainForm.Top + RM.Bottom - RM.Top;
            fmChart.Left  := FMainForm.Left;
            fmChart.Width := FMainForm.Width;
            end;
          end;

        // развернутое состояние - окно предпросмотра в правом нижнем углу
        wsMaximized:
          begin
          if FMainForm.FormStyle = fsStayOnTop then
            fmChart.FormStyle := fsSystemStayOnTop else
            fmChart.FormStyle := fsStayOnTop;
          //fmChart.Top       := 0;
          //fmChart.Left      := 0;
          Width := 0;
          Height := 0;
          GetWindowRect(fmChart.Handle, RG);
          fmChart.Top  := RM.Bottom + RM.Top - RG.Bottom + RG.Top;
          fmChart.Left := RM.Right + RM.Left - RG.Right + RG.Left;
          end;
        end;
      end;
  end;

end.
