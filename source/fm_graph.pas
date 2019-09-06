unit fm_graph;

{$mode objfpc}{$H+}

interface

uses
  Classes, Windows, SysUtils, FileUtil, TAGraph, TASeries, TAStyles,
  TAIntervalSources, TANavigation, TATools, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ActnList, ComCtrls, Buttons, StdCtrls;

type

  { TfmGraph }

  TfmGraph = class(TForm)
    acMagnetLeft:      TAction;
    acMagnetRight:     TAction;
    acMagnetUp:        TAction;
    acMagnetDown:      TAction;
    ActionList1:       TActionList;
    Chart1:            TChart;
    Chart1LineSeries1: TLineSeries;
    gbMagnet:          TGroupBox;
    ImageList1:        TImageList;
    pSettings:         TPanel;
    rgGraphDataType:   TRadioGroup;
    SpeedButton1:      TSpeedButton;
    SpeedButton2:      TSpeedButton;
    SpeedButton3:      TSpeedButton;
    SpeedButton4:      TSpeedButton;
    Timer1:            TTimer;
    procedure acMagnetExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
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
  fmGraph: TfmGraph;

implementation

{$R *.lfm}

const
  DATA_TYPE_8_BIT  = 0;
  DATA_TYPE_16_BIT = 1;
  DATA_TYPE_24_BIT = 2;
  DATA_TYPE_32_BIT = 3;
  DATA_TYPE_HEX    = 4;
  DATA_TYPE_DEC    = 5;

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

{ TfmGraph }

procedure TfmGraph.FormCreate(Sender: TObject);
  begin
    FGraphData := '';
  end;

procedure TfmGraph.FormHide(Sender: TObject);
  begin
    Timer1.Enabled := False;
  end;

procedure TfmGraph.FormShow(Sender: TObject);
  begin
    Timer1.Enabled := True;
    UpdateFormPosition;
  end;

procedure TfmGraph.FormKeyPress(Sender: TObject; var Key: Char);
  begin
    case Ord(Key) of
      27:
        Close;
      end;
  end;


procedure TfmGraph.Timer1Timer(Sender: TObject);
  begin
    if fmGraph.Visible then
      UpdateGraph;
  end;

procedure TfmGraph.SetGraphData(AValue: String);
  begin
    if FGraphData = AValue then Exit;
    FGraphData := AValue;
  end;


procedure TfmGraph.acMagnetExecute(Sender: TObject);
  begin
    Width  := 0;
    Height := 0;
    UpdateFormPosition;
  end;


procedure TfmGraph.UpdateGraph;
  begin
    Chart1LineSeries1.Clear;

    if FGraphData.Length > 0 then
      case rgGraphDataType.ItemIndex of
        DATA_TYPE_8_BIT:
          Chart1LineSeries1.AddArray(ParseChars(FGraphData, 1));

        DATA_TYPE_16_BIT:
          Chart1LineSeries1.AddArray(ParseChars(FGraphData, 2));

        DATA_TYPE_24_BIT:
          Chart1LineSeries1.AddArray(ParseChars(FGraphData, 3));

        DATA_TYPE_32_BIT:
          Chart1LineSeries1.AddArray(ParseChars(FGraphData, 4));

        DATA_TYPE_HEX:
          Chart1LineSeries1.AddArray(ParseDigits(FGraphData, dt16));

        DATA_TYPE_DEC:
          Chart1LineSeries1.AddArray(ParseDigits(FGraphData, dt10));
        end;
  end;

procedure TfmGraph.UpdateFormPosition;
  var
    RM, RG: TRect;
  begin
    if fmGraph.Visible then
      begin
      GetWindowRect(FMainForm.Handle, RM);
      GetWindowRect(fmGraph.Handle, RG);

      case FMainForm.WindowState of

        // нормальное состояние - окно предпросмотра примагничено
        wsNormal:
          begin

          if not (acMagnetDown.Checked or acMagnetUp.Checked or
            acMagnetLeft.Checked or acMagnetRight.Checked) then
            begin
            Width  := 0;
            Height := 0;
            GetWindowRect(fmGraph.Handle, RG);
            fmGraph.Top       := RM.Bottom - RG.Bottom + RG.Top;
            fmGraph.Left      := RM.Right - RG.Right + RG.Left;
            fmGraph.FormStyle := fsStayOnTop;
            end;

          fmGraph.FormStyle := fsNormal;

          // окно примагничено к левой стороне главного
          if acMagnetLeft.Checked then
            begin
            fmGraph.Top    := FMainForm.Top;
            fmGraph.Left   := FMainForm.Left + RG.Left - RG.Right;
            fmGraph.Height := FMainForm.Height;
            end;

          // окно примагничено к правой стороне главного
          if acMagnetRight.Checked then
            begin
            fmGraph.Top    := FMainForm.Top;
            fmGraph.Left   := FMainForm.Left - RM.Left + RM.Right;
            fmGraph.Height := FMainForm.Height;
            end;

          // окно примагничено к верху главного
          if acMagnetUp.Checked then
            begin
            fmGraph.Top   := FMainForm.Top - RG.Bottom + RG.Top;
            fmGraph.Left  := FMainForm.Left;
            fmGraph.Width := FMainForm.Width;
            end;

          // окно примагничено к низу главного
          if acMagnetDown.Checked then
            begin
            fmGraph.Top   := FMainForm.Top + RM.Bottom - RM.Top;
            fmGraph.Left  := FMainForm.Left;
            fmGraph.Width := FMainForm.Width;
            end;
          end;

        // развернутое состояние - окно предпросмотра в правом нижнем углу
        wsMaximized:
          begin
          fmGraph.FormStyle := fsStayOnTop;
          //fmGraph.Top       := 0;
          //fmGraph.Left      := 0;
          Width             := 0;
          Height            := 0;
          GetWindowRect(fmGraph.Handle, RG);
          fmGraph.Top  := RM.Bottom + RM.Top - RG.Bottom + RG.Top;
          fmGraph.Left := RM.Right + RM.Left - RG.Right + RG.Left;
          end;
        end;
      end;
  end;

end.
