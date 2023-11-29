unit fm_insertchar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics,
  Grids, ExtCtrls, Buttons, ActnList, LCLType, ComCtrls, u_encodings, Types;

resourcestring
  TXT_INSERT_CHAR = '%s, символ <%s>';

type

  { TfmASCIIChar }

  TfmASCIIChar = class(TForm)
    acCancel:    TAction;
    acOK:        TAction;
    ActionList1: TActionList;
    bbInsert:    TBitBtn;
    bbCancel:    TBitBtn;
    pControl:    TPanel;
    pDivide:     TPanel;
    rgCodeType:  TRadioGroup;
    sgChar:      TStringGrid;
    sbStatus:    TStatusBar;

    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgCodeTypeClick(Sender: TObject);
    procedure sgCharDblClick(Sender: TObject);
    procedure sgCharDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect; aState: TGridDrawState);
    procedure sgCharMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sgCharPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure sgCharSelectCell(Sender: TObject; aCol, aRow: Integer; var CanSelect: Boolean);

  private
    FEncoding:     String;
    FSelectedChar: Integer;

    function GetCharName(code: Integer): String;
    function GetCharNameFull(code: Integer): String;
    procedure CreateTable;

  public
    property Encoding: String read FEncoding write FEncoding;
    property SelectedChar: Integer read FSelectedChar;
  end;

const
  CT_HEX = 0;
  CT_BIN = 1;
  CT_DEC = 2;
  CT_NON = 3;

var
  fmASCIIChar: TfmASCIIChar;
  first_show:  Boolean = True;

implementation

{$R *.lfm}

 { TfmASCIIChar }

 // получение имени символа по его коду
function TfmASCIIChar.GetCharName(code: Integer): String;
  const
    // https://ru.wikipedia.org/wiki/Управляющие_символы
    char_name: array [0..31] of String = (
      'NUL', 'SOH', 'STX', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL',
      'BS', 'HT', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI',
      'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB',
      'CAN', 'EM', 'SUB', 'ESC', 'FS', 'GS', 'RS', 'US');
    char_del = 'DEL';
  begin
    case code of
      0..31: Result := char_name[code];
      127: Result   := char_del;
      else
        Result      := EncodingToUTF8(Char(code), FEncoding);
      end;
  end;

    // получение полного имени символа по его коду
function TfmASCIIChar.GetCharNameFull(code: Integer): String;
  const
    // https://ru.wikipedia.org/wiki/Управляющие_символы
    char_name_full: array [0..31] of String = (
      'NULL',
      'START OF HEADING',
      'START OF TEXT',
      'END OF TEXT',
      'END OF TRANSMISSION',
      'ENQUIRY',
      'ACKNOWLEDGE',
      'BELL',
      'BACKSPACE',
      'CHARACTER TABULATION (horizontal tabulation)',
      'LINE FEED',
      'LINE TABULATION (vertical tabulation)',
      'FORM FEED',
      'CARRIAGE RETURN',
      'SHIFT OUT (locking-shift one)',
      'SHIFT IN (locking-shift zero)',
      'DATA LINK ESCAPE',
      'DEVICE CONTROL ONE',
      'DEVICE CONTROL TWO',
      'DEVICE CONTROL THREE',
      'DEVICE CONTROL FOUR',
      'NEGATIVE ACKNOWLEDGE',
      'SYNCHRONOUS IDLE',
      'END TRANSMISSION BLOCK',
      'CANCEL',
      'END OF MEDIUM',
      'SUBSTITUTE',
      'ESCAPE',
      'INFORMATION SEPARATOR FOUR (file separator)',
      'INFORMATION SEPARATOR THREE (group separator)',
      'INFORMATION SEPARATOR TWO (record separator)',
      'INFORMATION SEPARATOR ONE (unit separator)');
    char_del_full = 'DELETE';
  begin
    case code of
      0..31: Result := char_name_full[code];
      127: Result   := char_del_full;
      else
        Result      := EncodingToUTF8(Char(code), FEncoding);
      end;
  end;

procedure TfmASCIIChar.CreateTable;
  var
    x, y, c: Integer;
    s:       String;
  begin
    with sgChar do
      begin
      for x := 0 to 7 do
        for y := 0 to 31 do
          begin
          s := '';
          c := y * 8 + x;
          s += GetCharName(c);

          Cells[x, y]   := s;
          RowHeights[y] := Round(Canvas.GetTextHeight('0') * 2.7);
          end;
      end;
  end;


procedure TfmASCIIChar.FormCreate(Sender: TObject);
  var
    i: Integer;
  begin
    with sgChar do
      begin
      with sgChar do
        begin
        for i := 0 to 7 do
          Columns.Add.Alignment := taCenter;

        RowCount        := 32;
        DefaultColWidth := 64;
        end;

      with Constraints do
        begin
        MinHeight := DefaultColWidth * 4;
        MinWidth  := MinHeight * 2 + VertScrollBar.Size + 4;
        MinHeight := MinHeight + 4;
        end;
      end;
  end;

procedure TfmASCIIChar.FormShow(Sender: TObject);
  begin
    if first_show then
      begin
      first_show := False;
      AutoSize   := True;
      AutoSize   := False;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;
      sgChar.ColRow         := Point(0, 0);
      end;

    CreateTable;
  end;


procedure TfmASCIIChar.rgCodeTypeClick(Sender: TObject);
  begin
    CreateTable;
  end;

procedure TfmASCIIChar.sgCharDblClick(Sender: TObject);
  begin
    acOK.Execute;
  end;

procedure TfmASCIIChar.sgCharDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
  var
    code:      String;
    c:         Integer;
    show_rect: TRect;
  begin
    with sgChar.Canvas do
      begin
      c := aCol + aRow * 8;

      case rgCodeType.ItemIndex of
        CT_HEX: code := c.ToHexString(2);
        CT_BIN: code := IntToBin(c, 8);
        CT_DEC: code := c.ToString;
        CT_NON: code := '';
        end;

      if (sgChar.Selection.Top <> aRow) or (sgChar.Selection.Left <> aCol) then
        if (aCol mod 2 = 0) xor (aRow mod 2 = 0) then
          Brush.Color := clBtnFace else
          Brush.Color := cl3DLight;

      Pen.Style := psClear;
      with aRect do
        Rectangle(Left, Top, Right + 1, Bottom + 1);

      if rgCodeType.ItemIndex = CT_NON then
        show_rect   := aRect else
        with aRect do
          show_rect := Rect(Left, Top, Right, Top + Round((Bottom - Top) * 0.6));

      Font.Height := Round(sgChar.Font.Height * 1.6);
      TextRect(show_rect, Left, Top, sgChar.Cells[aCol, aRow]);

      Font.Height := sgChar.Font.Height;
      with aRect do
        TextRect(Rect(Left, Top + Round((Bottom - Top) * 0.6), Right, Bottom),
          Left, Top, code);
      end;
  end;

procedure TfmASCIIChar.sgCharMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if Shift = [ssCtrl] then
      with sgChar.Font do
        begin
        if WheelDelta > 0 then
          Size := Size + 1;

        if (WheelDelta < 0) and (Size > 5) then
          Size := Size - 1;
        end;
  end;

procedure TfmASCIIChar.sgCharPrepareCanvas(Sender: TObject;
  aCol, aRow: Integer; aState: TGridDrawState);
  var
    ts: TTextStyle;
  begin
    with sgChar do
      begin
      ts               := Canvas.TextStyle;
      ts.SingleLine    := False;
      //ts.Wordbreak  := True;
      Canvas.TextStyle := ts;
      end;
  end;

procedure TfmASCIIChar.sgCharSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
  var
    c: Integer;
  begin
    c       := aCol + aRow * 8;
    Caption := Format(TXT_INSERT_CHAR, [GetEncodingCaption(Encoding), GetCharName(c)]);

    sbStatus.Panels.Items[0].Text := '0x' + c.ToHexString(2);
    sbStatus.Panels.Items[1].Text := c.ToString;
    sbStatus.Panels.Items[2].Text := '0b' + intToBin(c, 8);
    sbStatus.Panels.Items[3].Text := GetCharNameFull(c);
  end;


procedure TfmASCIIChar.acOKExecute(Sender: TObject);
  begin
    with sgChar.Selection do
      FSelectedChar := Top * 8 + Left;

    //ShowMessage(FSelectedChar.ToHexString(2));
    ModalResult := mrOk;
  end;

procedure TfmASCIIChar.acCancelExecute(Sender: TObject);
  begin
    ModalResult := mrCancel;
  end;

end.
