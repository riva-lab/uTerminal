unit fm_insertchar;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, LazUTF8, Forms, Controls, Graphics,
  Grids, ExtCtrls, Buttons, ActnList, LCLType, ComCtrls, Types;

resourcestring
  FORM_ASCII_CAPTION = 'Вставить символ ASCII';

type

  { TfmASCIIChar }

  TfmASCIIChar = class(TForm)
    acOK:        TAction;
    acCancel:    TAction;
    ActionList1: TActionList;
    BitBtn1:     TBitBtn;
    BitBtn2:     TBitBtn;
    pDivide:     TPanel;
    Panel2:      TPanel;
    rgCodeType:  TRadioGroup;
    sgChar:      TStringGrid;
    StatusBar:   TStatusBar;
    procedure acCancelExecute(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgCodeTypeClick(Sender: TObject);
    procedure sgCharDblClick(Sender: TObject);
    procedure sgCharDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgCharMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sgCharPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure sgCharResize(Sender: TObject);
    procedure sgCharSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure sgCharUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  PRIVATE
    FSelectedChar: Integer;

    procedure CreateTable;
  PUBLIC
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

// получение имени символа по его коду
function GetCharName(code: Integer): String;
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
        Result := WinCPToUTF8(Char(code));
      end;
  end;

// получение полного имени символа по его коду
function GetCharNameFull(code: Integer): String;
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
        Result := WinCPToUTF8(Char(code));
      end;
  end;


{ TfmASCIIChar }

procedure TfmASCIIChar.CreateTable;
  var
    x, y, c: Integer;
    code, s: String;
  begin

    with sgChar do
      begin
      for x := 0 to 7 do
        for y := 0 to 31 do
          begin
          s := '';
          c := y * 8 + x;
          s += GetCharName(c);

          //if rgCodeType.ItemIndex <> CT_NON then
          //  s += #13#10;
          //
          //case rgCodeType.ItemIndex of
          //  CT_HEX: code := c.ToHexString(2);
          //  CT_BIN: code := IntToBin(c, 8);
          //  CT_DEC: code := c.ToString;
          //  CT_NON: code := '';
          //  end;

          Cells[x, y] := s;
          end;
      end;
  end;


procedure TfmASCIIChar.FormCreate(Sender: TObject);
  var
    x, y, c: Integer;
  begin
    with sgChar do
      begin
      DefaultRowHeight := 48;
      DefaultColWidth  := 64;

      with sgChar do
        begin
        for x := 0 to 7 do
          begin
          Columns.Add.Alignment := taCenter;
          end;

        RowCount := 32;
        CreateTable;
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
      end;
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
          Brush.Color := sgChar.Color - $222222 else
          Brush.Color := sgChar.Color;

      Pen.Style := psClear;
      with aRect do
        Rectangle(Left, Top, Right + 1, Bottom + 1);

      Font.Size := 12;
      with aRect do
        TextRect(Rect(Left, Top + trunc((Bottom - Top) * 0.5), Right, Bottom),
          Left, Top, code);

      if rgCodeType.ItemIndex = CT_NON then
        show_rect := aRect else
        with aRect do
          show_rect := Rect(Left, Top, Right, Top + trunc((Bottom - Top) * 0.7));

      Font.Size := 18;
      TextRect(show_rect, Left, Top, sgChar.Cells[aCol, aRow]);
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
      ts            := Canvas.TextStyle;
      ts.SingleLine := False;
      //ts.Wordbreak  := True;
      Canvas.TextStyle := ts;
      end;
  end;

procedure TfmASCIIChar.sgCharResize(Sender: TObject);
  begin
    //with sgChar do
    //  DefaultRowHeight := ColWidths[0];
  end;

procedure TfmASCIIChar.sgCharSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
  var
    c: Integer;
  begin
    c       := aCol + aRow * 8;
    Caption := FORM_ASCII_CAPTION + ' - <' + GetCharName(c) + '>';

    StatusBar.Panels.Items[0].Text := '0x' + c.ToHexString(2);
    StatusBar.Panels.Items[1].Text := c.ToString;
    StatusBar.Panels.Items[2].Text := '0b' + intToBin(c, 8);
    StatusBar.Panels.Items[3].Text := GetCharNameFull(c);
  end;


procedure TfmASCIIChar.sgCharUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    case UTF8Key of
      chr(13):
        acOK.Execute;
      chr(27):
        acCancel.Execute;
      end;
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



