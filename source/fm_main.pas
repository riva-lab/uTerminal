unit fm_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, Spin, ExtCtrls,
  ActnList, Buttons, ComCtrls, Menus, IniPropStorage, Graphics, StrUtils,
  CPort, LazUTF8, com_detect, fm_commands, fm_insertchar, fm_about, fm_graph;

resourcestring
  APPLICATION_CAPTION = 'uTerminal';
  TX_CAPTION          = 'Передача [Tx]';
  RX_CAPTION          = 'Прием [Rx]';
  ERROR               = 'Ошибка';
  CONNECTED_TO        = 'Подключен к';
  CONNECT             = 'Подключить';
  CONNECT_HINT        = 'Подключиться к выбранному порту';
  DISCONNECT_HINT     = 'Отключиться от текущего порта';
  DISCONNECTED_FROM   = 'Отключен от';
  DISCONNECT          = 'Отключить';
  SPEED               = 'бод/с';
  PORTS_FINDED        = 'Найдено портов';
  BYTE1               = 'байт';
  BYTE2               = 'байта';
  BYTE5               = 'байт';


type
  TComLinesStatus = record
    RTS:   Boolean;
    DTR:   Boolean;
    Break: Boolean;
  end;

  { TfmMain }

  TfmMain = class(TForm)
    acConnect:              TAction;
    acScan:                 TAction;
    acAutoSend:             TAction;
    acRxClear:              TAction;
    acConnectionHeader:     TAction;
    acExit:                 TAction;
    acShowSettings:         TAction;
    acShowTxBox:            TAction;
    acShowRxBox:            TAction;
    acFileHeader:           TAction;
    acHelpHeader:           TAction;
    acAbout:                TAction;
    acAutoAnswerHeader:     TAction;
    acAutoAnswerSetup:      TAction;
    acAutoAnswerEnable:     TAction;
    acInsertChar:           TAction;
    acShowSignals:          TAction;
    acShowHEX:              TAction;
    acRxExport:             TAction;
    acShowGraph:            TAction;
    acRxHeader:             TAction;
    acFontTx:               TAction;
    acFontRx:               TAction;
    acTxHeader:             TAction;
    acTxImport:             TAction;
    acToggleRTS:            TAction;
    acToggleDTR:            TAction;
    acToggleBreak:          TAction;
    acViewHeader:           TAction;
    acTxClear:              TAction;
    acTx:                   TAction;
    ActionList1:            TActionList;
    ApplicationProperties1: TApplicationProperties;
    bbScan:                 TBitBtn;
    bbConnect:              TBitBtn;
    cbBaudrateX:            TComboBox;
    cbDataBitsX:            TComboBox;
    cbParityBitsX:          TComboBox;
    cbPortsList:            TComboBox;
    cbParityBits:           TComboBox;
    cbPortsListX:           TComboBox;
    cbStopBitsX:            TComboBox;
    cbTxType:               TComboBox;
    cbRxType:               TComboBox;
    cbBaudrate:             TComboBox;
    cbDataBits:             TComboBox;
    cbStopBits:             TComboBox;
    ComDataPacket1:         TComDataPacket;
    ComPort1:               TComPort;
    FontDialog1:            TFontDialog;
    gbTx:                   TGroupBox;
    gbRx:                   TGroupBox;
    gbConnection:           TGroupBox;
    gbSignals:              TGroupBox;
    ImageList1:             TImageList;
    IniPropStorage1:        TIniPropStorage;
    Label1:                 TLabel;
    Label2:                 TLabel;
    Label3:                 TLabel;
    Label4:                 TLabel;
    Label5:                 TLabel;
    Label7:                 TLabel;
    Label8:                 TLabel;
    Label9:                 TLabel;
    MainMenu1:              TMainMenu;
    MenuItem1:              TMenuItem;
    MenuItem10:             TMenuItem;
    MenuItem11:             TMenuItem;
    MenuItem12:             TMenuItem;
    MenuItem13:             TMenuItem;
    MenuItem14:             TMenuItem;
    MenuItem15:             TMenuItem;
    MenuItem16:             TMenuItem;
    MenuItem17:             TMenuItem;
    MenuItem18:             TMenuItem;
    MenuItem19:             TMenuItem;
    MenuItem2:              TMenuItem;
    MenuItem20:             TMenuItem;
    MenuItem21:             TMenuItem;
    MenuItem22:             TMenuItem;
    MenuItem23:             TMenuItem;
    MenuItem24:             TMenuItem;
    MenuItem25:             TMenuItem;
    MenuItem26:             TMenuItem;
    MenuItem27:             TMenuItem;
    MenuItem28:             TMenuItem;
    MenuItem29:             TMenuItem;
    MenuItem3:              TMenuItem;
    MenuItem30:             TMenuItem;
    MenuItem31:             TMenuItem;
    MenuItem32:             TMenuItem;
    MenuItem33:             TMenuItem;
    MenuItem4:              TMenuItem;
    MenuItem5:              TMenuItem;
    MenuItem6:              TMenuItem;
    MenuItem7:              TMenuItem;
    MenuItem8:              TMenuItem;
    MenuItem9:              TMenuItem;
    mRx:                    TMemo;
    mTx:                    TMemo;
    mTxHex:                 TMemo;
    mRxHex:                 TMemo;
    pPortSettShort:         TPanel;
    Panel4:                 TPanel;
    Panel5:                 TPanel;
    pmPort:                 TPopupMenu;
    seBaudRateCustomX:      TSpinEdit;
    ToolBarRx:              TToolBar;
    ToolBarTx:              TToolBar;
    ToolButton18:           TToolButton;
    ToolButton19:           TToolButton;
    ToolButton20:           TToolButton;
    ToolButton21:           TToolButton;
    ToolButton22:           TToolButton;
    ToolButton23:           TToolButton;
    ToolButton24:           TToolButton;
    ToolButton25:           TToolButton;
    ToolButton26:           TToolButton;
    ToolButton27:           TToolButton;
    ToolButton28:           TToolButton;
    ToolButton29:           TToolButton;
    ToolButton30:           TToolButton;
    ToolButton31:           TToolButton;
    TxOpenDialog:           TOpenDialog;
    Panel3:                 TPanel;
    Panel2:                 TPanel;
    pRTS:                   TPanel;
    pDTR:                   TPanel;
    pBreak:                 TPanel;
    pDSR:                   TPanel;
    pRing:                  TPanel;
    pRLSD:                  TPanel;
    pTranceiver:            TPanel;
    pCTS:                   TPanel;
    RxSaveDialog:           TSaveDialog;
    seBaudRateCustom:       TSpinEdit;
    seAutoSendTime:         TSpinEdit;
    sbRTS:                  TSpeedButton;
    sbDTR:                  TSpeedButton;
    sbBreak:                TSpeedButton;
    SplitterTx:             TSplitter;
    SplitterRx:             TSplitter;
    StatusBar1:             TStatusBar;
    TimerAutoSending:       TTimer;
    ToolBar1:               TToolBar;
    ToolButton1:            TToolButton;
    ToolButton14:           TToolButton;
    ToolButton2:            TToolButton;
    ToolButton3:            TToolButton;
    ToolButton4:            TToolButton;
    ToolButton5:            TToolButton;
    ToolButton6:            TToolButton;
    ToolButton7:            TToolButton;
    ToolButton9:            TToolButton;
    procedure acAboutExecute(Sender: TObject);
    procedure acAutoAnswerSetupExecute(Sender: TObject);
    procedure acAutoSendExecute(Sender: TObject);
    procedure acConnectExecute(Sender: TObject);
    procedure acFontChangeExecute(Sender: TObject);
    procedure acInsertCharExecute(Sender: TObject);
    procedure acRxExportExecute(Sender: TObject);
    procedure acShowGraphExecute(Sender: TObject);
    procedure acShowHEXExecute(Sender: TObject);
    procedure acShowRxBoxExecute(Sender: TObject);
    procedure acShowSettingsExecute(Sender: TObject);
    procedure acExitExecute(Sender: TObject);
    procedure acRxClearExecute(Sender: TObject);
    procedure acScanExecute(Sender: TObject);
    procedure acShowSignalsExecute(Sender: TObject);
    procedure acShowTxBoxExecute(Sender: TObject);
    procedure acToggleBreakExecute(Sender: TObject);
    procedure acToggleDTRExecute(Sender: TObject);
    procedure acToggleRTSExecute(Sender: TObject);
    procedure acTxClearExecute(Sender: TObject);
    procedure acTxExecute(Sender: TObject);
    procedure acTxImportExecute(Sender: TObject);
    procedure cbPortsListChange(Sender: TObject);
    procedure cbRxTypeChange(Sender: TObject);
    procedure cbTxTypeChange(Sender: TObject);
    procedure ComPort1Signals(Sender: TObject);
    procedure ComPort1SignalsChange(Sender: TObject; OnOff: Boolean);
    procedure ComPort1RxChar(Sender: TObject; Count: Integer);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure mRxChange(Sender: TObject);
    procedure mTxChange(Sender: TObject);
    procedure TimerAutoSendingTimer(Sender: TObject);

    procedure SetPortSettingsControls(Sender: TObject);
  PRIVATE
    procedure RefreshControls;
    function GetComSignals(): Integer;
    procedure DisableComSignals;
  PUBLIC
  end;

  { TSendingThread }

  TSendingThread = class(TThread)
  PRIVATE
    FExecuting: Boolean;
    FData:      String;

    procedure SetData(AValue: String);
  PROTECTED
    procedure Execute; OVERRIDE;
  PUBLIC
    constructor Create(CreateSuspended: Boolean);

    property Executing: Boolean read FExecuting;
    property Data: String read FData write SetData;
  end;

const
  TT_ASC = 0; // ASCII
  TT_HEX = 1;
  TT_BIN = 2;
  TT_DEC = 3;

  indicator: array[0..2] of TColor = ($4040FF, $00D000, $CCCCCC);
// 0 RED; 1 GREEN; inactive

var
  fmMain: TfmMain;
  tx, rx: String;

implementation

var
  // error messages
  xComErrorMessages:         array[1..23] of String;
  br_last, db_last, sb_last: Integer;
  status:                    TComLinesStatus;
  send_thread:               TSendingThread;

{$R *.lfm}

{ вспомогоательные функции }

function StringToHex(AText: String): String;
  var
    i, len: Integer;
  begin
    AText  := UTF8ToWinCP(AText);
    Result := '';
    len    := AText.Length;
    for i := 1 to len do
      begin
      Result += Ord(AText[i]).ToHexString(2);

      if i mod 8 = 0 then
        Result += #13#10
      else
        if i < len then
          begin
          Result += ' ';
          if i mod 4 = 0 then
            Result += ' ';
          end;
      end;
  end;

function SizeStr(AText: String): String;
  var
    size: Integer;
  begin
    size   := UTF8ToWinCP(AText).Length;
    Result := size.ToString + ' (';

    case size mod 10 of
      1:
        Result += BYTE1;
      2..4:
        Result += BYTE2
      else
        Result += BYTE5;
      end;

    Result += ')';
  end;

function CodeStrToText(AData: String; ASysIn, ASysOut: Integer): String;
  var
    i, l, ch_out: Integer;
    ch:           Char;
    prefix, tmp:  String;
    symbols:      set of Char;
    delimiters:   set of Char = [' ', ',', ';', '-', chr(13), chr(10)];
  begin
    case ASysIn of
      2:
        begin
        prefix  := '%';
        symbols := ['0'..'1'];
        end;

      10:
        begin
        prefix  := '';
        symbols := ['0'..'9'];
        end;

      16:
        begin
        prefix  := '$';
        symbols := ['0'..'9', 'A'..'F'];
        end;
      end;

    AData  := UpperCase(AData);
    l      := AData.Length;
    Result := '';
    tmp    := '';

    if l > 0 then
      begin
      for i := 1 to l do
        begin
        ch := AData[i];

        if ch in symbols then
          tmp += ch;

        if (ch in delimiters) or (i = l) then
            try
            ch_out := (prefix + tmp).ToInteger;
            tmp    := '';

            case ASysOut of
              0: Result  += chr(ch_out);
              2: Result  += intToBin(ch_out, 8) + ' ';
              10: Result += ch_out.ToString + ' ';
              16: Result += ch_out.ToHexString(2) + ' ';
              end;
            except
            end;
        end;

      if (Result.Length > 0) and (ASysOut > 0) then
        while Result[Result.Length] = ' ' do
          Delete(Result, Result.Length, 1);
      end;

    Result := WinCPToUTF8(Result);
  end;


{ TSendingThread }

procedure TSendingThread.SetData(AValue: String);
  begin
    AValue := UTF8ToWinCP(AValue);
    if FData = AValue then Exit;
    FData := AValue;
  end;

procedure TSendingThread.Execute;
  begin
    with fmMain do
        try
        FExecuting   := True;
        acTx.Enabled := False;

        if ComPort1.Connected then
          ComPort1.WriteStr(FData);
        finally
        FExecuting   := False;
        acTx.Enabled := True;
        FData        := '';
        end;
  end;

constructor TSendingThread.Create(CreateSuspended: Boolean);
  begin
    FreeOnTerminate := True;
    FExecuting      := False;
    inherited Create(CreateSuspended);
  end;


{ TfmMain }

function TfmMain.GetComSignals: Integer;
  var
    s:         TComSignals;
    i:         Integer;
    signal:    array[1..4] of Integer;
    connected: Boolean;
  begin
    // http://www.aggsoft.ru/rs232-pinout-cable/pinout-and-signal.htm
    connected       := ComPort1.Connected;
    sbRTS.Enabled   := connected;
    sbDTR.Enabled   := connected;
    sbBreak.Enabled := connected;

    for i := Low(signal) to High(signal) do
      signal[i] := 2;

    if connected then
        try
        s            := ComPort1.Signals;
        signal[1]    := (csCTS in s).ToInteger;  // Clear to Send
        signal[2]    := (csDSR in s).ToInteger;  // Data Set Ready
        signal[3]    := (csRing in s).ToInteger; // Ring Indicator
        signal[4]    := (csRLSD in s).ToInteger; // Receive Line Signal Detect
        pRTS.Color   := indicator[status.RTS.ToInteger];   // Request to Send
        pDTR.Color   := indicator[status.DTR.ToInteger];   // Data Terminal Ready
        pBreak.Color := indicator[status.Break.ToInteger]; // Break
        except
        end
    else
      begin
      pRTS.Color   := indicator[2];
      pDTR.Color   := indicator[2];
      pBreak.Color := indicator[2];
      end;

    pCTS.Color  := indicator[signal[1]];
    pDSR.Color  := indicator[signal[2]];
    pRing.Color := indicator[signal[3]];
    pRLSD.Color := indicator[signal[4]];
  end;

procedure TfmMain.ComPort1Signals(Sender: TObject);
  begin
    GetComSignals;
  end;

procedure TfmMain.ComPort1SignalsChange(Sender: TObject; OnOff: Boolean);
  begin
    GetComSignals;
  end;

procedure TfmMain.ComPort1RxChar(Sender: TObject; Count: Integer);
  var
    s:     String;
    index: Integer;
  begin
    ComPort1.ReadStr(s, Count);
    rx += WinCPToUTF8(s);

    if acAutoAnswerEnable.Checked then
      begin
      index := fmCommands.SequenceList.IndexOf(s);
      if index >= 0 then
        ComPort1.WriteStr(fmCommands.AnswerList.Strings[index]);
      end;

    mRxChange(Sender);
  end;


procedure TfmMain.FormCreate(Sender: TObject);
  begin
    acConnect.Hint := CONNECT_HINT;
    send_thread    := TSendingThread.Create(True);
    RefreshControls;
  end;

procedure TfmMain.FormShow(Sender: TObject);
  begin
    //AutoSize := True;
    //AutoSize := False;
    //with Constraints do
    //  begin
    //  MinHeight := Height;
    //  MinWidth  := Width;
    //  end;

    ToolBarTx.ButtonHeight := cbTxType.Height;
    ToolBarRx.ButtonHeight := cbRxType.Height;
    bbScan.Caption         := '';
    bbConnect.Caption      := '';
    fmGraph.MainForm       := fmMain;
    mTxHex.Font            := mTx.Font;
    mRxHex.Font            := mRx.Font;

    acScan.Execute;
    acRxClear.Execute;
    acTxClear.Execute;
    acShowRxBoxExecute(nil);
    acShowTxBoxExecute(nil);
    acShowSettingsExecute(nil);
    acShowSignalsExecute(nil);
    acShowHEXExecute(nil);
    SetPortSettingsControls(nil);
    FormChangeBounds(nil);
    mTxChange(nil);
    mRxChange(nil);

    fmAbout.ShowSplash(False);
  end;

procedure TfmMain.FormChangeBounds(Sender: TObject);
  begin
    fmGraph.UpdateFormPosition;
    StatusBar1.Panels.Items[0].Width := Width - 250;
  end;

procedure TfmMain.TimerAutoSendingTimer(Sender: TObject);
  begin
    if acTx.Enabled then
      acTx.Execute;
  end;


// ввод/вывод данных

procedure TfmMain.mTxChange(Sender: TObject);
  begin
    case cbTxType.ItemIndex of
      TT_ASC: tx := mTx.Text;
      TT_HEX: tx := CodeStrToText(mTx.Text, 16, 0);
      TT_BIN: tx := CodeStrToText(mTx.Text, 2, 0);
      TT_DEC: tx := CodeStrToText(mTx.Text, 10, 0);
      end;

    mTxHex.Text     := StringToHex(tx);
    mTxHex.SelStart := Length(mTxHex.Text);
    gbTx.Caption    := TX_CAPTION + ' - ' + SizeStr(tx);
  end;

procedure TfmMain.mRxChange(Sender: TObject);
  begin
    mRxHex.Text := StringToHex(rx);
    cbRxTypeChange(Sender);

    mRx.SelStart      := Length(mRx.Text);
    mRxHex.SelStart   := Length(mRxHex.Text);
    gbRx.Caption      := RX_CAPTION + ' - ' + SizeStr(rx);
    fmGraph.GraphData := rx;
  end;

procedure TfmMain.cbTxTypeChange(Sender: TObject);
  begin
    case cbTxType.ItemIndex of
      TT_ASC: mTx.Text := CodeStrToText(mTxHex.Text, 16, 0).Replace(#0, #1);
      TT_HEX: mTx.Text := CodeStrToText(mTxHex.Text, 16, 16);
      TT_BIN: mTx.Text := CodeStrToText(mTxHex.Text, 16, 2);
      TT_DEC: mTx.Text := CodeStrToText(mTxHex.Text, 16, 10);
      end;
  end;

procedure TfmMain.cbRxTypeChange(Sender: TObject);
  begin
    case cbRxType.ItemIndex of
      TT_ASC: mRx.Text := CodeStrToText(mRxHex.Text, 16, 0).Replace(#0, #1);
      TT_HEX: mRx.Text := CodeStrToText(mRxHex.Text, 16, 16);
      TT_BIN: mRx.Text := CodeStrToText(mRxHex.Text, 16, 2);
      TT_DEC: mRx.Text := CodeStrToText(mRxHex.Text, 16, 10);
      end;
  end;


// настройки трансивера UART

procedure TfmMain.cbPortsListChange(Sender: TObject);
  begin
    if ComPort1.Connected then
      begin
      ComPort1.Close;
      StatusBar1.Panels.Items[1].Text := DISCONNECTED_FROM + ' ' + ComPort1.Port;
      end;

    RefreshControls;
  end;

procedure TfmMain.SetPortSettingsControls(Sender: TObject);
  var
    tmp_cb: TComboBox;
    tmp_se: TSpinEdit;

  procedure SetBaudRate(index, custom: Integer);
    begin
      if index = 15 then
        begin
        ComPort1.BaudRate         := brCustom;
        ComPort1.CustomBaudRate   := custom;
        seBaudRateCustom.Enabled  := True;
        seBaudRateCustomX.Visible := True;
        end
      else
        begin
        ComPort1.BaudRate         := TBaudRate(index + 1);
        seBaudRateCustom.Enabled  := False;
        seBaudRateCustomX.Visible := False;
        end;
    end;

  procedure SetDataBits(index: Integer);
    begin
      ComPort1.DataBits := TDataBits(index);
    end;

  procedure SetStopBits(index: Integer);
    begin
      ComPort1.StopBits := TStopBits(index);
    end;

  procedure SetParityBits(index: Integer);
    begin
      ComPort1.Parity.Bits  := TParityBits(index);
      ComPort1.Parity.Check := False;
    end;

  begin
    if pPortSettShort.Visible then
      cbPortsList.ItemIndex  := cbPortsListX.ItemIndex else
      cbPortsListX.ItemIndex := cbPortsList.ItemIndex;

    if pPortSettShort.Visible then
      begin
      SetParityBits(cbParityBitsX.ItemIndex);
      cbParityBits.ItemIndex := cbParityBitsX.ItemIndex;
      end else
      begin
      SetParityBits(cbParityBits.ItemIndex);
      cbParityBitsX.ItemIndex := cbParityBits.ItemIndex;
      end;


    if pPortSettShort.Visible then
      tmp_cb := cbDataBitsX else
      tmp_cb := cbDataBits;

      try
      SetDataBits(tmp_cb.ItemIndex);
      db_last := tmp_cb.ItemIndex;
      except
      tmp_cb.ItemIndex := db_last;
      SetDataBits(tmp_cb.ItemIndex);
      end;

    if pPortSettShort.Visible then
      cbDataBits.ItemIndex  := cbDataBitsX.ItemIndex else
      cbDataBitsX.ItemIndex := cbDataBits.ItemIndex;


    if pPortSettShort.Visible then
      tmp_cb := cbStopBitsX else
      tmp_cb := cbStopBits;

      try
      SetStopBits(tmp_cb.ItemIndex);
      sb_last := tmp_cb.ItemIndex;
      except
      tmp_cb.ItemIndex := sb_last;
      SetStopBits(tmp_cb.ItemIndex);
      end;

    if pPortSettShort.Visible then
      cbStopBits.ItemIndex  := cbStopBitsX.ItemIndex else
      cbStopBitsX.ItemIndex := cbStopBits.ItemIndex;


    if pPortSettShort.Visible then
      begin
      tmp_cb := cbBaudrateX;
      tmp_se := seBaudRateCustomX;
      end else
      begin
      tmp_cb := cbBaudRate;
      tmp_se := seBaudRateCustom;
      end;

      try
      SetBaudRate(tmp_cb.ItemIndex, tmp_se.Value);
      br_last := tmp_cb.ItemIndex;
      except
      tmp_cb.ItemIndex := br_last;
      SetBaudRate(tmp_cb.ItemIndex, tmp_se.Value);
      end;

    if pPortSettShort.Visible then
      begin
      cbBaudrate.ItemIndex   := cbBaudrateX.ItemIndex;
      seBaudRateCustom.Value := seBaudRateCustomX.Value;
      end else
      begin
      cbBaudrateX.ItemIndex   := cbBaudrate.ItemIndex;
      seBaudRateCustomX.Value := seBaudRateCustom.Value;
      end;

    RefreshControls;
  end;


// команды

procedure TfmMain.acExitExecute(Sender: TObject);
  begin
    Close;
  end;

procedure TfmMain.acAboutExecute(Sender: TObject);
  begin
    fmAbout.Show;
  end;

procedure TfmMain.acScanExecute(Sender: TObject);
  var
    com_txt: String;
  begin
    if pPortSettShort.Visible then
      com_txt := cbPortsListX.Text else
      com_txt := cbPortsList.Text;

    ComPort1.Close;
    DetectComPorts(cbPortsList.Items);
    cbPortsListX.Items.Assign(cbPortsList.Items);
    RefreshControls;

    cbPortsList.Text := com_txt;
    if cbPortsList.ItemIndex < 0 then
      cbPortsList.ItemIndex := 0;

    cbPortsListX.Text := com_txt;
    if cbPortsListX.ItemIndex < 0 then
      cbPortsListX.ItemIndex := 0;

    StatusBar1.Panels.Items[1].Text :=
      PORTS_FINDED + ': ' + cbPortsList.Items.Count.ToString;
  end;

procedure TfmMain.acConnectExecute(Sender: TObject);
  var
    connected: Boolean;
  begin
    connected := ComPort1.Connected;

    if not connected then
      begin
        try
        SetPortSettingsControls(nil);
        if pPortSettShort.Visible then
          ComPort1.Port := cbPortsListX.Text else
          ComPort1.Port := cbPortsList.Text;
        ComPort1.Open;
        send_thread := TSendingThread.Create(True);
        except
        on e: EComPort do
          ShowMessage(xComErrorMessages[e.Code] {+ #13#10 + 'Code=' +
            e.Code.ToString + '; WinCode=' + e.WinCode.ToString});
        end;

      if not ComPort1.Connected then
        StatusBar1.Panels.Items[1].Text := ERROR;
      end;

    DisableComSignals;

    if connected then
      begin
      ComPort1.Close;
      StatusBar1.Panels.Items[1].Text := DISCONNECTED_FROM + ' ' + ComPort1.Port;
      acConnect.Hint                  := CONNECT_HINT;
      end
    else
      acConnect.Hint := DISCONNECT_HINT;

    RefreshControls;
  end;

procedure TfmMain.acTxExecute(Sender: TObject);
  begin
    send_thread      := TSendingThread.Create(True);
    send_thread.Data := tx;
    send_thread.Start;
  end;

procedure TfmMain.acAutoSendExecute(Sender: TObject);
  begin
    TimerAutoSending.Interval := seAutoSendTime.Value;
    TimerAutoSending.Enabled  := acAutoSend.Checked;
  end;

procedure TfmMain.acAutoAnswerSetupExecute(Sender: TObject);
  begin
    fmCommands.ShowModal;
  end;

procedure TfmMain.acInsertCharExecute(Sender: TObject);
  var
    inserted_char, tmp:       String;
    select_start, txt_length: Integer;
  begin
    with fmASCIIChar do
      if ShowModal = mrOk then
        begin
        case cbTxType.ItemIndex of
          TT_ASC: inserted_char := WinCPToUTF8(chr(SelectedChar));
          TT_HEX: inserted_char := SelectedChar.ToHexString(2);
          TT_BIN: inserted_char := intToBin(SelectedChar, 8);
          TT_DEC: inserted_char := SelectedChar.ToString;
          end;

        select_start := mTx.SelStart;
        tmp          := mTx.Text;
        txt_length   := tmp.Length;

        if cbTxType.ItemIndex <> TT_ASC then
          begin
          if (select_start <> 0) and (select_start <= txt_length) and
            (tmp[select_start] <> ' ') then
            inserted_char := ' ' + inserted_char;

          if (select_start < txt_length) and (tmp[select_start + 1] <> ' ') then
            inserted_char := inserted_char + ' ';
          end;

        mTx.Text     := tmp.Insert(select_start, inserted_char);
        mTx.SelStart := select_start + inserted_char.Length;
        mTx.SetFocus;
        end;
  end;

procedure TfmMain.acRxExportExecute(Sender: TObject);
  var
    tmp: String;
  begin
    tmp := RxSaveDialog.FileName;

    if RxSaveDialog.Execute then
      begin
      mRx.Lines.SaveToFile(RxSaveDialog.FileName);
      RxSaveDialog.FileName := ExtractFileName(RxSaveDialog.FileName);
      end
    else
      RxSaveDialog.FileName := tmp;
  end;

procedure TfmMain.acTxImportExecute(Sender: TObject);
  begin
    TxOpenDialog.FileName := ExtractFileName(TxOpenDialog.FileName);
    if TxOpenDialog.Execute then
      mTx.Lines.LoadFromFile(TxOpenDialog.FileName);
  end;

procedure TfmMain.acShowGraphExecute(Sender: TObject);
  begin
    fmGraph.Show;
  end;

procedure TfmMain.acFontChangeExecute(Sender: TObject);
  begin
    with FontDialog1.Font do
      case TAction(Sender).Tag of
        1: Assign(mTx.Font);
        2: Assign(mRx.Font);
        end;

    with FontDialog1 do
      if Execute then
        begin
        case TAction(Sender).Tag of
          1: mTx.Font.Assign(Font);
          2: mRx.Font.Assign(Font);
          end;

        mTxHex.Font := mTx.Font;
        mRxHex.Font := mRx.Font;
        end;
  end;

// Вид

procedure TfmMain.acTxClearExecute(Sender: TObject);
  begin
    tx := '';
    mTx.Clear;
  end;

procedure TfmMain.acRxClearExecute(Sender: TObject);
  begin
    rx := '';
    mRx.Clear;
  end;

procedure TfmMain.acShowRxBoxExecute(Sender: TObject);
  begin
    gbRx.Visible := acShowRxBox.Checked;
  end;

procedure TfmMain.acShowTxBoxExecute(Sender: TObject);
  begin
    gbTx.Visible := acShowTxBox.Checked;
  end;

procedure TfmMain.acShowSettingsExecute(Sender: TObject);
  begin
    gbConnection.Visible   := acShowSettings.Checked;
    ToolButton1.Visible    := not gbConnection.Visible;
    ToolButton2.Visible    := not gbConnection.Visible;
    ToolButton4.Visible    := not gbConnection.Visible;
    pPortSettShort.Visible := not gbConnection.Visible;

    if pPortSettShort.Visible then
      begin
      cbPortsListX.ItemIndex  := cbPortsList.ItemIndex;
      cbStopBitsX.ItemIndex   := cbStopBits.ItemIndex;
      cbDataBitsX.ItemIndex   := cbDataBits.ItemIndex;
      cbParityBitsX.ItemIndex := cbParityBits.ItemIndex;
      cbBaudrateX.ItemIndex   := cbBaudrate.ItemIndex;
      seBaudRateCustomX.Value := seBaudRateCustom.Value;
      end
    else
      begin
      cbPortsList.ItemIndex  := cbPortsListX.ItemIndex;
      cbStopBits.ItemIndex   := cbStopBitsX.ItemIndex;
      cbDataBits.ItemIndex   := cbDataBitsX.ItemIndex;
      cbParityBits.ItemIndex := cbParityBitsX.ItemIndex;
      cbBaudrate.ItemIndex   := cbBaudrateX.ItemIndex;
      seBaudRateCustom.Value := seBaudRateCustomX.Value;
      end;
  end;

procedure TfmMain.acShowSignalsExecute(Sender: TObject);
  begin
    gbSignals.Visible := acShowSignals.Checked;
    //pSignals.Visible  := acShowSignals.Checked;
  end;

procedure TfmMain.acShowHEXExecute(Sender: TObject);
  begin
    mTxHex.Visible     := acShowHEX.Checked;
    SplitterTx.Visible := acShowHEX.Checked;
    mRxHex.Visible     := acShowHEX.Checked;
    SplitterRx.Visible := acShowHEX.Checked;

    mTxHex.Visible     := False;
    SplitterTx.Visible := False;
    mRxHex.Visible     := False;
    SplitterRx.Visible := False;

    mTxHex.Visible     := acShowHEX.Checked;
    SplitterTx.Visible := acShowHEX.Checked;
    mRxHex.Visible     := acShowHEX.Checked;
    SplitterRx.Visible := acShowHEX.Checked;
  end;


// Сигналы RS-232

procedure TfmMain.DisableComSignals;
  begin
    status.RTS   := True;
    status.DTR   := True;
    status.Break := True;
    acToggleRTSExecute(nil);
    acToggleDTRExecute(nil);
    acToggleBreakExecute(nil);
  end;

procedure TfmMain.acToggleRTSExecute(Sender: TObject);
  begin
      try
      ComPort1.SetRTS(not status.RTS);
      status.RTS := not status.RTS;
      GetComSignals;
      except
      end;
  end;

procedure TfmMain.acToggleDTRExecute(Sender: TObject);
  begin
      try
      ComPort1.SetDTR(not status.DTR);
      status.DTR := not status.DTR;
      GetComSignals;
      except
      end;
  end;

procedure TfmMain.acToggleBreakExecute(Sender: TObject);
  begin
      try
      ComPort1.SetBreak(not status.Break);
      status.Break := not status.Break;
      GetComSignals;
      except
      end;
  end;


procedure TfmMain.RefreshControls;
  var
    tmp, port: String;
    connected: Boolean;
  begin
    connected    := ComPort1.Connected;
    acTx.Enabled := connected and not send_thread.Executing;

    if connected then
      acConnect.Caption := DISCONNECT  else
      acConnect.Caption := CONNECT;

    tmp  := APPLICATION_CAPTION;
    port := '';
    if connected then
      begin
      tmp  += ' [' + ComPort1.Port + ':  ';
      port += DataBitsToStr(ComPort1.DataBits) + '-';
      port += ParityToStr(ComPort1.Parity.Bits)[1] + '-';
      port += StopBitsToStr(ComPort1.StopBits) + '  ';

      if ComPort1.BaudRate = brCustom then
        port += ComPort1.CustomBaudRate.ToString  else
        port += BaudRateToStr(ComPort1.BaudRate);
      port += ' ' + SPEED;
      tmp += port + ']';

      StatusBar1.Panels.Items[1].Text :=
        CONNECTED_TO + ' ' + ComPort1.Port + #13#10 + port;

      acConnect.ImageIndex := 26;
      ImageList1.GetBitmap(26, bbConnect.Glyph);
      end
    else
      begin
      acConnect.ImageIndex := 2;
      ImageList1.GetBitmap(2, bbConnect.Glyph);
      end;

    Caption           := tmp;
    Application.Title := tmp;
    GetComSignals;
  end;


initialization
  br_last := 6;
  db_last := 3;
  sb_last := 0;

  xComErrorMessages[1]  := ('Невозможно открыть COM-порт');
  xComErrorMessages[2]  := ('Сбой функции WriteFile');
  xComErrorMessages[3]  := ('Сбой функции ReadFile');
  xComErrorMessages[4]  := ('Неправильный параметр Async');
  xComErrorMessages[5]  := ('Сбой функции PurgeComm');
  xComErrorMessages[6]  := ('Невозможно получить статус async');
  xComErrorMessages[7]  := ('Сбой функции SetCommState');
  xComErrorMessages[8]  := ('Сбой функции SetCommTimeouts');
  xComErrorMessages[9]  := ('Сбой функции SetupComm');
  xComErrorMessages[10] := ('Сбой функции ClearCommError');
  xComErrorMessages[11] := ('Сбой функции GetCommModemStatus');
  xComErrorMessages[12] := ('Сбой функции EscapeCommFunction');
  xComErrorMessages[13] := ('Сбой функции TransmitCommChar');
  xComErrorMessages[14] := ('Невозиожно установить свойство при активном соединении');
  xComErrorMessages[15] := ('Сбой функции EnumPorts');
  xComErrorMessages[16] := ('Сбой сохранения настроек');
  xComErrorMessages[17] := ('Сбой загрузки настроек');
  xComErrorMessages[18] := ('-Link (un)registration failed');
  xComErrorMessages[19] := ('-Cannot change led state if ComPort is selected');
  xComErrorMessages[20] := ('-Cannot wait for event if event thread is created');
  xComErrorMessages[21] := ('Сбой метода WaitForEvent');
  xComErrorMessages[22] := ('-A component is linked to OnRxBuf event');
  xComErrorMessages[23] := ('Ошибка реестра');
end.
