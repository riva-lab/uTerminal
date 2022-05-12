unit fm_main;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Buttons, Classes, Clipbrd, ComCtrls, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, IniPropStorage, LazFileUtils, LazUTF8,
  LCLType, Menus, PairSplitter, Spin, StdCtrls, LCLIntf, Grids, StrUtils,
  SynEdit, SynEditTypes,
  SysUtils, Types, fm_about,
  fm_commands, fm_confirm, fm_chart, fm_insertchar, fm_settings, u_encodings,
  u_serial, u_strings, u_utilities, u_txsequences, base64;

resourcestring
  TX_CAPTION        = 'Передача [Tx]';
  RX_CAPTION        = 'Прием [Rx]';
  ERROR             = 'Ошибка';
  CONNECTED_TO      = 'Подключен к';
  CONNECT           = 'Подключить';
  CONNECT_HINT      = 'Подключиться к выбранному порту';
  DISCONNECT_HINT   = 'Отключиться от текущего порта';
  DISCONNECTED_FROM = 'Отключен от';
  DISCONNECT        = 'Отключить';
  SPEED             = 'бод/с';
  PORTS_FINDED      = 'Найдено портов';
  TEXT_TYPE_TEXT    = 'Текст';
  TXT_ENCODING      = 'кодировка';

const
  HELP_DIR        = 'help';
  HELP_DIR_ONLINE = '-/blob/master/help';
  HELP_FILE       = 'uTerminal-help';

  TT_ASC = 0; // отображение данных в ASCII
  TT_HEX = 1; // отображение данных в HEX
  TT_BIN = 2; // отображение данных в BIN
  TT_DEC = 3; // отображение данных в DEC

  indicatorColor: array[Boolean] of TColor = ($4040FF, $00D000); // 0 RED; 1 GREEN
  indicatorText: array[Boolean] of String = ('0', '1');          // 0; 1
  inactiveColor = $CCCCCC;

type

  { TfmMain }

  TfmMain = class(TForm)
    acConnect:           TAction;
    acScan:              TAction;
    acAutoSend:          TAction;
    acRxClear:           TAction;
    acConnectionHeader:  TAction;
    acExit:              TAction;
    acShowTxBox:         TAction;
    acShowRxBox:         TAction;
    acFileHeader:        TAction;
    acHelpHeader:        TAction;
    acInfo:              TAction;
    acAutoAnswerHeader:  TAction;
    acAutoAnswerSetup:   TAction;
    acAutoAnswerEnable:  TAction;
    acInsertChar:        TAction;
    acShowSignals:       TAction;
    acShowHEX:           TAction;
    acRxExport:          TAction;
    acShowGraph:         TAction;
    acRxHeader:          TAction;
    acRxScrollToEnd:     TAction;
    acShowLineCounts:    TAction;
    acShowOnTop:         TAction;
    acSettings:          TAction;
    acReset:             TAction;
    acRxEnable:          TAction;
    acRxCopy:            TAction;
    acSearchNext:        TAction;
    acSearch:            TAction;
    acHelp:              TAction;
    acHelpMD:            TAction;
    acHelpNet:           TAction;
    acSearchPrev:        TAction;
    acTxSeqMoveDown:     TAction;
    acTxSeqMoveUp:       TAction;
    acTxSequencesHeader: TAction;
    acTxSeqGet:          TAction;
    acTxSeqEdit:         TAction;
    acTxSeqRemove:       TAction;
    acTxSeqSend:         TAction;
    acTxSeqAdd:          TAction;
    acTxSequences:       TAction;
    acWebsite:           TAction;
    acTxCommandMode:     TAction;
    acTxHeader:          TAction;
    acTxImport:          TAction;
    acToggleRTS:         TAction;
    acToggleDTR:         TAction;
    acToggleBreak:       TAction;
    acViewHeader:        TAction;
    acTxClear:           TAction;
    acTx:                TAction;
    alActionList:        TActionList;
    AppProperties:       TApplicationProperties;
    cbBaudrate:          TComboBox;
    cbDataBits:          TComboBox;
    cbParityBits:        TComboBox;
    cbPortsList:         TComboBox;
    cbTxEncoding:        TComboBox;
    cbStopBits:          TComboBox;
    cbTxType:            TComboBox;
    cbRxType:            TComboBox;
    cbRxEncoding:        TComboBox;
    cbSearchCaseSens:    TCheckBox;
    cbSearchReplace:     TCheckBox;
    cbSearchReplaceAll:  TCheckBox;
    edSearch:            TEdit;
    edReplace:           TEdit;
    gbTx:                TGroupBox;
    gbRx:                TGroupBox;
    gbSignals:           TGroupBox;
    imImages16:          TImageList;
    IniStorageMain:      TIniPropStorage;
    lbSearch2:           TLabel;
    lbSignalCTS:         TLabel;
    lbSignalDSR:         TLabel;
    lbSignalRing:        TLabel;
    lbSignalRLSD:        TLabel;
    lbSearch:            TLabel;
    lbSearch1:           TLabel;
    lbHelpHint:          TLabel;
    lbRxPosAndSel:       TLabel;
    lbTxSize:            TLabel;
    lbRxSize:            TLabel;
    lbTxPosAndSel:       TLabel;
    MenuItem12:          TMenuItem;
    MenuItem13:          TMenuItem;
    MenuItem16:          TMenuItem;
    MenuItem39:          TMenuItem;
    MenuItem41:          TMenuItem;
    MenuItem43:          TMenuItem;
    MenuItem44:          TMenuItem;
    MenuItem45:          TMenuItem;
    MenuItem46:          TMenuItem;
    MenuItem47:          TMenuItem;
    MenuItem48:          TMenuItem;
    MenuItem49:          TMenuItem;
    MenuItem50:          TMenuItem;
    MenuItem51:          TMenuItem;
    MenuItem52:          TMenuItem;
    MenuItem53:          TMenuItem;
    MenuItem54:          TMenuItem;
    MenuItem55:          TMenuItem;
    MenuItem56:          TMenuItem;
    MenuItem57:          TMenuItem;
    MenuItem58:          TMenuItem;
    miSelection:         TMenuItem;
    mmEmpty:             TMainMenu;
    mmMainMenu:          TMainMenu;
    MenuItem1:           TMenuItem;
    MenuItem10:          TMenuItem;
    MenuItem11:          TMenuItem;
    MenuItem14:          TMenuItem;
    MenuItem15:          TMenuItem;
    MenuItem17:          TMenuItem;
    MenuItem18:          TMenuItem;
    MenuItem19:          TMenuItem;
    MenuItem2:           TMenuItem;
    MenuItem20:          TMenuItem;
    MenuItem21:          TMenuItem;
    MenuItem22:          TMenuItem;
    MenuItem23:          TMenuItem;
    MenuItem24:          TMenuItem;
    MenuItem25:          TMenuItem;
    MenuItem26:          TMenuItem;
    MenuItem27:          TMenuItem;
    MenuItem28:          TMenuItem;
    MenuItem29:          TMenuItem;
    MenuItem3:           TMenuItem;
    MenuItem30:          TMenuItem;
    MenuItem31:          TMenuItem;
    MenuItem32:          TMenuItem;
    MenuItem33:          TMenuItem;
    MenuItem34:          TMenuItem;
    MenuItem35:          TMenuItem;
    MenuItem36:          TMenuItem;
    MenuItem37:          TMenuItem;
    MenuItem38:          TMenuItem;
    MenuItem4:           TMenuItem;
    MenuItem40:          TMenuItem;
    MenuItem42:          TMenuItem;
    MenuItem5:           TMenuItem;
    MenuItem6:           TMenuItem;
    MenuItem7:           TMenuItem;
    MenuItem8:           TMenuItem;
    MenuItem9:           TMenuItem;
    nbTxRight:           TNotebook;
    Page1:               TPage;
    Page2:               TPage;
    pSearch:             TPanel;
    pPortToolbar:        TPanel;
    pMainToolbar:        TPanel;
    pToolbar:            TPanel;
    pTranceiverMsg:      TPanel;
    pTxPanelDelta1:      TPanel;
    pTxRightPanel:       TPanel;
    psSplitterTxRx:      TPairSplitter;
    psSide1:             TPairSplitterSide;
    psSide2:             TPairSplitterSide;
    pLEDTx:              TPanel;
    pLEDRx:              TPanel;
    pPortSettings:       TPanel;
    Panel5:              TPanel;
    pmPort:              TPopupMenu;
    pRxRightPanel:       TPanel;
    pTxPanelDelta:       TPanel;
    rbTx:                TRadioButton;
    rbRx:                TRadioButton;
    sbtnSearch1:         TSpeedButton;
    seBaudRateCustom:    TSpinEdit;
    seRx:                TSynEdit;
    seTxHex:             TSynEdit;
    seTx:                TSynEdit;
    seRxHex:             TSynEdit;
    sbtnSearch:          TSpeedButton;
    sgTxSequences:       TStringGrid;
    tbTxSequences:       TToolBar;
    tmrMain50ms:         TTimer;
    tbPort:              TToolBar;
    tbRx:                TToolBar;
    tbTx:                TToolBar;
    ToolButton1:         TToolButton;
    ToolButton10:        TToolButton;
    ToolButton11:        TToolButton;
    ToolButton12:        TToolButton;
    ToolButton13:        TToolButton;
    ToolButton15:        TToolButton;
    ToolButton16:        TToolButton;
    ToolButton17:        TToolButton;
    ToolButton18:        TToolButton;
    ToolButton19:        TToolButton;
    ToolButton2:         TToolButton;
    ToolButton20:        TToolButton;
    ToolButton21:        TToolButton;
    ToolButton22:        TToolButton;
    ToolButton23:        TToolButton;
    ToolButton24:        TToolButton;
    ToolButton25:        TToolButton;
    ToolButton26:        TToolButton;
    ToolButton27:        TToolButton;
    ToolButton28:        TToolButton;
    ToolButton29:        TToolButton;
    ToolButton3:         TToolButton;
    ToolButton30:        TToolButton;
    ToolButton31:        TToolButton;
    ToolButton32:        TToolButton;
    ToolButton33:        TToolButton;
    ToolButton34:        TToolButton;
    ToolButton35:        TToolButton;
    ToolButton36:        TToolButton;
    ToolButton37:        TToolButton;
    ToolButton38:        TToolButton;
    ToolButton39:        TToolButton;
    ToolButton4:         TToolButton;
    ToolButton40:        TToolButton;
    ToolButton5:         TToolButton;
    ToolButton8:         TToolButton;
    tiTrayIcon:          TTrayIcon;
    TxOpenDialog:        TOpenDialog;
    pSignalRTS:          TPanel;
    pSignalDTR:          TPanel;
    pSignalBreak:        TPanel;
    pSignalDSR:          TPanel;
    pSignalRing:         TPanel;
    pSignalRLSD:         TPanel;
    pTranceiver:         TPanel;
    pSignalCTS:          TPanel;
    RxSaveDialog:        TSaveDialog;
    seAutoSendTime:      TSpinEdit;
    sbRTS:               TSpeedButton;
    sbDTR:               TSpeedButton;
    sbBreak:             TSpeedButton;
    SplitterTx:          TSplitter;
    SplitterRx:          TSplitter;
    stStatusBar:         TStatusBar;
    tbMain:              TToolBar;
    ToolButton14:        TToolButton;
    ToolButton6:         TToolButton;
    ToolButton7:         TToolButton;
    ToolButton9:         TToolButton;

    procedure acInsertCharExecute(Sender: TObject);
    procedure acResetExecute(Sender: TObject);
    procedure acRxExportExecute(Sender: TObject);
    procedure acTxImportExecute(Sender: TObject);
    procedure cbRxTypeChange(Sender: TObject);
    procedure cbTxTypeChange(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure pSplitterTxRxResize(Sender: TObject);
    procedure pTranceiverMsgResize(Sender: TObject);
    procedure seRxChange(Sender: TObject);
    procedure seTxChange(Sender: TObject);
    procedure seTxRxMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    procedure SetPortSettingsControls(Sender: TObject);
    procedure sgTxSequencesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure tmrMain50msTimer(Sender: TObject);
    procedure tiTrayIconClick(Sender: TObject);

    procedure actionCommon(Sender: TObject);
    procedure actionViewGeneral(Sender: TObject);
    procedure actionPortGeneral(Sender: TObject);
    procedure actionTxGeneral(Sender: TObject);
    procedure actionRxGeneral(Sender: TObject);
    procedure actionSearchGeneral(Sender: TObject);
    procedure SettingsApply(Sender: TObject = nil);

    procedure sgTxSequencesEditingDone(Sender: TObject);
    procedure sgTxSequencesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgTxSequencesPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure sgTxSequencesUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure sgTxSequencesChangeBounds(Sender: TObject);
    procedure sgTxSequencesUpdate;

  PRIVATE
    FWSPrevious: TWindowState;

  PRIVATE
    procedure SettingsSaveToIni;
    procedure SettingsLoadFromIni;

    procedure OnCommRxStart;
    procedure OnCommRxEnd;
    procedure OnCommTxStart;
    procedure OnCommTxEnd;

    procedure UpdateControls(AForceUpdate: Boolean = False);
    procedure UpdateIndicators;
    procedure UpdatePanelsLayout;
    function UpdateSelectionInfo(ASynEdit: TSynEdit; AEncoding: String;
      InHEX: Boolean = False): String;
    procedure LanguageChange;
    procedure EncodingsTxRxSet;

    function GetEditorCursorPosition(ASynEdit: TSynEdit; AEncoding: String;
      InHEX: Boolean = False): Integer;
    function GetEditorSelectionSize(ASynEdit: TSynEdit; AEncoding: String;
      InHEX: Boolean = False): Integer;
  PUBLIC
  end;

var
  fmMain:        TfmMain;
  serial:        TSerialPortThread; // класс-поток для работы с портом
  txSeqList:     TSequencesList;    // класс-хранитель списка сохраненных сообщений для передачи
  txSequence:    Boolean = False;   // флаг запуска передачи выбранного сбщ из списка сохраненных
  tx, rx:        String;            // буферы ввода и вывода
  splitPercent:  Double = 0.5;      // положение разделителя полей в/в (от 0 до 1)
  redrawBoxes:   Boolean;           // флаг запуска перерисовки полей в/в
  portWasOpened: Boolean = False;   // был ли открыт порт при завершении предыдущей сессии

implementation

{$R *.lfm}

{ TfmMain }

{ ***  Обработка событий главной формы  *** }

// инициализация
procedure TfmMain.FormCreate(Sender: TObject);
  begin
    //ReadAppInfo;
    IniStorageMain.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;

    Menu := mmEmpty;

    serial.Start;

    serial.OnRxStart := @OnCommRxStart;
    serial.OnRxEnd   := @OnCommRxEnd;
    serial.OnTxStart := @OnCommTxStart;
    serial.OnTxEnd   := @OnCommTxEnd;
  end;

// появление формы главного окна на экране
procedure TfmMain.FormShow(Sender: TObject);

  procedure SetControlWidth(ACtrl: TControl; ASize: Integer);
    begin
      if (ACtrl <> nil) and (ASize > 0) then
        begin
        ACtrl.Constraints.MinWidth := ASize;
        ACtrl.Constraints.MaxWidth := ASize;
        end;

      case ACtrl.ToString of

        'TComboBox':
          TComboBox(ACtrl).ItemWidth := GetListStringsMaxWidth(Self, TComboBox(ACtrl).Items);

        'TLabel':
          TLabel(ACtrl).Constraints.MaxWidth := 0;

        end;
    end;

  procedure SetControlSizes(ACtrl: TControl; ASizeH: Integer; ASizeW: Integer = 0);
    begin
      if ACtrl = nil then Exit;

      if ACtrl.ToString = 'TToolBar' then
        begin
        TToolBar(ACtrl).ButtonHeight := ASizeH;
        TToolBar(ACtrl).ButtonWidth  := CheckBoolean(ASizeW > 0, ASizeW, ASizeH);
        Exit;
        end;

      //if ACtrl.ToString = 'TToolBar' then
        begin
        ACtrl.Constraints.MinHeight := ASizeH;
        ACtrl.Constraints.MinWidth  := CheckBoolean(ASizeW > 0, ASizeW, ASizeH);
        end;
    end;

  begin
    //AutoSize := True;
    //AutoSize := False;
    //with Constraints do
    //  begin
    //  MinHeight := Height;
    //  MinWidth  := Width;
    //  end;

    tiTrayIcon.Icon := Application.Icon;
    pSearch.Color   := CheckBoolean(pSearch.Color > $222222, pSearch.Color - $222222, 0);

    serial.PortSettings(cbPortsList.Text, 0, 0, 'N', 0);
    EncodingsTxRxSet;
    SettingsLoadFromIni;
    SettingsApply;

    // заставка (если включена)
    fmAbout.ShowSplash(fmSettings.Splash);

    // адаптация размеров компонентов интерфейса
    SetControlWidth(pLEDTx, 2 * Canvas.GetTextHeight('0'));
    SetControlWidth(pLEDRx, 2 * Canvas.GetTextHeight('0'));
    SetControlWidth(lbTxSize, Canvas.GetTextWidth('0 234 567 ' + BYTE_MULTIPLE_2));
    SetControlWidth(lbRxSize, Canvas.GetTextWidth('0 234 567 ' + BYTE_MULTIPLE_2));
    SetControlWidth(cbTxType, 0);
    SetControlWidth(cbRxType, 0);
    SetControlWidth(cbPortsList, Canvas.GetTextWidth('COM00') + VertScrollBar.Size + 8);
    SetControlWidth(cbParityBits, Canvas.GetTextWidth('Space') + VertScrollBar.Size + 8);
    SetControlWidth(cbDataBits, Canvas.GetTextWidth('5') + VertScrollBar.Size + 8);
    SetControlWidth(cbStopBits, Canvas.GetTextWidth('1.5') + VertScrollBar.Size + 8);
    SetControlWidth(seAutoSendTime, Canvas.GetTextWidth('000000') + VertScrollBar.Size + 8);
    SetControlWidth(seBaudRateCustom, cbBaudrate.Width + 8);
    SetControlWidth(cbBaudrate, GetListStringsMaxWidth(Self, cbBaudrate.Items) + 8);
    SetControlWidth(cbTxType, GetListStringsMaxWidth(Self, cbTxType.Items) + 8);
    SetControlWidth(cbRxType, GetListStringsMaxWidth(Self, cbRxType.Items) + 8);
    SetControlWidth(cbTxEncoding, Canvas.GetTextWidth('macintosh1') + VertScrollBar.Size + 8);
    SetControlWidth(cbRxEncoding, Canvas.GetTextWidth('macintosh1') + VertScrollBar.Size + 8);

    SetControlSizes(tbTx, cbTxType.Height, cbTxType.Height + 8);
    SetControlSizes(tbRx, cbTxType.Height, cbTxType.Height + 8);
    SetControlSizes(tbMain, cbTxType.Height + 1);
    SetControlSizes(tbPort, cbTxType.Height + 1);
    SetControlSizes(tbTxSequences, cbTxType.Height + 1);

    // предустановка состояний элементов
    actionViewGeneral(acShowLineCounts);
    actionViewGeneral(acShowHEX);
    actionViewGeneral(acShowOnTop);
    actionViewGeneral(acShowSignals);
    actionSearchGeneral(acSearch);
    actionSearchGeneral(rbTx);

    // высота индикаторов RS-232
    pSignalBreak.Constraints.MinHeight := Canvas.GetTextHeight('0') + 2;

    // автоподключение к порту
    if fmSettings.Autoconnect and portWasOpened then acConnect.Execute;

    // начальная инициализация
    acScan.Execute;
    //acRxClear.Execute;
    //acTxClear.Execute;
    SetPortSettingsControls(nil);

    OnShow   := nil;       // выкл. обработчик, нужен только при запуске
    Position := poDefault; // чтобы не менялась позиция окна при разворачивании из трея
    FormChangeBounds(nil);
    pSplitterTxRxResize(nil);
  end;

// изменение состояния главного окна (свернуто, нормально, развернуто)
procedure TfmMain.FormChangeBounds(Sender: TObject);
  begin
    if OnShow <> nil then Exit;

    if WindowState <> wsMinimized then
      FWSPrevious := WindowState;

    if fmSettings.MinToTray and (WindowState = wsMinimized) then
      tiTrayIconClick(Sender);

    fmChart.UpdateFormPosition;
    stStatusBar.Panels.Items[0].Width := Width - stStatusBar.Height -
      Canvas.GetTextWidth(' COM00, 0-N-0, 000000 ' + SPEED);

    redrawBoxes := True;
  end;

// действие при попытке закрыть приложение
procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    SettingsSaveToIni;
  end;

procedure TfmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
  begin
    TxOpenDialog.FileName := FileNames[0];
    acTxImportExecute(nil);
  end;


{ ***  Работа с хранилищем настроек  *** }

// сохранение настроек в файл INI
procedure TfmMain.SettingsSaveToIni;
  begin
    with IniStorageMain do
      begin
      if not Active then Exit;

      // параметры состояния формы и компонентов
      IniSection := 'Last Parameters';
      WriteInteger('WindowMainTop', fmMain.RestoredTop);
      WriteInteger('WindowMainLeft', fmMain.RestoredLeft);
      WriteInteger('WindowMainWidth', fmMain.RestoredWidth);
      WriteInteger('WindowMainHeight', fmMain.RestoredHeight);
      WriteInteger('WindowMainState', Ord(fmMain.WindowState));
      WriteBoolean('OnTop', acShowOnTop.Checked);
      WriteInteger('Splitter', round(splitPercent * 1e9));
      WriteInteger('EncodingTx', cbTxEncoding.ItemIndex);
      WriteInteger('EncodingRx', cbRxEncoding.ItemIndex);
      WriteInteger('TypeTx', cbTxType.ItemIndex);
      WriteInteger('TypeRx', cbRxType.ItemIndex);
      WriteBoolean('PortOpened', serial.Connected);

      IniSection := 'Buffers Data';

      WriteString('TxBufferData',
        CheckBoolean(fmSettings.TxRestore, EncodeStringBase64(seTx.Text), ''));

      WriteString('RxBufferData',
        CheckBoolean(fmSettings.RxRestore, EncodeStringBase64(rx), ''));

      IniSection := ''; // выход из текущей секции
      end;

    txSeqList.SaveToIni(IniStorageMain);
  end;

// загрузка настроек из файла INI
procedure TfmMain.SettingsLoadFromIni;
  var
    cnt: Integer;
  begin
    with IniStorageMain do
      begin

      // параметры состояния формы и компонентов
      IniSection    := 'Last Parameters';
      fmMain.Width  := ReadInteger('WindowMainWidth', 650);
      fmMain.Height := ReadInteger('WindowMainHeight', 420);
      fmMain.Top    := ReadInteger('WindowMainTop', (Screen.Height - Height) div 2);
      fmMain.Left   := ReadInteger('WindowMainLeft', (Screen.Width - Width) div 2);

      fmMain.WindowState     := TWindowState(ReadInteger('WindowMainState', 0));
      acShowOnTop.Checked    := ReadBoolean('OnTop', False);
      splitPercent           := ReadInteger('Splitter', round(0.5 * 1e9)) / 1e9;
      cbTxEncoding.ItemIndex := ReadInteger('EncodingTx', 0);
      cbRxEncoding.ItemIndex := ReadInteger('EncodingRx', 0);
      cbTxType.ItemIndex     := ReadInteger('TypeTx', 0);
      cbRxType.ItemIndex     := ReadInteger('TypeRx', 0);
      portWasOpened          := ReadBoolean('PortOpened', False);

      IniSection := 'Buffers Data';

        try
        seTx.Text := DecodeStringBase64(ReadString('TxBufferData', ''));
        rx        := DecodeStringBase64(ReadString('RxBufferData', ''));
        seRxChange(nil);
        except
        end;

      IniSection := ''; // выход из текущей секции
      end;

    txSeqList.LoadFromIni(IniStorageMain);
    sgTxSequencesUpdate;

    // на случай, если некорректные параметры положения формы
    if abs(Top) > Screen.Height - Height then Top := (Screen.Height - Height) div 2;
    if abs(Left) > Screen.Width - Width then Left := (Screen.Width - Width) div 2;
  end;

// сброс настроек
procedure TfmMain.acResetExecute(Sender: TObject);
  begin
    if fmConfirm.Show(TXT_RESET, WARN_RESET, [mbYes, mbNo], Self) <> mrYes then Exit;

    // при сбросе настроек отключаем хранилища
    IniStorageMain.Active                := False;
    fmSettings.IniStorageSettings.Active := False;
    fmCommands.IniStorageCmd.Active      := False;

    // восстанавливаем настройки - удаляем файл настроек
    if FileExistsUTF8(IniStorageMain.IniFileName) then
      DeleteFileUTF8(IniStorageMain.IniFileName);
  end;


procedure TfmMain.pTranceiverMsgResize(Sender: TObject);
  begin
    BeginFormUpdate;
    pTranceiver.Constraints.MaxHeight := pTranceiverMsg.Height;
    pTranceiver.Constraints.MaxWidth  := pTranceiverMsg.Width;
    EndFormUpdate;
  end;

procedure TfmMain.pSplitterTxRxResize(Sender: TObject);
  var
    x: Integer;
  begin
    with psSplitterTxRx do
      begin
      x := CheckBoolean(fmSettings.PanelsLayout in [plTxTop, plTxDown], Height, Width);

      if (splitPercent < 0) or (splitPercent > 1) then
        splitPercent := 0.5;

      Position := round(x * splitPercent);

      if Visible then
        splitPercent := Position / x;
      end;
  end;

procedure TfmMain.tiTrayIconClick(Sender: TObject);
  begin
    if Showing then
      fmMain.Hide
    else
      begin
      WindowState := FWSPrevious;
      fmMain.Show;
      seRxChange(nil);
      end;
  end;

procedure TfmMain.tmrMain50msTimer(Sender: TObject);
  const
    tmr1s: Integer = 0;

  procedure UpdateLabelIfNotEmpty(ALabel: TLabel; s: String);
    begin
      ALabel.Caption := CheckBoolean(s = '', ALabel.Caption, s);
    end;

  begin
    BeginFormUpdate;

    UpdateControls;
    UpdateIndicators;

    UpdateLabelIfNotEmpty(lbTxPosAndSel, UpdateSelectionInfo(seTx,
      GetEncodingByIndex(cbTxEncoding.ItemIndex)));

    UpdateLabelIfNotEmpty(lbRxPosAndSel, UpdateSelectionInfo(seRx,
      GetEncodingByIndex(cbRxEncoding.ItemIndex)));

    UpdateLabelIfNotEmpty(lbTxPosAndSel, UpdateSelectionInfo(seTxHex,
      GetEncodingByIndex(cbTxEncoding.ItemIndex), True));

    UpdateLabelIfNotEmpty(lbRxPosAndSel, UpdateSelectionInfo(seRxHex,
      GetEncodingByIndex(cbRxEncoding.ItemIndex), True));

    // перерисовка полей в/в при необходимости
    if tmr1s > 0 then Dec(tmr1s) else
      begin
      if redrawBoxes then
        begin
        tmr1s       := 1000 div tmrMain50ms.Interval;
        redrawBoxes := False;
        cbTxTypeChange(Sender);
        cbRxTypeChange(Sender);
        end;
      end;

    EndFormUpdate;
  end;


{ ***  Обработчики приема и передачи данных  *** }

procedure TfmMain.OnCommRxStart;
  begin
  end;

procedure TfmMain.OnCommRxEnd;
  var
    index: Integer;
  begin
    if serial.RxEnable then
      begin
      rx += serial.DataRx;

      // ограничение объема данных в буфере приема
      with fmSettings do
        if (RxSizeLimit > 0) and (rx.Length > RxSizeLimit) then
          rx := rx.Remove(0, rx.Length - RxSizeLimit);

      // простой автоответ на вх. последовательность
      if acAutoAnswerEnable.Checked then
        begin
        index := fmCommands.SequenceList.IndexOf(serial.DataRx);
        if index >= 0 then
          serial.TransmitAnswer(UTF8ToEncodingByIndex(
            fmCommands.AnswerList.Strings[index], cbRxEncoding.ItemIndex));
        end;

      seRxChange(nil);
      end;
  end;

procedure TfmMain.OnCommTxStart;
  begin
    if not txSequence then
      serial.DataTx := tx
    else
      serial.DataTx := txSeqList.Data[sgTxSequences.Selection.Top - 1];

    acTx.Enabled := False;
  end;

procedure TfmMain.OnCommTxEnd;
  begin
    if acTxCommandMode.Checked and not txSequence then
      acTxClear.Execute;

    txSequence := False;
  end;


{ ***  Ввод/вывод данных  *** }

procedure TfmMain.seTxChange(Sender: TObject);
  var
    x: Integer;
  begin
    with seTx do
      case cbTxType.ItemIndex of
        TT_ASC:
          begin
          x  := (Lines.TextLineBreakStyle = tlbsCRLF).ToInteger + 1;
          tx := UTF8ToEncodingByIndex(Text.Remove(Text.Length - x, x), cbTxEncoding.ItemIndex);
          end;
        TT_HEX: tx := CodesToStr(Text, 16, 0);
        TT_BIN: tx := CodesToStr(Text, 2, 0);
        TT_DEC: tx := CodesToStr(Text, 10, 0);
        end;

    if seTxHex.Visible then
      begin
      seTxHex.BeginUpdate;
      with fmSettings do
        seTxHex.Text   := StringToHex(tx, HEXLineBytes, HEXBlockBytes);
      seTxHex.SelStart := Length(seTxHex.Text);
      seTxHex.EndUpdate;
      end;

    if lbTxSize.Visible then
      lbTxSize.Caption := SizeStr(tx);
  end;

procedure TfmMain.seRxChange(Sender: TObject);

  procedure SynEditUpdateWithScroll(ASE: TSynEdit; AText: String; LE: Boolean = False);
    begin
      with ASE do
        begin
        BeginUpdate;
        if AText = '' then
          Clear
        else
          Text   := AText + CheckBoolean(LE, LineEnding, '');  // SetFocus;
        SelStart := Text.Length;  // Perform(EM_SCROLLCARET, 0, 0);
        EndUpdate;
        end;
    end;

  procedure SynEditUpdateWithoutScroll(ASE: TSynEdit; AText: String; LE: Boolean = False);
    var
      x, y, ss, se, t: Longint;
    begin
      with ASE do
        begin
        BeginUpdate;
        t  := TopLine;
        x  := CaretX;
        y  := CaretY;
        ss := SelStart;
        se := SelEnd;
        if AText = '' then
          Clear
        else
          Text   := AText + CheckBoolean(LE, LineEnding, '');
        TopLine  := t;
        SelStart := ss;
        SelEnd   := se;
        CaretX   := x;
        CaretY   := y;
        EndUpdate;
        end;
    end;

  procedure SynEditUpdate(AScroll: Boolean; ASE: TSynEdit; AText: String; LE: Boolean = False);
    begin
      if AScroll then
        SynEditUpdateWithScroll(ASE, AText, LE)
      else
        SynEditUpdateWithoutScroll(ASE, AText, LE);
    end;

  var
    rxUTF: String;
  begin
    if not Visible then Exit; // если форма скрыта, то обновлять незачем

    case cbRxType.ItemIndex of
      TT_ASC: rxUTF := EncodingToUTF8ByIndex(rx, cbRxEncoding.ItemIndex);
      TT_HEX: rxUTF := StrToCodes(rx, 16, seRx.CharsInWindow);
      TT_BIN: rxUTF := StrToCodes(rx, 2, seRx.CharsInWindow);
      TT_DEC: rxUTF := StrToCodes(rx, 10, seRx.CharsInWindow);
      end;

    if acShowRxBox.Checked then
      begin
      SynEditUpdate(acRxScrollToEnd.Checked, seRx, rxUTF, True);

      if seRxHex.Visible then
        SynEditUpdate(acRxScrollToEnd.Checked, seRxHex,
          StringToHex(rx, fmSettings.HEXLineBytes, fmSettings.HEXBlockBytes));
      end;

    if lbRxSize.Visible then lbRxSize.Caption := SizeStr(rx);
    if fmChart.Visible then fmChart.GraphData := rx;
  end;

procedure TfmMain.cbTxTypeChange(Sender: TObject);
  begin
    txSeqList.Encoding := GetEncodingByIndex(cbTxEncoding.ItemIndex);

    seTx.BeginUpdate;
    case cbTxType.ItemIndex of
      TT_ASC: seTx.Text := EncodingToUTF8ByIndex(tx, cbTxEncoding.ItemIndex) +
          CheckBoolean(tx.Length > 0, LineEnding, '');
      TT_HEX: seTx.Text := StrToCodes(tx, 16, seTx.CharsInWindow);
      TT_BIN: seTx.Text := StrToCodes(tx, 2, seTx.CharsInWindow);
      TT_DEC: seTx.Text := StrToCodes(tx, 10, seTx.CharsInWindow);
      end;
    seTx.EndUpdate;

    seTxChange(Sender);
    sgTxSequencesUpdate;
    UpdateControls(True);
  end;

procedure TfmMain.cbRxTypeChange(Sender: TObject);
  begin
    seRxChange(Sender);
    UpdateControls(True);
  end;

procedure TfmMain.seTxRxMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  begin
    if Shift = [ssCtrl] then
      begin
      TSynEdit(Sender).Font.Size := TSynEdit(Sender).Font.Size
        + WheelDelta div abs(WheelDelta);

      case TSynEdit(Sender).Name of
        'seTx': fmSettings.FontTx.Assign(seTx.Font);
        'seRx': fmSettings.FontRx.Assign(seRx.Font);
        end;

      sgTxSequencesChangeBounds(Sender);
      end;
  end;


{ ***  Управление портом  *** }

// настройки трансивера UART
procedure TfmMain.SetPortSettingsControls(Sender: TObject);
  const
    parity: array [0..4] of Char = ('N', 'E', 'O', 'M', 'S');
  var
    baudrateValue: Integer;
  begin
    if cbBaudrate.ItemIndex = cbBaudrate.Items.Count - 1 then
      begin
      baudrateValue            := seBaudRateCustom.Value;
      seBaudRateCustom.Visible := True;
      end
    else
      begin
      baudrateValue            := String(cbBaudrate.Text).ToInteger;
      seBaudRateCustom.Visible := False;
      end;

    if Sender <> nil then
      serial.PortSettings(
        cbPortsList.Text,
        baudrateValue,
        cbDataBits.ItemIndex + 5,
        parity[cbParityBits.ItemIndex],
        cbStopBits.ItemIndex);

    UpdateControls(True);
  end;

procedure TfmMain.actionPortGeneral(Sender: TObject);
  begin
    case TAction(Sender).Name of

      // получение списка доступных портов
      'acScan':
        begin
        cbPortsList.Items.CommaText := serial.GetExistingPorts;
        cbPortsList.ItemWidth       := GetListStringsMaxWidth(Self, cbPortsList.Items);
        cbPortsList.ItemIndex       := serial.GetPortIndexInList;
        if cbPortsList.ItemIndex < 0 then cbPortsList.ItemIndex := 0;

        stStatusBar.Panels.Items[1].Text :=
          PORTS_FINDED + ': ' + cbPortsList.Items.Count.ToString;
        end;

      // подключение к порту / отключение от порта
      'acConnect':
        if serial.Connected then
          begin
          serial.Started := False;
          end
        else
          begin
          SetPortSettingsControls(Sender);
          serial.Started := True;
          end;

      // разрешение работы приемника
      'acRxEnable':
        begin
        serial.RxEnable := acRxEnable.Checked;
        UpdateControls(True);
        end;

      // управление сигналом RS-232 RTS
      'acToggleRTS':
        serial.Signal[ssRTS] := not serial.Signal[ssRTS];

      // управление сигналом RS-232 DTR
      'acToggleDTR':
        serial.Signal[ssDTR] := not serial.Signal[ssDTR];

      // управление сигналом RS-232 Break
      'acToggleBreak':
        serial.Signal[ssBreak] := not serial.Signal[ssBreak];

      end;
  end;


{ ***  Управление передатчиком  *** }

procedure TfmMain.actionTxGeneral(Sender: TObject);
  begin
    case TComponent(Sender).Name of

      // команда отправки данных
      'acTx':
        serial.Transmit;

      // автопередача по таймеру
      'acAutoSend', 'seAutoSendTime':
        begin
        serial.AutoSendInterval := seAutoSendTime.Value;
        serial.AutoSend         := acAutoSend.Checked;
        seAutoSendTime.Visible  := acAutoSend.Checked;

        // перепривязка к кнопке acAutoSend (без этого "съезжает" вправо)
        seAutoSendTime.AnchorSideLeft.Side    := asrLeft;
        seAutoSendTime.AnchorSideLeft.Control := ToolButton24;
        end;

      // очистка поля ввода
      'acTxClear':
        begin
        tx := '';
        seTx.Clear;
        seTxChange(Sender);
        end;

      // добавить данные из буфера в список сохраненных сообщений
      'acTxSeqAdd':
        if (tx <> '') then
          begin
          txSeqList.AddData(tx);
          sgTxSequencesUpdate;
          sgTxSequences.Row := sgTxSequences.RowCount;
          end;

      // редактировать выбранное сообщение
      'acTxSeqEdit':
        if acTxSequences.Checked and (tx <> '') then
          begin
          txSeqList.Data[sgTxSequences.Selection.Top - 1] := tx;
          sgTxSequencesUpdate;
          end;

      // удалить выбранное сообщение из списка
      'acTxSeqRemove':
        if acTxSequences.Checked then
          begin
          txSeqList.Delete(sgTxSequences.Selection.Top - 1);
          sgTxSequencesUpdate;
          end;

      // заменить данные в буфере данными выбранного сообщения
      'acTxSeqGet':
        if acTxSequences.Checked then
          begin
          tx := txSeqList.Data[sgTxSequences.Selection.Top - 1];
          cbTxTypeChange(nil);
          end;

      // отправить выбранное сообщение
      'acTxSeqSend', 'sgTxSequences':
        if acTxSequences.Checked then
          begin
          txSequence := True;
          serial.Transmit;
          end;

      // перемещение элемента списка вверх на 1 позицию
      'acTxSeqMoveUp':
        if acTxSequences.Checked then
          if txSeqList.Move(sgTxSequences.Selection.Top - 1, True) then
            begin
            sgTxSequences.Row := sgTxSequences.Selection.Top - 1;
            sgTxSequencesUpdate;
            end;

      // перемещение элемента списка вниз на 1 позицию
      'acTxSeqMoveDown':
        if acTxSequences.Checked then
          if txSeqList.Move(sgTxSequences.Selection.Top - 1, False) then
            begin
            sgTxSequences.Row := sgTxSequences.Selection.Top + 1;
            sgTxSequencesUpdate;
            end;
      end;
  end;

procedure TfmMain.acInsertCharExecute(Sender: TObject);
  var
    inserted_char, tmp:       String;
    select_start, txt_length: Integer;
  begin
    fmASCIIChar.Encoding := GetEncodingByIndex(cbTxEncoding.ItemIndex);

    with fmASCIIChar do
      if ShowModal = mrOk then
        begin
        case cbTxType.ItemIndex of
          TT_ASC: inserted_char :=
              EncodingToUTF8ByIndex(chr(SelectedChar), cbTxEncoding.ItemIndex);
          TT_HEX: inserted_char := SelectedChar.ToHexString(2);
          TT_BIN: inserted_char := intToBin(SelectedChar, 8);
          TT_DEC: inserted_char := SelectedChar.ToString;
          end;

        select_start := seTx.SelStart - 1;
        tmp          := seTx.Text;
        txt_length   := tmp.Length;

        if cbTxType.ItemIndex <> TT_ASC then
          begin
          if (select_start <> 0) and (select_start <= txt_length) and
            (tmp[select_start] <> ' ') then
            inserted_char := ' ' + inserted_char;

          if (select_start < txt_length) and (tmp[select_start + 1] <> ' ') then
            inserted_char := inserted_char + ' ';
          end;

        seTx.Text     := tmp.Insert(select_start, inserted_char);
        seTx.SelStart := select_start + inserted_char.Length + 1;
        seTx.SetFocus;
        end;

    seTxChange(Sender);
  end;

procedure TfmMain.acTxImportExecute(Sender: TObject);
  begin
    if Sender <> nil then
      begin
      TxOpenDialog.FileName := ExtractFileName(TxOpenDialog.FileName);
      if not TxOpenDialog.Execute then Exit;
      end;

    if not fmSettings.FileDataAdding and fmSettings.LoadWarning and (tx <> '')
      and (fmConfirm.Show(TXT_WARNING, WARN_LOAD, mbYesNo, Self) = mrNo) then Exit;

    with TStringList.Create do
      begin
      LoadFromFile(TxOpenDialog.FileName);
      if fmSettings.FileDataAdding then
        tx += Text
      else
        tx := Text;
      Free;
      end;

    cbTxTypeChange(Sender);
  end;


{ ***  Управление приемником  *** }

procedure TfmMain.actionRxGeneral(Sender: TObject);
  begin
    case TAction(Sender).Name of

      // настройка ответов на полученные данные
      'acAutoAnswerSetup':
        fmCommands.ShowModal;

      // копирование содержимого буфера приема в буфер обмена
      'acRxCopy':
        if seRx.Text.Length > 0 then
          Clipboard.AsText := seRx.Text.Remove(seRx.Text.Length - 2, 2);

      // очистка поля вывода (буфера приема)
      'acRxClear':
        begin
        rx := '';
        seRxChange(Sender);
        end;

      // показать окно построения графика по полученных данных
      'acShowGraph':
        begin
        fmChart.MainForm  := fmMain;
        fmChart.FormStyle := fmMain.FormStyle;
        fmChart.Show;
        seRxChange(nil);
        end;
      end;
  end;

procedure TfmMain.acRxExportExecute(Sender: TObject);
  var
    tmp: String;
  begin
    tmp := RxSaveDialog.FileName;

    if RxSaveDialog.Execute then
      begin

      with TStringList.Create do
        begin
        Text := rx;
        SaveToFile(RxSaveDialog.FileName);
        Free;
        end;

      RxSaveDialog.FileName := ExtractFileName(RxSaveDialog.FileName);
      end
    else
      RxSaveDialog.FileName := tmp;
  end;


{ ***  Управление видом  *** }
procedure TfmMain.actionViewGeneral(Sender: TObject);
  begin
    BeginFormUpdate;
    case TAction(Sender).Name of

      // опция главной формы 'поверх всех окон'
      'acShowOnTop':
        FormStyle := CheckBoolean(acShowOnTop.Checked, fsSystemStayOnTop, fsNormal);

      // вкл/выкл панель сигналов порта
      'acShowSignals':
        gbSignals.Visible := acShowSignals.Checked;

      // вкл/выкл номера строк
      'acShowLineCounts':
        begin
        seRx.Gutter.Visible    := acShowLineCounts.Checked;
        seRxHex.Gutter.Visible := acShowLineCounts.Checked;
        seTx.Gutter.Visible    := acShowLineCounts.Checked;
        seTxHex.Gutter.Visible := acShowLineCounts.Checked;
        end;

      // вкл/выкл доп. поле с данными в HEX-виде или панель сохраненных сообщений
      'acShowHEX', 'acTxSequences':
        begin
        //seTxHex.Visible    := acShowHEX.Checked;
        seRxHex.Visible       := acShowHEX.Checked;
        SplitterRx.Visible    := acShowHEX.Checked;
        nbTxRight.Visible     := acShowHEX.Checked or acTxSequences.Checked;
        SplitterTx.Visible    := nbTxRight.Visible;
        nbTxRight.PageIndex   := acTxSequences.Checked.ToInteger;
        acTxSeqEdit.Enabled   := acTxSequences.Checked;
        acTxSeqRemove.Enabled := acTxSequences.Checked;
        acTxSeqGet.Enabled    := acTxSequences.Checked;

        seRxChange(Sender);
        seTxChange(Sender);
        end;

      // вкл/выкл видимость полей в/в
      'acShowTxBox', 'acShowRxBox':
        begin
        gbRx.Visible           := acShowRxBox.Checked;
        gbTx.Visible           := acShowTxBox.Checked;
        psSplitterTxRx.Visible := gbRx.Visible and gbTx.Visible;
        UpdatePanelsLayout;

        if gbRx.Visible or gbTx.Visible then
          lbHelpHint.Caption := ''
        else
          lbHelpHint.Caption := MultiString(TXT_HELP_HINT);
        end;
      end;

    FormChangeBounds(Sender);
    EndFormUpdate;
    sgTxSequencesChangeBounds(nil);
  end;


{ ***  Поиск текста  *** }
procedure TfmMain.actionSearchGeneral(Sender: TObject);
  var
    se:         TSynEdit;
    option:     TSynSearchOptions = [];
    senderName: String;

  begin
    senderName := TComponent(Sender).Name;

    case senderName of

      // вкл/выкл панель поиска
      'acSearch':
        pSearch.Visible := acSearch.Checked;

      // выбор поля, по которому искать
      'rbTx', 'rbRx':
        begin
        BeginFormUpdate;
        cbSearchReplace.Visible    := rbTx.Checked;
        cbSearchReplaceAll.Visible := rbTx.Checked;
        edReplace.Visible          := rbTx.Checked;
        lbSearch1.Visible          := rbTx.Checked;
        lbSearch2.Visible          := rbTx.Checked;
        rbRx.Checked               := not rbTx.Checked;
        EndFormUpdate;
        end;

      // команда запуска поиска/замены
      'acSearchNext', 'acSearchPrev':
        begin
        se := TSynEdit(CheckBooleanPtr(rbTx.Checked, seTx, seRx));
        if not se.CanFocus then Exit;
        if cbSearchCaseSens.Checked then option += [ssoMatchCase];
        if senderName = 'acSearchPrev' then option += [ssoBackwards];
        if rbTx.Checked and cbSearchReplace.Checked then option += [ssoReplace];
        if rbTx.Checked and cbSearchReplaceAll.Checked then option += [ssoReplaceAll];
        se.SetFocus;
        se.SelStart := CheckBoolean(senderName = 'acSearchPrev', se.SelStart, se.SelEnd);
        se.SearchReplace(edSearch.Text, edReplace.Text, option);
        end;

      end;
  end;


{ ***  Команды общие  *** }
procedure TfmMain.actionCommon(Sender: TObject);
  begin
    case TAction(Sender).Name of

      // завершение работы приложения
      'acExit':
        Close;

      // открыть окно настроек приложения
      'acSettings':
        begin
        fmSettings.ShowModal;
        SettingsApply;
        end;

      // вызов справки html
      'acHelp':
        OpenURL('..' + DirectorySeparator + HELP_DIR + DirectorySeparator + HELP_FILE + '.html');

      // вызов справки markdown
      'acHelpMD':
        OpenURL('..' + DirectorySeparator + HELP_DIR + DirectorySeparator + HELP_FILE + '.md');

      // вызов справки онлайн
      'acHelpNet':
        OpenURL(APP_SITE_ADDRESS + '/' + HELP_DIR_ONLINE + '/' + HELP_FILE + '.md');

      // ссылка на репозиторий
      'acWebsite':
        OpenURL(APP_SITE_ADDRESS);

      // окно информации о приложении
      'acInfo':
        begin
        fmAbout.FormStyle := FormStyle;
        fmAbout.Show;
        end;
      end;
  end;


{ ***  Отображение таблицы сохраненных сообщений для передачи  *** }

// изменение размеров, подгонка ширины строк
procedure TfmMain.sgTxSequencesChangeBounds(Sender: TObject);
  var
    vsbWidth: Integer = 0;
  begin
    if not acTxSequences.Checked then Exit;

    with sgTxSequences, sgTxSequences.Columns do
      begin
      BeginUpdate;
      Font.Assign(seTx.Font);
      DefaultRowHeight := round(abs(seTx.Font.FontData.Height) * 1.3);

      Columns.Items[0].Title.Font.Assign(Font);
      Columns.Items[1].Title.Font.Assign(Font);
      Columns.Items[0].Title.Font.Bold := True;
      Columns.Items[1].Title.Font.Bold := True;

      // если полоса прокрутки есть - получаем ее ширину
      if VisibleRowCount < (RowCount - 1) then
        vsbWidth := VertScrollBar.Size;

      Items[0].Width := Canvas.GetTextWidth((RowCount - 1).ToString + '  ');
      Items[1].Width := Width - Items[0].Width - vsbWidth;
      EndUpdate;
      end;
  end;

// изменение шрифта перед выводом ячейки
procedure TfmMain.sgTxSequencesPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
  begin
    if (aCol > 0) and (aRow >= sgTxSequences.FixedRows) then
      sgTxSequences.Canvas.Font.Italic := txSeqList.HasCaption(aRow - 1);
  end;

// обновление содержимого таблицы сохраненных сообщений
procedure TfmMain.sgTxSequencesUpdate;
  const
    num_sys: array [0..3] of Integer = (0, 16, 2, 10);
  var
    i: Integer;
  begin
    if not acTxSequences.Checked then Exit;

    txSeqList.Encoding := GetEncodingByIndex(cbTxEncoding.ItemIndex);
    txSeqList.NumSys   := num_sys[cbTxType.ItemIndex];

    with sgTxSequences do
      begin
      BeginUpdate;

      RowCount := txSeqList.Count + 1;
      for i := 1 to txSeqList.Count do
        with Rows[i] do
          begin
          Strings[0] := i.ToString;
          Strings[1] := txSeqList.GetCell(i - 1);
          end;

      EndUpdate;
      end;

    sgTxSequencesChangeBounds(nil);
  end;

// подсказка для таблицы под курсором
procedure TfmMain.sgTxSequencesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    aCol, aRow: Integer;
  begin
    sgTxSequences.MouseToCell(X, Y, aCol, aRow);
    sgTxSequences.Hint := txSeqList.GetHint(aRow - 1);
  end;

// вход в режим редактирования заголовка сообщения правой кнопкой мыши
procedure TfmMain.sgTxSequencesMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  var
    aCol, aRow: Integer;
  begin
    with sgTxSequences do
      begin
      MouseToCell(X, Y, aCol, aRow);
      Col := aCol;

      if (Button = mbRight) and (SelectedColumn.Index > 0) then
        Options := Options + [goEditing, goAlwaysShowEditor] else
        Options := Options - [goEditing, goAlwaysShowEditor];
      end;
  end;

// обработка нажатий клавиатуры в таблице
procedure TfmMain.sgTxSequencesUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
  begin
    with sgTxSequences do
      begin
      Options := Options - [goEditing, goAlwaysShowEditor];

      if UTF8Key = chr(13) then
        actionTxGeneral(sgTxSequences)
      else
        if SelectedColumn.Index > 0 then
          Options := Options + [goEditing];
      end;
  end;

// окончание редактирования заголовка сообщения
procedure TfmMain.sgTxSequencesEditingDone(Sender: TObject);
  begin
    with sgTxSequences do
      if goEditing in Options then
        begin
        Options := Options - [goEditing, goAlwaysShowEditor];
        txSeqList.Caption[Selection.Top - 1] := Cells[1, Selection.Top];
        sgTxSequencesUpdate;
        end;
  end;


{ ***  Сервисные методы  *** }

// обновление изменяемых элементов интерфейса
procedure TfmMain.UpdateControls(AForceUpdate: Boolean);
  const
    updateFlag: Boolean = True;
    connected: Boolean  = True;
  var
    atitle, port, status: String;
  begin
    BeginFormUpdate;

    atitle              := fmAbout.AppIntName;
    port                := '';
    updateFlag          := connected xor serial.Connected;
    connected           := serial.Connected;
    acTx.Enabled        := connected and not serial.IsTxing;
    acTxSeqSend.Enabled := acTx.Enabled and acTxSequences.Checked;

    if AForceUpdate or updateFlag then
      begin
      if connected then
        begin
        port   := serial.ConfigString + ' ' + SPEED;
        status := serial.ConfigShort + ' ' + SPEED;
        atitle += ' - ' + port + ' - Tx';
        atitle += CheckBoolean(serial.RxEnable, '/Rx', '');

        acConnect.Caption    := DISCONNECT;
        acConnect.Hint       := DISCONNECT_HINT;
        acConnect.ImageIndex := 26;
        end
      else
        begin
        if serial.Error = ceNone then
          status := '' else
          status := serial.ErrorString + ' ' + serial.Port;

        acConnect.Caption    := CONNECT;
        acConnect.Hint       := CONNECT_HINT;
        acConnect.ImageIndex := 2;
        end;

      stStatusBar.Panels.Items[1].Text := status;
      Caption           := atitle;
      Application.Title := Caption;
      tiTrayIcon.Hint   := Caption;

      //cbTxEncoding.Enabled := cbTxType.ItemIndex = 0;
      //cbRxEncoding.Enabled := cbRxType.ItemIndex = 0;

      gbTx.Caption := TX_CAPTION +
        CheckBoolean(fmSettings.ShowEncoding and (cbTxType.ItemIndex = 0),
        ' — ' + TXT_ENCODING + ' ' + cbTxEncoding.Text, '');

      gbRx.Caption := RX_CAPTION +
        CheckBoolean(fmSettings.ShowEncoding and (cbRxType.ItemIndex = 0),
        ' — ' + TXT_ENCODING + ' ' + cbRxEncoding.Text, '');
      end;

    pTranceiver.Visible := acShowTxBox.Checked or acShowRxBox.Checked;
    EndFormUpdate;
  end;

// обновление состояния индикаторов сигналов порта
procedure TfmMain.UpdateIndicators;
  const
    lock: Integer = 0;

  function SetIndicatorText(AValue: Boolean): String;
    begin
      if not fmSettings.ShowRS232Captions then Exit('');
      Result := CheckBoolean(serial.Connected, indicatorText[AValue], '---');
    end;

  function SetIndicatorColor(AValue: Boolean): TColor;
    begin
      Result := CheckBoolean(serial.Connected, indicatorColor[AValue], inactiveColor);
    end;

  begin
    if lock > 0 then Exit;
    Inc(lock);

    // цвета индикаторов активности Tx/Rx
    if fmSettings.ShowIndicators then
      begin
      pLEDTx.Color := SetIndicatorColor(serial.IsTxing);
      pLEDRx.Color := SetIndicatorColor(serial.IsRxing);
      end;

    // цвета индикаторов RS-232
    pSignalRTS.Color   := SetIndicatorColor(serial.Signal[ssRTS]);
    pSignalDTR.Color   := SetIndicatorColor(serial.Signal[ssDTR]);
    pSignalBreak.Color := SetIndicatorColor(serial.Signal[ssBreak]);
    pSignalCTS.Color   := SetIndicatorColor(serial.Signal[ssCTS]);
    pSignalDSR.Color   := SetIndicatorColor(serial.Signal[ssDSR]);
    pSignalRing.Color  := SetIndicatorColor(serial.Signal[ssRing]);
    pSignalRLSD.Color  := SetIndicatorColor(serial.Signal[ssRLSD]);

    // подписи индикаторов RS-232
    pSignalRTS.Caption   := SetIndicatorText(serial.Signal[ssRTS]);
    pSignalDTR.Caption   := SetIndicatorText(serial.Signal[ssDTR]);
    pSignalBreak.Caption := SetIndicatorText(serial.Signal[ssBreak]);
    pSignalCTS.Caption   := SetIndicatorText(serial.Signal[ssCTS]);
    pSignalDSR.Caption   := SetIndicatorText(serial.Signal[ssDSR]);
    pSignalRing.Caption  := SetIndicatorText(serial.Signal[ssRing]);
    pSignalRLSD.Caption  := SetIndicatorText(serial.Signal[ssRLSD]);

    // краткое текст ошибки в панели статуса
    if serial.Error <> ceNone then
      begin
      stStatusBar.Panels.Items[1].Text := serial.ErrorString;
      serial.ResetError;
      end;

    Dec(lock);
  end;

// обновление схемы размещения полей в/в
procedure TfmMain.UpdatePanelsLayout;
  begin
    BeginFormUpdate;

    with fmSettings do
        try
        if psSplitterTxRx.Visible then
          begin
          gbTx.Parent := TPairSplitterSide(CheckBooleanPtr(
            PanelsLayout in [plTxTop, plTxLeft], psSide1, psSide2));

          gbRx.Parent := TPairSplitterSide(CheckBooleanPtr(
            PanelsLayout in [plTxTop, plTxLeft], psSide2, psSide1));
          end
        else
          begin
          gbTx.Parent := pTranceiver;
          gbRx.Parent := pTranceiver;
          end;

        psSplitterTxRx.SplitterType := CheckBoolean(
          PanelsLayout in [plTxTop, plTxDown], pstVertical, pstHorizontal);

        psSplitterTxRx.Cursor := CheckBoolean(
          PanelsLayout in [plTxTop, plTxDown], crVSplit, crHSplit);
        finally
        end;

    EndFormUpdate;
  end;

// обновление информации о выделении в полях в/в
function TfmMain.UpdateSelectionInfo(ASynEdit: TSynEdit; AEncoding: String;
  InHEX: Boolean = False): String;
  var
    pos, sel: Integer;
  begin
    if fmSettings.ShowPosAndSel then
      if not ASynEdit.Focused then
        Result := ''
      else
        begin
        pos := GetEditorCursorPosition(ASynEdit, AEncoding, InHEX);
        sel := GetEditorSelectionSize(ASynEdit, AEncoding, InHEX);

        if fmSettings.ShowMenu then
          miSelection.Caption :=
            TXT_BYTE_POS + ' ' + pos.ToString + ', ' +
            TXT_BYTE_SEL + ' ' + sel.ToString
        else
          Result              := pos.ToString + ', ' + sel.ToString;
        end;
  end;


// формирование списка кодировок и режимов отображения данных
procedure TfmMain.EncodingsTxRxSet;

  procedure UpdateComboBoxEncoding(AComboBox: TComboBox);
    begin
      with AComboBox do
        begin
        Tag := ItemIndex;
        EncodingsListAssign(Items);
        ItemIndex := CheckBoolean(Tag < 0, 0, Tag);
        ItemWidth := GetListStringsMaxWidth(Self, Items);
        end;
    end;

  procedure UpdateComboBoxType(AComboBox: TComboBox);
    begin
      with AComboBox do
        begin
        Tag := ItemIndex;
        Items.Clear;
        Items.Add(TEXT_TYPE_TEXT);
        Items.Add('HEX');
        Items.Add('BIN');
        Items.Add('DEC');
        ItemIndex := CheckBoolean(Tag < 0, 0, Tag);
        end;
    end;

  begin
    EncodingsListUpdate;
    UpdateComboBoxEncoding(cbTxEncoding);
    UpdateComboBoxEncoding(cbRxEncoding);
    UpdateComboBoxType(cbTxType);
    UpdateComboBoxType(cbRxType);
  end;

// получение позиции курсора в поле в/в (смещение в байтах)
function TfmMain.GetEditorCursorPosition(ASynEdit: TSynEdit; AEncoding: String;
  InHEX: Boolean = False): Integer;
  const
    lastSel: Integer = 0;
    lastRes: Integer = 0;
  begin
    with ASynEdit do
      if lastSel <> SelStart then
        begin
        if InHEX then
          with fmSettings do
            lastRes := HexStringPosition(SelStart, HEXLineBytes, HEXBlockBytes)
        else
          lastRes   := UTF8ToEncoding(Text.Substring(0, SelStart), AEncoding).Length - 1;

        if lastRes < 0 then lastRes := 0;

        lastSel := SelStart;
        end;

    Result := lastRes;
  end;

// получение размера выделения в поле в/в (в байтах)
function TfmMain.GetEditorSelectionSize(ASynEdit: TSynEdit;
  AEncoding: String; InHEX: Boolean = False): Integer;
  const
    lastLen: Integer = 0;
    lastRes: Integer = 0;
  begin
    with ASynEdit do
      if lastLen <> SelEnd - SelStart then
        begin
        if InHEX then
          with fmSettings do
            lastRes :=
              HexStringPosition(SelEnd + 2, HEXLineBytes, HEXBlockBytes) -
              HexStringPosition(SelStart, HEXLineBytes, HEXBlockBytes)
        else
          lastRes   := UTF8ToEncoding(
            Text.Substring(SelStart - 1, SelEnd - SelStart), AEncoding).Length;

        lastLen := SelEnd - SelStart;
        end;

    Result := lastRes;
  end;


// действие: применение настроек
procedure TfmMain.SettingsApply(Sender: TObject);
  var
    tmpSE: TCustomSynEdit;
  begin
    BeginFormUpdate;
    with fmSettings do
        try
        seTx.Font.Assign(FontTx);
        seTxHex.Font.Assign(FontTx);
        seRx.Font.Assign(FontRx);
        seRxHex.Font.Assign(FontRx);

        fmMain.Menu           := TMainMenu(CheckBooleanPtr(ShowMenu, mmMainMenu, mmEmpty));
        miSelection.Visible   := ShowPosAndSel;
        lbTxPosAndSel.Visible := ShowPosAndSel and not ShowMenu;
        lbRxPosAndSel.Visible := ShowPosAndSel and not ShowMenu;
        stStatusBar.Visible   := ShowStatusBar;
        pLEDTx.Visible        := ShowIndicators;
        pLEDRx.Visible        := ShowIndicators;
        lbTxSize.Visible      := ShowSizes;
        lbRxSize.Visible      := ShowSizes;
        seTx.RightEdge        := RightEdge;
        seRx.RightEdge        := RightEdge;
        seTx.TabWidth         := TabSize;
        seRx.TabWidth         := TabSize;

        { применение стиля новой строки, так хитро потому, что
          в уже созданного SynEdit невозможно изменить стиль напрямую }
        System.DefaultTextLineBreakStyle := LineBreakStyle;
        tmpSE      := TCustomSynEdit.Create(nil);
        tmpSE.Text := seTx.Text;
        seTx.ShareTextBufferFrom(tmpSE);
        FreeAndNil(tmpSE);

        serial.Hardflow           := Hardflow;
        serial.BreakDuration      := BreakTime;
        serial.DeadlockTimeout    := DeadlockTimeout;
        serial.CheckPort          := CheckPort;
        serial.TimestampStrBefore := TSBefore;
        serial.TimestampStrAfter  := TSAfter;
        serial.RxPacketTime       := TimeoutRx;
        serial.EnableRxTimestamp  := Timestamp;

        sbRTS.Enabled := not Hardflow;
        sbDTR.Enabled := not Hardflow;

        if ShowRightEdge then
          begin
          seTx.Options := seTx.Options - [eoHideRightMargin];
          seRx.Options := seRx.Options - [eoHideRightMargin];
          end
        else
          begin
          seTx.Options := seTx.Options + [eoHideRightMargin];
          seRx.Options := seRx.Options + [eoHideRightMargin];
          end;

        //if fmSettings <> nil then
        //  begin
        //  FormWindowStateChange(self);
        //  end;
        except
        if fmConfirm.Show(TXT_ERROR, WARN_SETTINGS, mbYesNo, self) = mrYes then
          Close;
        end;

    LanguageChange;
    actionViewGeneral(acShowTxBox);
    FormChangeBounds(Sender);
    seTxChange(Sender);
    seRxChange(Sender);
    EndFormUpdate;
  end;

// перевод интерфейса
procedure TfmMain.LanguageChange;
  begin
    fmSettings.LanguageChangeImmediately;

    EncodingsTxRxSet;   // обновляем список кодировок

    // обновляем подсказки
    acToggleRTS.Hint        := MultiString(HINT_RTS);
    acToggleDTR.Hint        := MultiString(HINT_DTR);
    acToggleBreak.Hint      := MultiString(HINT_BREAK);
    lbSignalCTS.Hint        := MultiString(HINT_CTS);
    lbSignalDSR.Hint        := MultiString(HINT_DSR);
    lbSignalRing.Hint       := MultiString(HINT_RING);
    lbSignalRLSD.Hint       := MultiString(HINT_RLSD);
    pSignalRTS.Hint         := MultiString(HINT_RTS);
    pSignalDTR.Hint         := MultiString(HINT_DTR);
    pSignalBreak.Hint       := MultiString(HINT_BREAK);
    pSignalCTS.Hint         := MultiString(HINT_CTS);
    pSignalDSR.Hint         := MultiString(HINT_DSR);
    pSignalRing.Hint        := MultiString(HINT_RING);
    pSignalRLSD.Hint        := MultiString(HINT_RLSD);
    acAutoSend.Hint         := MultiString(HINT_AUTOSEND);
    acAutoAnswerEnable.Hint := MultiString(HINT_AUTOANSWER);

    // обновляем выпадающие списки
    ComboBoxUpdateList(cbDataBits, [
      TXT_LIST_DATABITS_5, TXT_LIST_DATABITS_6,
      TXT_LIST_DATABITS_7, TXT_LIST_DATABITS_8]);

    ComboBoxUpdateList(cbParityBits, [
      TXT_LIST_PARBITS_N, TXT_LIST_PARBITS_O, TXT_LIST_PARBITS_E,
      TXT_LIST_PARBITS_M, TXT_LIST_PARBITS_S]);

    ComboBoxUpdateList(cbStopBits, [
      TXT_LIST_STOPBITS_1, TXT_LIST_STOPBITS_15, TXT_LIST_STOPBITS_2]);

    ComboBoxUpdateList(cbBaudrate, [
      '110', '300', '600', '1200', '2400', '4800', '9600',
      '14400', '19200', '38400', '56000', '57600', '115200',
      '128000', '230400', '256000', '460800', '921600', TXT_BR_OTHER]);

    ComboBoxUpdateList(fmChart.cbGraphDataType, [
      TXT_LIST_CHART_RAW_8, TXT_LIST_CHART_RAW_16,
      TXT_LIST_CHART_RAW_24, TXT_LIST_CHART_RAW_32,
      TXT_LIST_CHART_HEX, TXT_LIST_CHART_DEC]);

    sgTxSequences.Columns.Items[1].Title.Caption := TXT_DATA;

    UpdateControls(True);
  end;


initialization
  serial    := TSerialPortThread.Create;
  txSeqList := TSequencesList.Create;

finalization
  serial.Terminate;
  FreeAndNil(txSeqList);
end.
