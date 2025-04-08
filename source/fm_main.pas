unit fm_main;

{$mode objfpc}{$H+}

interface

uses
  ActnList, Buttons, Classes, Clipbrd, ComCtrls, Controls, Dialogs, ExtCtrls,
  Forms, Graphics, LazFileUtils, LazUTF8, LCLType, Menus,
  PairSplitter, Spin, StdCtrls, LCLIntf, Grids, ImageSVGList, StrUtils,
  SysUtils, Types, SynEdit, SynEditTypes, Math, DateUtils,
  AppTuner, AppLocalizer, AppSettings,

  // chart units
  TAGraph, TASeries, TAChartLiveView, TATools, TATypes, TANavigation,
  TALegend, TADrawUtils, TAGUIConnectorBGRA, TADrawerBGRA,

  // BGRA
  BGRABitmap, BGRABitmapTypes,

  // other forms
  fm_about, fm_commands, fm_confirm, fm_insertchar, fm_settings, fm_update,

  // project units
  u_encodings, u_serial, u_common, u_utilities, u_txsequences, u_settings_record,
  u_plotter, u_plotter_types, u_plotter_charts,

  // additional
  base64, csvdocument, appAbout;

const
  indicatorColor: array[Boolean] of TColor = ($4040FF, $00D000); // 0 RED; 1 GREEN
  indicatorText: array[Boolean] of String  = ('0', '1');         // 0; 1

type

  TLineParam = (lpNone, lpWidth, lpStyle, lpPoint);

  { TfmMain }

  TfmMain = class(TForm)
    {$INCLUDE i_main_controls.inc}

    { ***  Обработка событий главной формы  *** }

    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure pTranceiverMsgResize(Sender: TObject);
    procedure pSplitterTxRxResize(Sender: TObject);
    procedure psSplitterTxRxChangeBounds(Sender: TObject);
    procedure tiTrayIconClick(Sender: TObject);
    procedure tmrMain50msTimer(Sender: TObject);

    { ***  Обработчики приема и передачи данных  *** }

    procedure OnCommRxStart;
    procedure OnCommRxEnd;
    procedure OnCommTxStart;
    procedure OnCommTxEnd;

    { ***  Ввод/вывод данных  *** }

    procedure seTxChange(Sender: TObject);
    procedure seRxChange(Sender: TObject);
    procedure cbTxTypeChange(Sender: TObject);
    procedure cbRxTypeChange(Sender: TObject);
    procedure seTxRxMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);

    { ***  Управление портом  *** }

    procedure SetPortSettingsControls(Sender: TObject);
    procedure actionPortGeneral(Sender: TObject);

    { ***  Управление передатчиком  *** }

    procedure actionTxGeneral(Sender: TObject);
    procedure acInsertCharExecute(Sender: TObject);
    procedure acTxImportExecute(Sender: TObject);

    { ***  Управление приемником  *** }

    procedure actionRxGeneral(Sender: TObject);
    procedure acRxExportExecute(Sender: TObject);

    { ***  Управление видом  *** }

    procedure actionViewGeneral(Sender: TObject);
    procedure actionViewForm(Sender: TObject);

    { ***  Поиск текста  *** }

    procedure actionSearchGeneral(Sender: TObject);

    { ***  Команды общие  *** }

    procedure actionCommon(Sender: TObject);

    { ***  Плоттер  *** }

    procedure actionPlotter(Sender: TObject);

    procedure chToolPointHintHint(ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
    procedure chToolZoomXBeforeMouseWheelDown(ATool: TChartTool; APoint: TPoint);
    procedure chToolZoomXBeforeMouseWheelUp(ATool: TChartTool; APoint: TPoint);
    procedure chToolsetZoomYBeforeMouseWheel(ATool: TChartTool; APoint: TPoint);
    procedure chToolZoomDragBeforeMouseDown(ATool: TChartTool; APoint: TPoint);
    procedure chPlotterDrawLegend(ASender: TChart; ADrawer: IChartDrawer; ALegendItems: TChartLegendItems; ALegendItemSize: TPoint; const ALegendRect: TRect; AColCount, ARowCount: Integer);
    procedure cbPlotterPenStyleDrawItem(Control: TWinControl; Index: Integer; ARect: TRect; State: TOwnerDrawState);
    procedure chPlotterMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chPlotterMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure chPlotterMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure PlotterParserInit;
    procedure PlotterParserReInit;
    procedure PlotterParserOnParseDone(ACharts: TPlotterChartsList);
    procedure PlotterParserOnCommand(ACommand: String; AValue: Double; AHexLen: Integer);
    procedure PlotterExportCSV;
    procedure PlotterExportImage;
    procedure PlotterLineStyleVerify(AForceDisable: Boolean = False);
    procedure PlotterSetLinesStyle(AValue: TPlotterPenStyle);
    procedure PlotterSetLinesWidth(AValue: Integer);
    procedure PlotterSetLinePointSize(AValue, AIndex: Integer);
    procedure PlotterSetPointSize(AValue: Integer);
    function PlotterGetLegendClickedIndex(X, Y: Integer): Integer;

    { ***  Таблица сохраненных сообщений для передачи  *** }

    procedure sgTxSequencesChangeBounds(Sender: TObject);
    procedure sgTxSequencesPrepareCanvas(Sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
    procedure sgTxSequencesUpdate;
    procedure sgTxSequencesMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure sgTxSequencesMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sgTxSequencesUTF8KeyPress(Sender: TObject; var UTF8Key: TUTF8Char);
    procedure sgTxSequencesEditingDone(Sender: TObject);
    procedure sgTxSequencesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);

    { ***  Сервисные методы  *** }

    procedure UpdateControls(AForceUpdate: Boolean = False);
    procedure UpdateIndicators;
    procedure UpdatePanelsLayout;
    procedure UpdateSelectionInfo(ALabel: TLabel; ASynEdit: TSynEdit; AEncoding: String; InHEX: Boolean = False);

    procedure EncodingsTxRxSet;
    function GetEditorCursorPosition(ASynEdit: TSynEdit; AEncoding: String; InHEX: Boolean = False): Integer;
    function GetEditorSelectionSize(ASynEdit: TSynEdit; AEncoding: String; InHEX: Boolean = False): Integer;

    procedure AdjustComponentSizes;
    procedure AdjustThemeDependentValues;

    { ***  Settings and language  *** }

    procedure BeforeSaveConfig;
    procedure AfterLoadConfig;
    procedure acResetExecute(Sender: TObject);
    procedure SettingsApply(Sender: TObject = nil);
    procedure OnLanguageChange(Sender: TObject);

  private
    FWSPrevious:    TWindowState;
    FLastMaxCh:     Integer;       // initial lines padding in window view of plotter
    FMouseDownPos:  TPoint;        // mouse coords when left button clicks
    FInactiveColor: Integer;       // RS-232 inactive led color
    FRedrawBoxes:   Boolean;       // flag to start redraw i/o boxes

    FLineSerie:      array [0..MAX_SERIES - 1] of TLineSeries; // линии плоттера
    FLineLZone:      array [0..MAX_SERIES - 1] of TRect;       // активные зоны меток линий
    FPlotterRedefLb: array [0..10] of TLabel;                  // метки переопределенных значений плоттера
  end;

var
  fmMain:       TfmMain;
  portList:     TStringList;         // список доступных портов без метки "занят"
  serial:       TSerialPortThread;   // класс-поток для работы с портом
  txSeqList:    TSequencesList;      // класс-хранитель списка сохраненных сообщений для передачи
  plotter:      TPlotterParser;      // класс-парсер для плоттера
  tx, rx:       String;              // буферы ввода и вывода
  txSequence:   Boolean = False;     // флаг запуска передачи выбранного сбщ из списка сохраненных
  lineSelected: Integer = -1;        // выбранная курсором линия плоттера
  lineParam:    TLineParam = lpNone; // параметр линии для изменения жестом

implementation

{$R *.lfm}

 { TfmMain }

 { ***  Обработка событий главной формы  *** }

 // инициализация
procedure TfmMain.FormCreate(Sender: TObject);

  procedure AddPortSettingsSubMenu;
    var
      i:     Integer;
      miSub: array of TMenuItem;
      miNew: TMenuItem;
    begin
      miNew        := TMenuItem.Create(MenuItem1);
      miNew.Action := acPortSettingsMenu;
      MenuItem1.Insert(5, miNew);

      SetLength(miSub, 0);
      for i := 0 to pmPortSettings.Items.Count - 1 do
        if pmPortSettings.Items.Items[i].Caption = '-' then
          miNew.AddSeparator
        else
          begin
          SetLength(miSub, Length(miSub) + 1);
          miSub[High(miSub)]        := TMenuItem.Create(miNew);
          miSub[High(miSub)].Action := pmPortSettings.Items.Items[i].Action;
          miNew.Add(miSub[High(miSub)]);
          end;
    end;

  begin
    serial.Start;
    serial.OnRxStart := @OnCommRxStart;
    serial.OnRxEnd   := @OnCommRxEnd;
    serial.OnTxStart := @OnCommTxStart;
    serial.OnTxEnd   := @OnCommTxEnd;

    PlotterParserInit;
    AddPortSettingsSubMenu;
    EncodingsTxRxSet;

    // default form size
    Width  := Scale96ToScreen(700);
    Height := Scale96ToScreen(450);

    appLocalizerEx.AddOnLanguageChangeHandler(@OnLanguageChange);

    // add settings related to fm_main
    {$Define inc_fm_main}
    {$Include i_config.inc}
  end;

// появление формы главного окна на экране
procedure TfmMain.FormShow(Sender: TObject);
  var
    _comp: TObject;
  begin
    tiTrayIcon.Visible    := True;
    tiTrayIcon.Icon       := Application.Icon;
    lbHelpHint.Caption    := TXT_HELP_HINT;
    lbTxPosAndSel.Caption := '';
    lbRxPosAndSel.Caption := '';
    OnShow                := nil;       // выкл. обработчик, нужен только при запуске
    Position              := poDefault; // чтобы не менялась позиция окна при разворачивании из трея

    appLocalizerEx.EnumerateComponents;

    with appTunerEx do
      begin
      AddAllForms;
      Form[Self].SaveProps := True; // save/restore props only for main form 
      Hide;
      LoadProperties;
      Form[Self].AllowDrag := True;

      // load property values to controls
      acShowOnTop.Checked  := Form[Self].StayOnTop;
      acShowBorder.Checked := not Form[Self].Borderless;
      end;

    AfterLoadConfig;
    SettingsApply;

    // предустановка состояний элементов
    actionPortGeneral(acRxEnable);

    for _comp in [acPlotterShow, acPlotterSettings, acPlotterQLiveMode,
        acPlotterQTracker, Sender] do
      actionPlotter(_comp);

    for _comp in [acShowLineCounts, acShowHEX, acShowSignals,
        acShowTBMain, acShowTBPort, acShowTBTx, acShowTBRx] do
      actionViewGeneral(_comp);

    for _comp in [acSearch, rbSearchTx] do
      actionSearchGeneral(_comp);

    actionViewForm(Sender);

    // автоподключение к порту
    if cfg.connect.auto and cfg.connect.opened then acConnect.Execute;

    // начальная инициализация
    acScan.Execute;
    SetPortSettingsControls(Sender);
    seAutoSendTime.Hide;

    FormChangeBounds(nil);
    Show;
  end;

// изменение состояния главного окна (свернуто, нормально, развернуто)
procedure TfmMain.FormChangeBounds(Sender: TObject);
  begin
    if OnShow <> nil then Exit;

    if WindowState <> wsMinimized then
      FWSPrevious := WindowState;

    // option: minimize to tray
    if cfg.com.tray and (WindowState = wsMinimized) then
      tiTrayIconClick(Sender);

    stStatusBar.Panels.Items[0].Width := Width - stStatusBar.Height -
      Canvas.GetTextWidth(' COM00, 0-N-0, 000000 ' + TXT_SPEED);

    // контроль стилизации линий плоттера для уменьшения лага прорисовки
    if acPlotterShow.Checked then PlotterLineStyleVerify;

    FRedrawBoxes := True;
  end;

// действие при попытке закрыть приложение
procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
    if fmUpdate.IsDownloading then
      CanClose := fmConfirm.Show(TXT_WARNING, WARN_UPDATE, mbYesNo, Self) = mrYes;

    if CanClose then
      begin
      Settings.SyncValues;
      BeforeSaveConfig;
      Settings.Save;
      Hide; // to prevent form flickering
      appTunerEx.Form[Self].MenuShow := False;
      appTunerEx.Form[Self].MenuTune := False;
      appTunerEx.SaveProperties;
      end;
  end;

procedure TfmMain.FormDropFiles(Sender: TObject; const FileNames: array of String);
  begin
    dlgTxOpen.FileName := FileNames[0];
    acTxImportExecute(nil);
  end;

procedure TfmMain.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    appTunerEx.Form[Self].ProcessMouseDown(X, Y);
  end;

procedure TfmMain.FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
    appTunerEx.Form[Self].ProcessMouseUp(X, Y);
  end;

procedure TfmMain.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  begin
    appTunerEx.Form[Self].ProcessMouseMove(X, Y);
  end;


procedure TfmMain.pTranceiverMsgResize(Sender: TObject);
  begin
    BeginFormUpdate;
    pTranceiver.Constraints.MaxHeight := pTranceiverMsg.Height;
    pTranceiver.Constraints.MaxWidth  := pTranceiverMsg.Width;
    EndFormUpdate;
  end;

procedure TfmMain.pSplitterTxRxResize(Sender: TObject);
  begin
    with psSplitterTxRx do
      if Showing then
        cfg.com.splitter := Position /
          (cfg.com.layout in [plTxTop, plTxDown]).Select(Height, Width);

    psSplitterTxRx.OnChangeBounds := @psSplitterTxRxChangeBounds;
  end;

procedure TfmMain.psSplitterTxRxChangeBounds(Sender: TObject);
  var
    _size: Integer;
  begin
    with psSplitterTxRx do
      begin
      if (cfg.com.splitter < 0) or (cfg.com.splitter > 1) then
        cfg.com.splitter := 0.5;

      _size    := (cfg.com.layout in [plTxTop, plTxDown]).Select(Height, Width);
      Position := round(_size * cfg.com.splitter);
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
  var
    etx, erx: String;
  begin
    BeginFormUpdate;

    UpdateControls;
    UpdateIndicators;

    // update position and selection values
    if cfg.editor.view.pos then
      begin
      etx := GetEncodingByIndex(cbTxEncoding.ItemIndex);
      erx := GetEncodingByIndex(cbRxEncoding.ItemIndex);
      UpdateSelectionInfo(lbTxPosAndSel, seTx, etx);
      UpdateSelectionInfo(lbRxPosAndSel, seRx, erx);
      UpdateSelectionInfo(lbTxPosAndSel, seTxHex, etx, True);
      UpdateSelectionInfo(lbRxPosAndSel, seRxHex, erx, True);
      end;

    // перерисовка полей в/в при необходимости
    if tmr1s > 0 then Dec(tmr1s) else
      begin
      if FRedrawBoxes then
        begin
        tmr1s        := 1000 div tmrMain50ms.Interval;
        FRedrawBoxes := False;
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

      // send data to plotter
      if acShowRxBox.Checked and acPlotterShow.Checked then
        plotter.Input := serial.DataRx;

      // простой автоответ на вх. последовательность
      if acAutoAnswerEnable.Checked and not acPlotterShow.Checked then
        begin
        index := fmCommands.SequenceList.IndexOf(serial.DataRx);
        if index >= 0 then
          serial.TransmitAnswer(UTF8ToEncodingByIndex(
            fmCommands.AnswerList.Strings[index], cbRxEncoding.ItemIndex));
        end;

      // add received data to buffer
      if not acPlotterShow.Checked or cfg.plt.copyRx then
        begin
        rx += serial.DataRx;

        // ограничение объема данных в буфере приема
        if (cfg.rx.limit > 0) and (rx.Length > cfg.rx.limit) then
          rx := rx.Remove(0, rx.Length - cfg.rx.limit);

        seRxChange(nil);
        end;
      end;
  end;

procedure TfmMain.OnCommTxStart;
  begin
    if not txSequence then
      serial.DataTx := tx
    else
      serial.DataTx := txSeqList.Data[sgTxSequences.Selection.Top - 1];

    acTxSend.Enabled := False;
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
      case TDataView(cbTxType.ItemIndex) of
        dvTxt:
          begin
          x       := (Lines.TextLineBreakStyle = tlbsCRLF).ToInteger + 1;
          tx      := UTF8ToEncodingByIndex(Text.Remove(Text.Length - x, x), cbTxEncoding.ItemIndex);
          end;
        dvHex: tx := Text.FromToCodes(16);
        dvBin: tx := Text.FromToCodes(2);
        dvDec: tx := Text.FromToCodes(10);
        end;

    if seTxHex.Visible then
      begin
      seTxHex.BeginUpdate;
      seTxHex.Text     := tx.ToHex(cfg.editor.hex.line, cfg.editor.hex.block);
      seTxHex.SelStart := Length(seTxHex.Text);
      seTxHex.EndUpdate;
      end;

    if lbTxSize.Visible then
      if cfg.editor.view.inBytes then
        lbTxSize.Caption := tx.Length.ToString + ' ' + TXT_BYTE_SHORT
      else
        lbTxSize.Caption := tx.Length.SizeInBytes(
          TXT_BYTE_SHORT, TXT_BYTE_KB, TXT_BYTE_MB, TXT_BYTE_GB, False);
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
          Text   := AText + LE.Select(LineEnding, '');  // SetFocus;
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
          Text   := AText + LE.Select(LineEnding, '');
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

    case TDataView(cbRxType.ItemIndex) of
      dvTxt: rxUTF := EncodingToUTF8ByIndex(rx, cbRxEncoding.ItemIndex);
      dvHex: rxUTF := rx.ToCodes(16, seRx.CharsInWindow);
      dvBin: rxUTF := rx.ToCodes(2, seRx.CharsInWindow);
      dvDec: rxUTF := rx.ToCodes(10, seRx.CharsInWindow);
      end;

    if acShowRxBox.Checked then
      begin
      SynEditUpdate(acRxScrollToEnd.Checked, seRx, rxUTF, True);

      if seRxHex.Visible then
        SynEditUpdate(acRxScrollToEnd.Checked, seRxHex,
          rx.ToHex(cfg.editor.hex.line, cfg.editor.hex.block));
      end;

    if lbRxSize.Visible then
      if cfg.editor.view.inBytes then
        lbRxSize.Caption := rx.Length.ToString + ' ' + TXT_BYTE_SHORT
      else
        lbRxSize.Caption := rx.Length.SizeInBytes(
          TXT_BYTE_SHORT, TXT_BYTE_KB, TXT_BYTE_MB, TXT_BYTE_GB, False);
  end;

procedure TfmMain.cbTxTypeChange(Sender: TObject);
  begin
    txSeqList.Encoding := GetEncodingByIndex(cbTxEncoding.ItemIndex);

    seTx.BeginUpdate;
    case TDataView(cbTxType.ItemIndex) of
      dvTxt: seTx.Text := EncodingToUTF8ByIndex(tx, cbTxEncoding.ItemIndex) +
          (tx.Length > 0).Select(LineEnding, '');
      dvHex: seTx.Text := tx.ToCodes(16, seTx.CharsInWindow);
      dvBin: seTx.Text := tx.ToCodes(2, seTx.CharsInWindow);
      dvDec: seTx.Text := tx.ToCodes(10, seTx.CharsInWindow);
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

        'seTx':
          if appTunerEx.IsDarkTheme then
            cfg.tx.fontdark.size := seTx.Font.Size
          else
            cfg.tx.font.size     := seTx.Font.Size;

        'seRx':
          if appTunerEx.IsDarkTheme then
            cfg.rx.fontdark.size := seRx.Font.Size
          else
            cfg.rx.font.size     := seRx.Font.Size;
        end;

      sgTxSequencesChangeBounds(Sender);
      Handled := True;
      end;
  end;


 { ***  Управление портом  *** }

 // настройки трансивера UART
procedure TfmMain.SetPortSettingsControls(Sender: TObject);
  var
    baudrateValue:      Integer;
    dataBits, stopBits: Integer;
    parityBit:          char;
  begin
    if cbBaudrate.ItemIndex = cbBaudrate.Items.Count - 1 then
      begin
      baudrateValue            := seBaudRateCustom.Value;
      seBaudRateCustom.Visible := True;
      end
    else
      begin
      baudrateValue            := StrToIntDef(cbBaudrate.Text, 9600);
      seBaudRateCustom.Visible := False;
      end;

    // default values: 8N1
    dataBits  := 8;
    parityBit := 'N';
    stopBits  := 0;

    // set data bits
    if acPortDB5.Checked then dataBits := 5 else
    if acPortDB6.Checked then dataBits := 6 else
    if acPortDB7.Checked then dataBits := 7 else
      acPortDB8.Checked                := True;  // if unchecked all set default

    // set parity bits
    if acPortPBE.Checked then parityBit := 'E' else
    if acPortPBO.Checked then parityBit := 'O' else
    if acPortPBM.Checked then parityBit := 'M' else
    if acPortPBS.Checked then parityBit := 'S' else
      acPortPBN.Checked                 := True; // if unchecked all set default

    // set stop bits
    if acPortSB1h.Checked then stopBits := 1 else
    if acPortSB2.Checked then stopBits  := 2 else
      acPortSB1.Checked                 := True; // if unchecked all set default

    // set new settings of port
    if (Sender <> nil) and (cbPortsList.ItemIndex in [0..portList.Count]) then
      serial.PortSettings(portList[cbPortsList.ItemIndex],
        baudrateValue, dataBits, parityBit, stopBits);

    UpdateControls(True);
  end;

procedure TfmMain.actionPortGeneral(Sender: TObject);
  begin
    case TAction(Sender).Name of

      // получение списка доступных портов
      'acScan':
        begin
        serial.CheckPort            := False;
        portList.CommaText          := serial.GetExistingPorts;
        serial.CheckPort            := cfg.connect.check;
        cbPortsList.Items.CommaText := serial.GetExistingPorts;
        cbPortsList.ItemIndex       := serial.GetPortIndexInList;
        if cbPortsList.ItemIndex < 0 then cbPortsList.ItemIndex := 0;

        appTunerEx.Form[Self].TuneComboboxes;

        stStatusBar.Panels.Items[1].Text :=
          Format(TXT_PORTS_FINDED, [cbPortsList.Items.Count]);
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
      'acTxSend':
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
        case TDataView(cbTxType.ItemIndex) of
          dvTxt: inserted_char :=
              EncodingToUTF8ByIndex(chr(SelectedChar), cbTxEncoding.ItemIndex);
          dvHex: inserted_char := SelectedChar.ToHexString(2);
          dvBin: inserted_char := intToBin(SelectedChar, 8);
          dvDec: inserted_char := SelectedChar.ToString;
          end;

        select_start := seTx.SelStart - 1;
        tmp          := seTx.Text;
        txt_length   := tmp.Length;

        if TDataView(cbTxType.ItemIndex) <> dvTxt then
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
      dlgTxOpen.FileName := ExtractFileName(dlgTxOpen.FileName);
      if not dlgTxOpen.Execute then Exit;
      end;

    if not cfg.tx.addition and cfg.tx.loadWarn and (tx <> '') then
      if (fmConfirm.Show(TXT_WARNING, WARN_LOAD, mbYesNo, Self) = mrNo) then Exit;

    with TStringList.Create do
      begin
      LoadFromFile(dlgTxOpen.FileName);
      if cfg.tx.addition then
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
      end;
  end;

procedure TfmMain.acRxExportExecute(Sender: TObject);
  const
    lastName: String = ''; // static var
  begin
    dlgRxSave.Filter   := Format('%s|*.txt|%s|*.*', [TXT_DLG_TEXT, TXT_DLG_ALL]);
    dlgRxSave.FileName := (lastName <> '').Select(lastName, 'buffer_rx');

    if dlgRxSave.Execute then
      begin

      with TStringList.Create do
        begin
        Text := rx;
        SaveToFile(dlgRxSave.FileName);
        Free;
        end;

      lastName := ExtractFileName(dlgRxSave.FileName);
      end;
  end;


{ ***  Управление видом  *** }
procedure TfmMain.actionViewGeneral(Sender: TObject);
  var
    _viewNothing: Boolean;
  begin
    FMouseDownPos.Create(Left, Top);
    BeginFormUpdate;
    case TAction(Sender).Name of

      // вкл/выкл панель сигналов порта
      'acShowSignals':
        pSignals.Visible := acShowSignals.Checked;

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
        seRxHex.Visible      := acShowHEX.Checked;
        spSplitterRx.Visible := acShowHEX.Checked;
        nbTxRight.Visible    := acShowHEX.Checked or acTxSequences.Checked;
        spSplitterTx.Visible := nbTxRight.Visible;
        nbTxRight.PageIndex  := acTxSequences.Checked.ToInteger;

        seRxChange(Sender);
        seTxChange(Sender);
        end;

      // вкл/выкл видимость полей в/в
      'acShowTxBox', 'acShowRxBox':
        begin
        pRxBox.Visible         := acShowRxBox.Checked;
        pTxBox.Visible         := acShowTxBox.Checked;
        _viewNothing           := not (pRxBox.Visible or pTxBox.Visible);
        psSplitterTxRx.Visible := pRxBox.Visible and pTxBox.Visible;
        lbHelpHint.Visible     := _viewNothing;

        if _viewNothing and acSearch.Checked then acSearch.Execute;

        UpdatePanelsLayout;
        end;

      // управление видимостью главной панели инструментов
      'acShowTBMain':
        begin
        pToolbar.Visible     := acShowTBMain.Checked;
        pMainToolbar.Visible := acShowTBMain.Checked or pPortSettings.Visible;
        end;

      // управление видимостью панели инструментов порта
      'acShowTBPort':
        begin
        pPortSettings.Visible := acShowTBPort.Checked;
        pMainToolbar.Visible  := acShowTBPort.Checked or pToolbar.Visible;
        end;

      // управление видимостью панели инструментов передатчика
      'acShowTBTx':
        pTxToolbar.Visible := acShowTBTx.Checked;

      // управление видимостью панели инструментов приемника
      'acShowTBRx':
        pRxToolbar.Visible := acShowTBRx.Checked;
      end;

    acTxSeqAdd.Enabled    := acShowTxBox.Checked;
    acTxSequences.Enabled := acShowTxBox.Checked;

    FormChangeBounds(Sender);
    UpdateControls(True);
    EndFormUpdate;
    sgTxSequencesChangeBounds(nil);
  end;

procedure TfmMain.actionViewForm(Sender: TObject);
  begin
    case TComponent(Sender).Name of

      // control visibility of form border
      'acShowBorder':
        begin
        appTunerEx.Form[Self].Borderless := not acShowBorder.Checked;
        end;

      // form show on top of other windows
      'acShowOnTop':
        appTunerEx.Form[Self].StayOnTop := acShowOnTop.Checked;
      end;

    acExitEx.Visible    := not acShowBorder.Checked;
    acShowOnTop.Enabled := acShowBorder.Checked or (WindowState <> wsMaximized);
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
      'rbSearchTx', 'rbSearchRx':
        begin
        BeginFormUpdate;
        lbSearch.Constraints.MinWidth  := Max(lbSearch.Width, lbReplace.Width);
        lbReplace.Constraints.MinWidth := Max(lbSearch.Width, lbReplace.Width);

        cbSearchReplace.Visible    := rbSearchTx.Checked;
        cbSearchReplaceAll.Visible := rbSearchTx.Checked;
        edReplace.Visible          := rbSearchTx.Checked;
        lbReplace.Visible          := rbSearchTx.Checked;
        lbSearchSpace1.Visible     := rbSearchTx.Checked;
        lbSearchSpace2.Visible     := rbSearchTx.Checked;
        rbSearchRx.Checked         := not rbSearchTx.Checked;
        EndFormUpdate;
        end;

      // команда запуска поиска/замены
      'acSearchNext', 'acSearchPrev':
        begin
        se := TSynEdit(rbSearchTx.Checked.Select(seTx, seRx));
        if not se.CanFocus then Exit;
        if cbSearchCaseSens.Checked then option    += [ssoMatchCase];
        if senderName = 'acSearchPrev' then option += [ssoBackwards];
        if cbSearchRegex.Checked then option       += [ssoRegExpr];

        se.SetFocus;
        se.SelStart := (senderName = 'acSearchPrev').Select(se.SelStart, se.SelEnd);

        if rbSearchTx.Checked and cbSearchReplace.Checked then
          if cbSearchReplaceAll.Checked then
            begin
            option      += [ssoReplaceAll];
            se.SelStart := 1;
            end
          else
            option      += [ssoReplace];

        se.SearchReplace(edSearch.Text, edReplace.Text, option);
        end;

      end;
  end;


{ ***  Команды общие  *** }
procedure TfmMain.actionCommon(Sender: TObject);
  begin
    case TAction(Sender).Name of

      // завершение работы приложения
      'acExit', 'acExitEx':
        Close;

      // открыть окно настроек приложения
      'acSettings':
        begin
        plotter.Pause := True;  // pause the plotter immediatelly
        fmSettings.ShowModal;   // show settings window
        SettingsApply;
        end;

      // вызов справки html
      'acHelp':
        OpenURL('..' + DirectorySeparator + FILE_HELP.Replace('.md', '.html'));

      // вызов справки markdown
      'acHelpMD':
        OpenURL('..' + DirectorySeparator + FILE_HELP);

      // вызов справки онлайн
      'acHelpNet':
        OpenURL(APP_URL_HELP);

      // link to online homepage
      'acWebsite':
        OpenURL(APP_URL_HOME);

      // окно информации о приложении
      'acInfo':
        begin
        fmAbout.FormStyle := FormStyle;
        fmAbout.Show;
        end;

      // show update app form
      'acAppUpdate', 'acUpdateGo':
        fmUpdate.ShowModal;

      // cancel showing update app notification
      'acUpdateCancel':
        fmUpdate.Later;

      end;
  end;


{ ***  Плоттер  *** }

procedure TfmMain.actionPlotter(Sender: TObject);
  var
    i: Integer;
  begin
    BeginFormUpdate;

    case TComponent(Sender).Name of

      // reset plotter state (used on start-up in FormShow() )
      'fmMain':
        begin
        plotter.OnParseDone := nil;

        PlotterParserReInit;
        plotter.Reset;

        cbPlotterRegExp.Visible := plotter.Protocol = ppRegExp;

        // clear and hide labels for redefined values
        pPlotterRedef.Hide;
        for i := Low(FPlotterRedefLb) to High(FPlotterRedefLb) do
          FPlotterRedefLb[i].Caption := '';

        for i := Low(FLineSerie) to High(FLineSerie) do
          begin
          FLineSerie[i].Clear;
          FLineSerie[i].Active := FLineSerie[i].Active or cfg.plt.reactivate;
          FLineLZone[i]        := Rect(-1, -1, -1, -1);

          if cfg.plt.recolor then
            begin
            FLineSerie[i].SeriesColor         := cfg.plt.color.line[i];
            FLineSerie[i].Pointer.Brush.Color := cfg.plt.color.line[i];
            end;
          end;

        // here we restore some settings that
        // may have been changed by the commands ...
        chPlotter.BottomAxis.Grid.Visible := cfg.ax.grid;
        chPlotter.LeftAxis.Grid.Visible   := cfg.ax.grid;

        // ... and restore colors
        AdjustThemeDependentValues;

        actionPlotter(sePlotterPenSize);
        actionPlotter(sePlotterPoints);
        actionPlotter(cbPlotterPenStyle);
        plotter.OnParseDone := @PlotterParserOnParseDone;
        end;

      'acPlotterClear':
        begin
        if cfg.plt.clearRx then
          acRxClear.Execute;
        actionPlotter(fmMain);
        end;

      // показать/закрыть плоттер
      'acPlotterShow':
        if acPlotterShow.Checked then
          begin
          if not acShowRxBox.Checked then acShowRxBox.Execute;
          actionPlotter(acPlotterSettings);
          pgRxPlotter.Show;
          seRxChange(nil);
          end
        else
          begin
          lbRxSize.Visible    := True;
          tbPlotter.Visible   := False;
          tbPlotterEx.Visible := False;
          pgRxText.Show;
          seRxChange(nil);
          end;

      // закрыть плоттер
      'acPlotterClose':
        acPlotterShow.Execute;

      'acPlotterSettings':
        PlotterParserReInit;

      'acPlotterTracker':
        acPlotterQTracker.Execute;

      'acPlotterQTracker':
        begin
        acPlotterTracker.Checked   := acPlotterQTracker.Checked;
        chToolPointTracker.Enabled := acPlotterQTracker.Checked;
        chToolPointHint.Enabled    := acPlotterQTracker.Checked;
        end;

      'acPlotterLiveMode':
        acPlotterQLiveMode.Execute;

      'sePlotterViewport', 'acPlotterQLiveMode':
        begin
        acPlotterLiveMode.Checked := acPlotterQLiveMode.Checked;
        if plotter.View = pvSweep then
          actionPlotter(acPlotterClear)
        else
          PlotterParserReInit;
        end;

      'cbPlotterPenStyle':
        PlotterSetLinesStyle(TPlotterPenStyle(cbPlotterPenStyle.ItemIndex));

      'sePlotterPenSize':
        PlotterSetLinesWidth(sePlotterPenSize.Value);

      'sePlotterPoints':
        PlotterSetPointSize(sePlotterPoints.Value);

      'cbPlotterProtocol':
        if TPlotterProtocol(cbPlotterProtocol.ItemIndex) <> plotter.Protocol then
          actionPlotter(acPlotterClear);

      'cbPlotterRegExp':
        with cbPlotterRegExp do
          if ItemIndex <> Tag then
            begin
            Tag := ItemIndex;
            actionPlotter(acPlotterClear);
            end;

      'cbPlotterView':
        if TPlotterView(cbPlotterView.ItemIndex) <> plotter.View then
          actionPlotter(acPlotterClear);

      'acPlotterExpCSV', 'acPlotterQExpCSV':
        PlotterExportCSV;

      'acPlotterExpImg', 'acPlotterQExpImg':
        PlotterExportImage;

      end;

    UpdateControls(True);
    EndFormUpdate;
  end;


procedure TfmMain.chToolPointHintHint(
  ATool: TDataPointHintTool; const APoint: TPoint; var AHint: String);
  var
    serie: TLineSeries;
    x, y:  Double;
    t:     String;
  begin
    serie := ATool.Series as TLineSeries;
    t     := serie.Title.Trim;
    x     := serie.XValue[ATool.PointIndex];
    y     := serie.YValue[ATool.PointIndex];

    // подсказка под перекрестием
    AHint := Format('%8s%s ' + LineEnding + ' X = %f ' + LineEnding + ' Y = %f ', ['', t, x, y]);

    // подсказка в статусной строке
    if cfg.com.status then
      stStatusBar.Panels[1].Text := Format('%s: %f  %f', [t, x, y]);
  end;

procedure TfmMain.chToolZoomXBeforeMouseWheelDown(
  ATool: TChartTool; APoint: TPoint);
  begin
    // контроль стилизации линий для уменьшения лага прорисовки
    PlotterLineStyleVerify;

    if acPlotterQLiveMode.Checked then
      with sePlotterViewport do
        if plotter.View = pvSweep then
          Value := Value + Increment
        else
          Value := Value * 2;
  end;

procedure TfmMain.chToolZoomXBeforeMouseWheelUp(
  ATool: TChartTool; APoint: TPoint);
  begin
    // контроль стилизации линий для уменьшения лага прорисовки
    PlotterLineStyleVerify;

    if acPlotterQLiveMode.Checked then
      with sePlotterViewport do
        if plotter.View = pvSweep then
          Value := Value - Increment
        else
        if Value >= 2 * Increment then
          Value := Value div 2;
  end;

procedure TfmMain.chToolsetZoomYBeforeMouseWheel(
  ATool: TChartTool; APoint: TPoint);
  begin
    // контроль стилизации линий для уменьшения лага прорисовки
    PlotterLineStyleVerify;

    if chLiveView.Active then
      chLiveView.ExtentY := lveLogical;
  end;

procedure TfmMain.chToolZoomDragBeforeMouseDown(ATool: TChartTool;
  APoint: TPoint);
  begin
    // отключить стилизации линий для уменьшения лага прорисовки
    PlotterLineStyleVerify(True);

    if chLiveView.Active then
      chLiveView.ExtentY := lveAuto;
  end;

procedure TfmMain.chPlotterDrawLegend(ASender: TChart; ADrawer: IChartDrawer;
  ALegendItems: TChartLegendItems; ALegendItemSize: TPoint;
  const ALegendRect: TRect; AColCount, ARowCount: Integer);
  var
    i, x, d, ex, ey, t, l, dx, h, y, xw, iw: Integer;
    f: TFont;
    s: String;
  begin
    if ALegendItems.Count = 0 then Exit;

    f := TFont.Create;
    f.Assign(ASender.Legend.Font);
    ADrawer.SetFont(f);

    dx := 0;
    d  := 0;
    l  := ALegendRect.Left;
    t  := ALegendRect.Top;
    h  := ADrawer.TextExtent('0').Y;
    xw := cfg.legend.style.Select(2 * h, round(0.7 * h));

    for i := Low(FLineSerie) to High(FLineSerie) do
      with FLineSerie[i] do
        begin
        if Count = 0 then continue; // пропуск пустых линий

        // пропуск линий без метки, если задана настройка
        if not cfg.legend.untitled and plotter.Charts[i].DefaultCaption then break;

        s := cfg.legend.index.Select((i + 1).ToString + ': ', '')
          + Title.Trim;

        if Pointer.HorizSize > 0 then
          d := Pointer.HorizSize * 2 + 1;

        x  := l + dx;
        iw := ADrawer.TextExtent(s).X;

        if x + xw + iw + 10 > chPlotter.Width - tbPlotter.Width then
          begin
          t  += h + 4;
          x  := l;
          dx := 0;
          end;

        y  := t + h div 2;
        ex := x + h - d div 2 + 4;
        ey := y - d div 2;

        FLineLZone[i] := Rect(x + 4, t + 1, x + xw + iw + 8, t + h);

        // draw frame
        if cfg.legend.frame then
          begin
          if cfg.legend.coloredframe then
            ADrawer.SetPenParams(LinePen.Style, SeriesColor)
          else
            ADrawer.SetPenParams(psSolid, chPlotter.Legend.Font.Color);
          ADrawer.SetBrushParams(bsSolid, chPlotter.BackColor);
          ADrawer.Rectangle(FLineLZone[i]);
          end;

        ADrawer.SetPenParams(LinePen.Style, SeriesColor);

        if Active then
          if cfg.legend.style then
            begin
            // draw line example
            ADrawer.SetPenWidth(LinePen.Width);
            ADrawer.SetBrushParams(bsClear, SeriesColor);
            ADrawer.Line(x + 8 + LinePen.Width div 2, y, x + xw - LinePen.Width div 2, y);

            // draw dots
            ADrawer.SetBrushParams(bsSolid, SeriesColor);
            ADrawer.SetPenParams(psSolid, clBlack);
            ADrawer.Ellipse(ex, ey, ex + d, ey + d);
            end
          else
            begin
            // simple rectangle with color of line
            ADrawer.SetPenWidth(1);
            ADrawer.SetBrushParams(bsSolid, SeriesColor);
            if cfg.legend.frame and not cfg.legend.coloredframe then
              ADrawer.Rectangle(x + 5, t + 2, x + xw, t + h - 1)
            else
              ADrawer.Rectangle(x + 4, t + 1, x + xw, t + h);
            end;

        // draw label
        if cfg.legend.colored then f.Color := SeriesColor;
        ADrawer.SetFont(f);
        ADrawer.SetBrushParams(bsClear, clWhite);
        ADrawer.TextOut.Text(s).Pos(x + xw + 4, t - 1).Done;

        dx += xw + iw + 8;
        end;

    f.Free;
  end;

procedure TfmMain.cbPlotterPenStyleDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
  var
    y: LongInt;
  begin
    // обработчик для отрисовки стиля линий вместо текстового описания

    y := (ARect.Bottom + ARect.Top) div 2;

    with cbPlotterPenStyle.Canvas do
      begin
      Pen.Width := 1;
      Pen.Color := Brush.Color;
      Pen.Style := psSolid;
      Rectangle(ARect);      // выделение

      Pen.Width := sePlotterPenSize.Value;
      Pen.Color := Font.Color;
      Pen.Style := PLOTTER_PEN_STYLE[TPlotterPenStyle(Index)];
      Line(ARect.Left + Pen.Width, y, ARect.Right - Pen.Width, y);
      end;
  end;

procedure TfmMain.chPlotterMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
    if not cfg.legend.interactive then Exit;

    FMouseDownPos := Point(X, Y);
    lineSelected  := PlotterGetLegendClickedIndex(X, Y);

    if ssCtrl in Shift then
      lineParam := lpStyle
    else
    if ssAlt in Shift then
      lineParam := lpPoint
    else
      lineParam := lpWidth;
  end;

procedure TfmMain.chPlotterMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
  begin
    if not cfg.legend.interactive then Exit;

    pPlotterHint.Visible := False;
    lineSelected         := PlotterGetLegendClickedIndex(X, Y);
    if lineSelected < 0 then Exit;

    case Button of
      mbLeft:      // ЛКМ - вкл/выкл видимость линии
        FLineSerie[lineSelected].Active := not FLineSerie[lineSelected].Active;

      mbRight:     // ПКМ - задать свой цвет линии
        begin
        dlgLineColor.Color := FLineSerie[lineSelected].SeriesColor;
        if dlgLineColor.Execute then
          begin
          FLineSerie[lineSelected].SeriesColor         := dlgLineColor.Color;
          FLineSerie[lineSelected].Pointer.Brush.Color := dlgLineColor.Color;
          end;
        end;
      end;

    lineSelected := -1;
    lineParam    := lpNone;
  end;

procedure TfmMain.chPlotterMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
  var
    cursorOutOfLegend: Boolean;
    pos:               Integer;
    s:                 String;
    v:                 TPlotterPenStyle;
  begin
    if not cfg.legend.interactive then Exit;

    // если движение мышью с ЛКМ было начато на метке - обрабатываем его
    if lineSelected >= 0 then
      begin
      // преобразуем расстояние движения в значение
      pos := (Y - FMouseDownPos.Y - 20) div 16;

      case lineParam of

        lpStyle: // движение вниз + Ctrl - стиль линии
          begin
          v := TPlotterPenStyle(Constrain(pos, Low(TPlotterPenStyle), High(TPlotterPenStyle)));
          plotter.Charts[lineSelected].Style := v;
          s := Format(TXT_PLOT_LINE_ST, [UTF8LowerCase(TXT_PLOTTER_PEN_STYLE[v])]);
          end;

        lpPoint: // движение вниз + Alt - размер точек линии
          begin
          PlotterSetLinePointSize(Constrain(pos, 0, 10), lineSelected);
          s := Format(TXT_PLOT_LINE_PS, [plotter.Charts[lineSelected].PointSize]);
          end;

        lpWidth: // движение вниз - толщина линии
          begin
          plotter.Charts[lineSelected].Width := Constrain(pos, 1, 10);
          s := Format(TXT_PLOT_LINE_WD, [plotter.Charts[lineSelected].Width]);
          end;
        end;

      // подсказка для отображения значения
      lbPlotterHint.Caption := FLineSerie[lineSelected].Title.Trim + LineEnding + s;
      with pPlotterHint do
        begin
        BevelColor := FLineSerie[lineSelected].SeriesColor;
        Visible    := True;
        Top        := Constrain(chPlotter.Top + Y + 30, 0, pgRxPlotter.Height - Height);
        Left       := Constrain(pPlotter.Left + X - Width div 2, 0, pgRxPlotter.Width - Width);
        end;

      PlotterLineStyleVerify;
      end;

    // не идем дальше, если кнопки мыши нажаты, чтобы не включать инструменты
    // в процессе настройки толщины и стиля линии
    if Shift * [ssLeft, ssRight] <> [] then Exit;

    // отключение инструментов на метках легенды
    cursorOutOfLegend      := PlotterGetLegendClickedIndex(X, Y) < 0;
    chToolMove.Enabled     := cursorOutOfLegend;
    chToolZoomDrag.Enabled := cursorOutOfLegend;
  end;


procedure TfmMain.PlotterParserInit;
  var
    i:      Integer;
    series: String = '';
  begin
    for i := Low(FLineSerie) to High(FLineSerie) do
      begin
      FLineSerie[i]                := TLineSeries.Create(chPlotter);
      FLineSerie[i].Active         := True;
      FLineSerie[i].AxisIndexX     := 0;
      FLineSerie[i].AxisIndexY     := 1;
      FLineSerie[i].LinePen.Width  := 1;
      FLineSerie[i].Legend.Visible := True;
      FLineSerie[i].ShowPoints     := False;
      FLineSerie[i].Pointer.Style  := psCircle;

      chPlotter.AddSeries(FLineSerie[i]);
      series += Format('%d,', [i]);
      end;

    // label to show redefined values
    for i := Low(FPlotterRedefLb) to High(FPlotterRedefLb) do
      begin
      FPlotterRedefLb[i]            := TLabel.Create(fmMain);
      FPlotterRedefLb[i].Caption    := '';
      FPlotterRedefLb[i].Parent     := pPlotterRedef;
      FPlotterRedefLb[i].Font.Color := clRed;
      end;

    chToolPointTracker.AffectedSeries := series;
    chToolPointHint.AffectedSeries    := series;
  end;

procedure TfmMain.PlotterParserReInit;

  procedure SetZoomControl(AToolsetZoom: TZoomMouseWheelTool; ASetting: TPlotterCtrl);
    begin
      case ASetting of
        pcWheel: AToolsetZoom.Shift      := [];
        pcCtrlWheel: AToolsetZoom.Shift  := [ssCtrl];
        pcShiftWheel: AToolsetZoom.Shift := [ssShift];
        pcRightWheel: AToolsetZoom.Shift := [ssRight];
        end;
    end;

  begin
    if fmSettings = nil then Exit; // если не можем прочитать настройки, выходим

    BeginFormUpdate;
    plotter.OnCommand      := nil;
    chNavScrollBar.Visible := cfg.ax.ctrl.bar;
    chNavPanel.Visible     := cfg.plt.view.minimap;
    tbPlotter.Visible      := not cfg.plt.view.panelOnMain;
    tbPlotterEx.Visible    := cfg.plt.view.panelOnMain and acPlotterShow.Checked;

    // в live-режиме отключаем масштабирование по оси X
    if acPlotterQLiveMode.Checked then
      begin
      chToolZoomX.ZoomFactor := 1;
      chToolZoomX.ZoomRatio  := 1;
      end
    else
      begin
      chToolZoomX.ZoomFactor := 1 / cfg.ax.ctrl.factor;
      chToolZoomX.ZoomRatio  := cfg.ax.ctrl.factor;
      end;

    SetZoomControl(chToolZoomX, cfg.ax.ctrl.method);
    SetZoomControl(chToolZoomY, cfg.ay.ctrl.method);

    actionPlotter(acPlotterQTracker);

    plotter.Protocol        := TPlotterProtocol(cbPlotterProtocol.ItemIndex);
    plotter.View            := TPlotterView(cbPlotterView.ItemIndex);
    plotter.WindowSize      := sePlotterViewport.Value;
    plotter.WindowClear     := round(plotter.WindowSize / 100 * cfg.ax.space);
    chLiveView.ViewportSize := sePlotterViewport.Value + 1;
    chToolZoomY.ZoomRatio   := 1 / cfg.ay.ctrl.factor;

    // settings for regexp format
    if cbPlotterRegExp.ItemIndex < 0 then cbPlotterRegExp.ItemIndex := 0;
    plotter.RegExpString   := cfg.regexp.list.Items[cbPlotterRegExp.ItemIndex].RegExp;
    plotter.RegExpLabel    := cfg.regexp.list.Items[cbPlotterRegExp.ItemIndex].RELabel;
    plotter.RegExpValue    := cfg.regexp.list.Items[cbPlotterRegExp.ItemIndex].REValue;
    plotter.RegExpCaseCare := cfg.regexp.casecare;

    chPlotter.ZoomFull(True);
    chPlotter.Foot.Visible     := cfg.ax.labels and (plotter.View = pv2D);
    sbxPlotSettings.Visible    := acPlotterSettings.Checked;
    acPlotterQLiveMode.Enabled := plotter.View <> pvSweep;
    sePlotterViewport.Enabled  := acPlotterQLiveMode.Checked or (plotter.View = pvSweep);
    chLiveView.ExtentY         := lveAuto;
    chLiveView.Active          := acPlotterQLiveMode.Checked and acPlotterQLiveMode.Enabled;
    chLiveView.ViewportSize    := sePlotterViewport.Value;

    // allow execute plotter commands
    if cfg.plt.commands then
      plotter.OnCommand := @PlotterParserOnCommand;

    // allow smoothing via BGRA connector
    if cfg.plt.smooth then
      chPlotter.GUIConnector := chGUIConnBGRA
    else
      chPlotter.GUIConnector := nil;

    // reset initial lines padding in window view
    FLastMaxCh := -1;

    EndFormUpdate;
  end;

procedure TfmMain.PlotterParserOnParseDone(ACharts: TPlotterChartsList);
  var
    _minCh, _maxCh: Integer;

  procedure AddTitles;
    var
      _ch: Integer;
    begin
      for _ch := _minCh to _maxCh do
        FLineSerie[_ch].Title := ACharts[_ch].Caption + '  ';
    end;

  procedure LimitSamples;
    var
      i, _ch, _del: Integer;
    begin
      for _ch := _minCh to _maxCh do
        begin
        _del := FLineSerie[_ch].Count - cfg.ax.samples;
        if _del > 0 then              // if we have a tail
          for i := 1 to _del do       // remove it
            FLineSerie[_ch].Delete(0);
        end;
    end;

  procedure AddNewData;
    var
      i, _ch: Integer;
    begin
      for _ch := _minCh to _maxCh do
        try
        for i := 0 to ACharts[_ch].Count - 1 do
          with ACharts[_ch].Points[i] do
            FLineSerie[_ch].AddXY(X, Y);
        except
        actionPlotter(acPlotterClear);
        end;
    end;

  procedure PlotViewStandard;
    begin
      AddTitles;
      AddNewData;
      LimitSamples;
    end;

  procedure PlotView2D;
    begin
      // show x-axis title
      if cfg.ax.labels and (_maxCh <> 0) then
        chPlotter.Foot.Text.Text := ACharts.Item[0].Caption;

      PlotViewStandard;
    end;

  procedure PlotViewWindow;
    var
      i, _ch: Integer;
    begin
      // init series in window mode
      if (FLastMaxCh < _maxCh) or (FLineSerie[0].Count < plotter.WindowSize + 2) then
        begin
        FLastMaxCh := _maxCh;

        for _ch := _minCh to _maxCh do
          while FLineSerie[_ch].Count < plotter.WindowSize + 2 do
            FLineSerie[_ch].AddXY(FLineSerie[_ch].Count, NaN);
        end;

      AddTitles;
      plotter.WindowPositionSave;

      // add new data
      for _ch := _minCh to _maxCh do
        begin
        plotter.WindowPositionRestore;

        for i := 0 to ACharts[_ch].Count - 1 do
          begin
          with ACharts[_ch].Points[i] do
            begin
            FLineSerie[_ch].YValue[plotter.WindowPos] := Y;

            if not plotter.WindowReset and (plotter.WindowPos = 0) then
              FLineSerie[_ch].YValue[plotter.WindowSize] := Y;
            end;

          plotter.WindowPositionIncrement;
          end;

        // draw 'empty cursor' after data as in oscilloscope
        if not plotter.WindowReset then
          for i := plotter.WindowPos to plotter.WindowPos + plotter.WindowClear do
            if i > plotter.WindowSize then
              break
            else
              FLineSerie[_ch].YValue[i] := NaN;
        end;
    end;

  begin
    _maxCh := (High(FLineSerie) < plotter.LinesCount - 1).Select(
      High(FLineSerie), plotter.LinesCount - 1);

    _minCh := (plotter.View = pv2D).Select(1, 0) + Low(FLineSerie);

    BeginFormUpdate;

      try
      case plotter.View of

        pvStandard:
          PlotViewStandard;

        pv2D:
          PlotView2D;

        pvSweep:
          PlotViewWindow;
        end;

      // lines styles control for lower rendering lag
      PlotterLineStyleVerify;

      except
      acPlotterClear.Execute;
      end;

    EndFormUpdate;
  end;

procedure TfmMain.PlotterParserOnCommand(ACommand: String; AValue: Double; AHexLen: Integer);
  var
    ivalue, tmp, i: Integer;
    valView:        TPlotterView;
  begin
    // if command contains more than 5 characters it's probably not a command
    if Length(ACommand) > 5 then Exit;
    ivalue := Round(AValue);

    case ACommand of

      // command RESET: clear all lines and reinitialize
      'r', 'reset':
        begin
        acPlotterClear.Execute;

        // exit here to show no hints
        Exit;
        end;

      // command CLEAR: remove <ivalue> last points of lines, 0 - all
      'c', 'clear':
        begin
        ivalue := Constrain(ivalue, 0, plotter.Samples);
        if ivalue = 0 then ivalue := plotter.Samples;
        plotter.DeleteLastSamples(ivalue);

        for i := Low(FLineSerie) to High(FLineSerie) do
          if FLineSerie[i].Count <= ivalue then
            FLineSerie[i].Clear
          else
            for tmp := 1 to ivalue do
              FLineSerie[i].Delete(FLineSerie[i].Count - 1);

        // exit here to show no hints
        Exit;
        end;

      // command XWIN: set viewport in samples
      'x', 'xwin':
        begin
        tmp := sePlotterViewport.Value;

        sePlotterViewport.Value    := ivalue;
        sePlotterViewport.OnChange := nil;
        sePlotterViewport.Value    := tmp;   // set old value
        sePlotterViewport.OnChange := @actionPlotter;
        FPlotterRedefLb[0].Caption := TXT_REDEF_VIEWPORT + ': ' + ivalue.ToString;
        end;

      // command GRID: on/off x and y axes grid visibility
      'g', 'grid':
        begin
        ivalue := Constrain(ivalue, 0, 1);

        chPlotter.BottomAxis.Grid.Visible := ivalue <> 0;
        chPlotter.LeftAxis.Grid.Visible   := ivalue <> 0;
        FPlotterRedefLb[1].Caption        := TXT_REDEF_GRID + ': ' + ivalue.ToString;
        end;

      // command XGRID: on/off x-axis grid visibility
      'xg', 'xgrid':
        begin
        ivalue := Constrain(ivalue, 0, 1);

        chPlotter.BottomAxis.Grid.Visible := ivalue <> 0;
        FPlotterRedefLb[2].Caption        := TXT_REDEF_XGRID + ': ' + ivalue.ToString;
        end;

      // command YGRID: on/off y-axis grid visibility
      'yg', 'ygrid':
        begin
        ivalue := Constrain(ivalue, 0, 1);

        chPlotter.LeftAxis.Grid.Visible := ivalue <> 0;
        FPlotterRedefLb[3].Caption      := TXT_REDEF_YGRID + ': ' + ivalue.ToString;
        end;

      // command BACKGROUND: set background color of chart
      'bg', 'back':
        begin
        chPlotter.BackColor        := RGBHexToColor(IntToHex(ivalue, AHexLen));
        FPlotterRedefLb[4].Caption := TXT_REDEF_BGCOLOR + ': #' + IntToHex(ivalue, AHexLen);
        end;

      // command WIDTH: set width of all lines
      'w', 'width':
        begin
        ivalue := Constrain(ivalue, 0, sePlotterPenSize.MaxValue);

        PlotterSetLinesWidth(ivalue);
        FPlotterRedefLb[5].Caption := TXT_REDEF_WIDTH + ': ' + ivalue.ToString;
        end;

      // command POINT: set size of points for all lines
      'p', 'point':
        begin
        ivalue := Constrain(ivalue, 0, sePlotterPoints.MaxValue);

        PlotterSetPointSize(ivalue);
        FPlotterRedefLb[6].Caption := TXT_REDEF_POINTS + ': ' + ivalue.ToString;
        end;

      // command VIEW: set plotter view mode
      'v', 'view':
        begin
        valView := TPlotterView(Constrain(ivalue, Low(TPlotterView), High(TPlotterView)));

        if valView <> plotter.View then
          begin
          actionPlotter(acPlotterClear);
          plotter.View := valView;
          end;

        FPlotterRedefLb[7].Caption := TXT_PLOTTER_VIEW[plotter.View];
        end;

        // unsupported command - exit immediately
      else
        Exit;
      end;

    // show hints of redefined values
    pPlotterRedef.Show;
  end;

procedure TfmMain.PlotterExportCSV;
  const
    lastName: String = ''; // static var
  var
    _max, p: Integer;
    csv:     TCSVDocument;
    fmt:     TFormatSettings;
    _state:  Boolean;


  procedure AddTitle;
    var
      i: Integer;
    begin
      csv.AddRow('#');

      if plotter.View = pv2D then       // 2nd col in 2D mode is X axis value
        if FLineSerie[1].Count > 0 then // do if line 1 has data
          csv.AddCell(csv.RowCount - 1, plotter.Charts[0].Caption);

      for i := Low(FLineSerie) to High(FLineSerie) do
        if FLineSerie[i].Count > 0 then // lines with data only
          csv.AddCell(csv.RowCount - 1, FLineSerie[i].Title.Trim);
    end;

  procedure AddLine(APoint: Integer);
    var
      i: Integer;
    begin
      csv.AddRow(APoint.ToString);

      if plotter.View = pv2D then       // 2nd col in 2D mode is X axis value
        if FLineSerie[1].Count > 0 then // do if line 1 has data
          csv.AddCell(csv.RowCount - 1, FloatToStr(FLineSerie[1].XValue[p], fmt));

      for i := Low(FLineSerie) to High(FLineSerie) do
        if FLineSerie[i].Count > 0 then // lines with data only
          csv.AddCell(csv.RowCount - 1, FloatToStr(FLineSerie[i].YValue[p], fmt));
    end;

  begin
    // disable plotter temporarily
    _state                := acPlotterShow.Checked;
    acPlotterShow.Checked := False;
    Sleep(100);

    csv  := TCSVDocument.Create;
    _max := FLineSerie[(plotter.View = pv2D).Select(1, 0)].Count - 1;

    if _max > 0 then
      begin
      dlgRxSave.Filter   := Format('%s|*.csv', [TXT_DLG_CSV]);
      dlgRxSave.FileName := (lastName <> '').Select(lastName, 'charts-data');

      if dlgRxSave.Execute then
        begin
        csv.Delimiter  := PLOTTER_CSV_DELIM[cfg.csv.delimiter];
        csv.QuoteChar  := PLOTTER_CSV_QUOTES[cfg.csv.quotes];
        csv.LineEnding := DATA_LINEBREAK[cfg.csv.linebreak];
        fmt            := DefaultFormatSettings;
        if cfg.csv.decimal <> pcddSystem then
          fmt.DecimalSeparator := PLOTTER_CSV_DECDELIM[cfg.csv.decimal];

        AddTitle;
        for p := 0 to _max do
          try
          AddLine(p);
          except
          end;

        csv.SaveToFile(dlgRxSave.FileName);

        lastName := ExtractFileName(dlgRxSave.FileName);
        end;
      end;

    csv.Free;
    acPlotterShow.Checked := _state; // re-enable plotter
  end;

procedure TfmMain.PlotterExportImage;
  const
    lastDir: String    = '';    // static var
    dlgShowed: Boolean = False; // static var  
  var
    bmp:       TBGRABitmap;
    id:        IChartDrawer;
    tmp, w, h: Integer;

  function FileNameSetted: Boolean;
    begin
      with dlgRxSave do
        begin
        Filter     := Format('%s|*.png', [TXT_DLG_PNG]);
        InitialDir := (lastDir = '').Select(InitialDir, lastDir);
        FileName   := GetDateTimeFilename + '.png';
        end;

      if not cfg.png.silent then
        dlgShowed          := dlgRxSave.Execute
      else
      if dlgShowed then
        dlgRxSave.FileName := dlgRxSave.InitialDir + dlgRxSave.FileName
      else
      if dlgRxSave.Execute then
        dlgShowed          := True;

      Result := dlgShowed;
    end;

  function GetFontSize: Integer;
    var
      _size: Double;
    begin
      _size := 0;

      if cfg.png.font.prop and cfg.png.custom then
        begin
        _size := Min(cfg.png.w / Width, cfg.png.h / Height);
        _size *= Canvas.GetTextHeight('1');
        end
      else
        _size := cfg.png.font.prop.Select(0, cfg.png.font.size);

      Result := Round(Double((_size < 0.05).Select(-1, _size)));
    end;

  procedure SetChartFontSize(ASize: Integer);
    begin
      if ASize < 0 then Exit;
      chPlotter.LeftAxis.Marks.LabelFont.Size := ASize;
      chPlotter.BottomAxis.Marks.LabelFont.Size := ASize;
      chPlotter.Legend.Font.Size := ASize;
      chPlotter.Foot.Font.Size := ASize;
    end;

  begin
    if FileNameSetted then
      with chPlotter do
        begin
        BeginFormUpdate;
        w   := cfg.png.custom.Select(cfg.png.w, Width);
        h   := cfg.png.custom.Select(cfg.png.h, Height);
        tmp := Font.Size;
        SetChartFontSize(GetFontSize);

        if cfg.plt.smooth then
          begin
          bmp  := TBGRABitmap.Create(w, h);
          DisableRedrawing;
            try
            id := TBGRABitmapDrawer.Create(bmp);
            Draw(id, Rect(0, 0, w, h));
            bmp.SaveToFileUTF8(dlgRxSave.FileName);
            finally
            EnableRedrawing;
            bmp.Free;
            end;
          end
        else
          begin
          if cfg.png.custom then
            begin
            Align  := alNone;
            Width  := w;
            Height := h;
            end;

          SaveToFile(TPortableNetworkGraphic, dlgRxSave.FileName);
          Align := alClient;
          end;

        SetChartFontSize(tmp);
        EndFormUpdate;

        lastDir := ExtractFilePath(dlgRxSave.FileName);
        end;
  end;

procedure TfmMain.PlotterLineStyleVerify(AForceDisable: Boolean);
  var
    _enable: Boolean;
    i:       Integer;
  begin
    // отключение стилизации линий для уменьшения лага прорисовки
    // при большом количестве отображаемых точек
    _enable := not AForceDisable and
      (chPlotter.LogicalExtent.b.X - chPlotter.LogicalExtent.a.X <
      chPlotter.Width * Map(chPlotter.Width, 500, 2000, 2, 1));

    for i := Low(FLineSerie) to High(FLineSerie) do
      with FLineSerie[i] do
        begin

        // if line has color defined in its parameters:
        if plotter.Charts[i].CustomColor then
          begin
          SeriesColor         := plotter.Charts[i].Color;
          Pointer.Brush.Color := SeriesColor;
          end;

        if _enable then

          // custom style of lines
          begin
          LinePen.Width := plotter.Charts[i].Width;
          LinePen.Style := PLOTTER_PEN_STYLE[plotter.Charts[i].Style];

          if plotter.Charts[i].PointSize < 0 then
            ShowPoints        := sePlotterPoints.Value > 0
          else
            begin
            Pointer.HorizSize := plotter.Charts[i].PointSize;
            Pointer.VertSize  := plotter.Charts[i].PointSize;
            ShowPoints        := plotter.Charts[i].PointSize > 0;
            end;
          end

        else

          // simplified style of lines for faster rendering
          begin
          LinePen.Width := 1;
          LinePen.Style := psSolid;
          ShowPoints    := False;
          end;
        end;
  end;

procedure TfmMain.PlotterSetLinesStyle(AValue: TPlotterPenStyle);
  var
    i: Integer;
  begin
    for i := Low(FLineSerie) to High(FLineSerie) do
      begin
      plotter.Charts[i].Style     := AValue;
      FLineSerie[i].LinePen.Style := PLOTTER_PEN_STYLE[AValue];
      end;
  end;

procedure TfmMain.PlotterSetLinesWidth(AValue: Integer);
  var
    i: Integer;
  begin
    for i := Low(FLineSerie) to High(FLineSerie) do
      begin
      plotter.Charts[i].Width     := AValue;
      FLineSerie[i].LinePen.Width := AValue;
      end;

    cbPlotterPenStyle.Repaint;
  end;

procedure TfmMain.PlotterSetLinePointSize(AValue, AIndex: Integer);
  begin
    if not (AIndex in [Low(FLineSerie)..High(FLineSerie)]) then Exit;
    plotter.Charts[AIndex].PointSize     := AValue;
    FLineSerie[AIndex].Pointer.HorizSize := AValue;
    FLineSerie[AIndex].Pointer.VertSize  := AValue;
    FLineSerie[AIndex].ShowPoints        := AValue > 0;
  end;

procedure TfmMain.PlotterSetPointSize(AValue: Integer);
  var
    i: Integer;
  begin
    for i := Low(FLineSerie) to High(FLineSerie) do
      PlotterSetLinePointSize(AValue, i);
  end;

function TfmMain.PlotterGetLegendClickedIndex(X, Y: Integer): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i  := Low(FLineSerie) to High(FLineSerie) do
      if FLineSerie[i].Count <> 0 then
        if InRange(X, FLineLZone[i].Left, FLineLZone[i].Right) and
          InRange(Y, FLineLZone[i].Top, FLineLZone[i].Bottom) then
          Exit(i);
  end;


 { ***  Таблица сохраненных сообщений для передачи  *** }

 // изменение размеров, подгонка ширины строк
procedure TfmMain.sgTxSequencesChangeBounds(Sender: TObject);
  var
    vsbWidth: Integer = 0;
    i:        Integer;
  begin
    if not acTxSequences.Checked then Exit;

    with sgTxSequences, sgTxSequences.Columns do
      begin
      BeginUpdate;
      Font.Assign(seTx.Font);

      for i := 0 to Count - 1 do
        begin
        Items[i].Font.Assign(Font);
        Items[i].Title.Font.Assign(Font);
        Items[i].Title.Font.Bold := True;
        end;

      Items[0].Font.Italic := True;
      DefaultRowHeight     := round(abs(seTx.Font.FontData.Height) * 1.3);

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
      for i    := 1 to txSeqList.Count do
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
      Row := aRow;
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
      if not (goEditing in Options) then
        if UTF8Key = chr(13) then
          actionTxGeneral(sgTxSequences)
        else
        if SelectedColumn.Index > 0 then
          Options := Options + [goEditing];
  end;

// окончание редактирования заголовка сообщения
procedure TfmMain.sgTxSequencesEditingDone(Sender: TObject);
  begin
    with sgTxSequences do
      if goEditing in Options then
        begin
        Options := Options - [goEditing, goAlwaysShowEditor];
        //txSeqList.Caption[Selection.Top - 1] := Cells[1, Selection.Top];
        sgTxSequencesUpdate;
        end;
  end;

// редактирование заголовка сохраненного сообщения
procedure TfmMain.sgTxSequencesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
  begin
    with sgTxSequences do
      if ARow >= FixedRows then
        txSeqList.Caption[ARow - 1] := Cells[1, ARow];
  end;


 { ***  Сервисные методы  *** }

 // обновление изменяемых элементов интерфейса
procedure TfmMain.UpdateControls(AForceUpdate: Boolean);
  const
    updateFlag: Boolean = True;
    connected: Boolean  = True;
  var
    atitle, atitSh, port, status: String;
    _txSeqEditing, _txSeqMoving:  Boolean;
    _textBoxesVisible:            Boolean;
  begin
    BeginFormUpdate;

    if Assigned(fmAbout) then
      atitle := appAbout.GetAppName;

    port             := '';
    updateFlag       := connected xor serial.Connected;
    connected        := serial.Connected;
    acTxSend.Enabled := connected and not serial.IsTxing;
    plotter.Pause    := not (connected and acRxEnable.Checked and acPlotterShow.Checked) or fmSettings.Showing;

    // auto answer mustn't work if plotter is working
    acAutoAnswerEnable.Enabled := not acPlotterShow.Checked;
    acAutoAnswerSetup.Enabled  := not acPlotterShow.Checked;

    // управление доступностью инструментов для работы с списком сохраненных сбщ
    _txSeqEditing           := acTxSequences.Checked and acShowTxBox.Checked and (sgTxSequences.Row >= 1);
    _txSeqMoving            := acTxSequences.Checked and acShowTxBox.Checked and (sgTxSequences.RowCount >= 3);
    acTxSeqSend.Enabled     := _txSeqEditing and acTxSend.Enabled;
    acTxSeqEdit.Enabled     := _txSeqEditing;
    acTxSeqRemove.Enabled   := _txSeqEditing;
    acTxSeqGet.Enabled      := _txSeqEditing;
    acTxSeqMoveDown.Enabled := _txSeqMoving;
    acTxSeqMoveUp.Enabled   := _txSeqMoving;

    acPlotterExpCSV.Enabled  := acPlotterShow.Checked;
    acPlotterQExpCSV.Enabled := acPlotterShow.Checked;
    acPlotterExpImg.Enabled  := acPlotterShow.Checked;
    acPlotterQExpImg.Enabled := acPlotterShow.Checked;

    lbRxSize.Visible         := not acPlotterShow.Checked and cfg.editor.view.size;
    lbRxPosAndSel.Visible    := not acPlotterShow.Checked and cfg.editor.view.pos;
    lbPlotterSize.Visible    := acPlotterShow.Checked and cfg.plt.size;
    lbPlotterCounter.Visible := acPlotterShow.Checked and cfg.ax.counter;

    cbSearchReplaceAll.Enabled := cbSearchReplace.Checked;

    if Assigned(fmUpdate) then
      pAppUpdate.Visible := fmUpdate.IsNotify;

    if lbPlotterCounter.Visible then
      lbPlotterCounter.Caption := plotter.Samples.ToString;

    if lbPlotterSize.Visible then
      lbPlotterSize.Caption := plotter.DataSize.SizeInBytes(
        TXT_BYTE_SHORT, TXT_BYTE_KB, TXT_BYTE_MB, TXT_BYTE_GB, False);

    if AForceUpdate or updateFlag then
      begin
      if connected then
        begin
        port   := serial.ConfigString + ' ' + TXT_SPEED;
        status := serial.ConfigShort + ' ' + TXT_SPEED;
        atitSh := atitle + ' – ' + status + ' – Tx';
        atitSh += serial.RxEnable.Select('/Rx', '');
        atitle += ' – ' + port + ' – Tx';
        atitle += serial.RxEnable.Select('/Rx', '');


        acConnect.Caption    := TXT_DISCONNECT;
        acConnect.Hint       := TXT_DISCONNECT_HINT;
        acConnect.ImageIndex := 26;
        end
      else
        begin
        if serial.Error = ceNone then
          status := '' else
          status := serial.ErrorString + ' ' + serial.Port;

        acConnect.Caption    := TXT_CONNECT;
        acConnect.Hint       := TXT_CONNECT_HINT;
        acConnect.ImageIndex := 2;
        end;

      stStatusBar.Panels.Items[1].Text := status;
      Caption           := atitle;
      Application.Title := atitle;
      tiTrayIcon.Hint   := atitSh.Replace(' – ', LineEnding);

      if acPlotterShow.Checked then
        tiTrayIcon.Hint := tiTrayIcon.Hint + Format(' – plotter: %s, %s',
          [cbPlotterProtocol.Text, cbPlotterView.Text]);

      //cbTxEncoding.Enabled := cbTxType.ItemIndex = 0;
      //cbRxEncoding.Enabled := cbRxType.ItemIndex = 0;

      _textBoxesVisible := acShowTxBox.Checked or
        (acShowRxBox.Checked and not acPlotterShow.Checked);

      acSearch.Enabled         := _textBoxesVisible;
      acShowHEX.Enabled        := _textBoxesVisible;
      acShowLineCounts.Enabled := _textBoxesVisible;

      lbTx.Caption := TXT_TX_CAPTION +
        (cfg.com.encoding and (cbTxType.ItemIndex = 0)).Select(
        ' — ' + Format(TXT_ENCODING, [cbTxEncoding.Text]), '');

      if acPlotterShow.Checked then
        lbRx.Caption := TXT_PLOTTER_CAPTION + Format(' — %s, %s', [
          String(cbPlotterProtocol.Text).Replace('  ', ' ').Replace('  ', ' '),
          String(cbPlotterView.Text).FirstLowCase])
      else
        lbRx.Caption := TXT_RX_CAPTION +
          (cfg.com.encoding and (cbRxType.ItemIndex = 0)).Select(
          ' — ' + Format(TXT_ENCODING, [cbRxEncoding.Text]), '');
      end;

    pTranceiver.Visible := acShowTxBox.Checked or acShowRxBox.Checked;
    EndFormUpdate;
  end;

// обновление состояния индикаторов сигналов порта
procedure TfmMain.UpdateIndicators;
  const
    lock: Integer = 0;

  function SetIndicatorText(AValue: TSerialSignal): String;
    begin
      if not cfg.com.RS232 then Exit('');
      Result := serial.Connected.Select(indicatorText[serial.Signal[AValue]], '---');
    end;

  function SetIndicatorColor(AValue: Boolean): TColor;
    begin
      Result := serial.Connected.Select(indicatorColor[AValue], FInactiveColor);
    end;

  function SetIndicatorColor(AValue: TSerialSignal): TColor;
    begin
      Result := SetIndicatorColor(serial.Signal[AValue]);
    end;

  begin
    if lock > 0 then Exit;
    Inc(lock);

    // цвета индикаторов активности Tx/Rx
    if cfg.com.leds then
      begin
      pLEDTx.Color := SetIndicatorColor(serial.IsTxing);
      pLEDRx.Color := SetIndicatorColor(serial.IsRxing);
      end;

    // цвета индикаторов RS-232
    pSignalRTS.Color   := SetIndicatorColor(ssRTS);
    pSignalDTR.Color   := SetIndicatorColor(ssDTR);
    pSignalBreak.Color := SetIndicatorColor(ssBreak);
    pSignalCTS.Color   := SetIndicatorColor(ssCTS);
    pSignalDSR.Color   := SetIndicatorColor(ssDSR);
    pSignalRing.Color  := SetIndicatorColor(ssRing);
    pSignalRLSD.Color  := SetIndicatorColor(ssRLSD);

    // подписи индикаторов RS-232
    pSignalRTS.Caption   := SetIndicatorText(ssRTS);
    pSignalDTR.Caption   := SetIndicatorText(ssDTR);
    pSignalBreak.Caption := SetIndicatorText(ssBreak);
    pSignalCTS.Caption   := SetIndicatorText(ssCTS);
    pSignalDSR.Caption   := SetIndicatorText(ssDSR);
    pSignalRing.Caption  := SetIndicatorText(ssRing);
    pSignalRLSD.Caption  := SetIndicatorText(ssRLSD);

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

      try
      if psSplitterTxRx.Visible then
        begin
        pTxBox.Parent := TPairSplitterSide(
          (cfg.com.layout in [plTxTop, plTxLeft]).Select(psSide1, psSide2));

        pRxBox.Parent := TPairSplitterSide(
          (cfg.com.layout in [plTxTop, plTxLeft]).Select(psSide2, psSide1));
        end
      else
        begin
        pTxBox.Parent := pTranceiver;
        pRxBox.Parent := pTranceiver;
        end;

      psSplitterTxRx.SplitterType :=
        (cfg.com.layout in [plTxTop, plTxDown]).Select(pstVertical, pstHorizontal);

      psSplitterTxRx.Cursor :=
        (cfg.com.layout in [plTxTop, plTxDown]).Select(crVSplit, crHSplit);
      finally
      psSplitterTxRxChangeBounds(nil);
      end;

    EndFormUpdate;
  end;

// обновление информации о выделении в полях в/в
procedure TfmMain.UpdateSelectionInfo(ALabel: TLabel; ASynEdit: TSynEdit; AEncoding: String;
  InHEX: Boolean = False);
  begin
    if ASynEdit.Focused and ALabel.Visible then
      ALabel.Caption := Format('%s: %d   %s: %d', [
        TXT_POSITION, GetEditorCursorPosition(ASynEdit, AEncoding, InHEX),
        TXT_SELECTION, GetEditorSelectionSize(ASynEdit, AEncoding, InHEX)]);
  end;


// формирование списка кодировок и режимов отображения данных
procedure TfmMain.EncodingsTxRxSet;

  procedure UpdateComboBoxEncoding(AComboBox: TComboBox);
    begin
      with AComboBox do
        begin
        Tag       := ItemIndex;
        EncodingsListAssign(Items);
        ItemIndex := (Tag < 0).Select(0, Tag);
        end;
    end;

  begin
    EncodingsListUpdate;
    UpdateComboBoxEncoding(cbTxEncoding);
    UpdateComboBoxEncoding(cbRxEncoding);
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
          with cfg.editor.hex do
            lastRes := HexStringPosition(SelStart, line, block)
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
          with cfg.editor.hex do
            lastRes :=
              HexStringPosition(SelEnd + 2, line, block) -
              HexStringPosition(SelStart, line, block)
        else
          lastRes   := UTF8ToEncoding(
            Text.Substring(SelStart - 1, SelEnd - SelStart), AEncoding).Length;

        lastLen := SelEnd - SelStart;
        end;

    Result := lastRes;
  end;


// адаптация размеров компонентов интерфейса
procedure TfmMain.AdjustComponentSizes;

  procedure SetControlWidth(AControls: array of TControl; ASize: Integer; ANoMax: Boolean = False);
    var
      item: TControl;
    begin
      if Length(AControls) = 0 then Exit;

      for item in AControls do
        begin
        if (item <> nil) and (ASize > 0) then
          begin
          item.Constraints.MinWidth   := ASize;
          if not ANoMax then
            item.Constraints.MaxWidth := ASize;
          end;

        case item.ToString of

          'TLabel':
            TLabel(item).Constraints.MaxWidth := 0;

          end;
        end;
    end;

  procedure SetControlSizes(AControls: array of TControl; ASizeH: Integer; ASizeW: Integer = 0);
    var
      item: TControl;
    begin
      if Length(AControls) = 0 then Exit;

      for item in AControls do
        begin
        if item.ToString = 'TToolBar' then
          begin
          TToolBar(item).ButtonHeight := ASizeH;
          TToolBar(item).ButtonWidth  := (ASizeW > 0).Select(ASizeW, ASizeH);
          Continue;
          end;

        item.Constraints.MinHeight := ASizeH;
        item.Constraints.MinWidth  := (ASizeW > 0).Select(ASizeW, ASizeH);
        end;
    end;

  procedure SetToolbarButtonSize(AToolbars: array of TToolBar; W: Integer; H: Integer = -1);
    var
      item: TToolBar;
      b:    Integer;
    begin
      if Length(AToolbars) = 0 then Exit;

      for item in AToolbars do
        begin
        item.ButtonHeight   := (H < 0).Select(W, H);
        item.ButtonWidth    := W;
        item.DisabledImages := imImageListD;

        // fix incorrect divider height in vertical toolbars
        for b := 0 to item.ButtonCount - 1 do
          with item.Buttons[b] do
            if Style = tbsDivider then
              begin
              Style := tbsButton;
              Style := tbsDivider;
              end;

        // set sizes of panels-spacers
        for b := 0 to item.ControlCount - 1 do
          if item.Controls[b].ClassName = 'TPanel' then
            with TPanel(item.Controls[b]) do
              begin
              if Tag > 0 then Continue; // skip with tag>0
              Constraints.MinHeight := item.ButtonHeight;
              Constraints.MinWidth  := item.ButtonWidth;
              end;
        end;
    end;

  var
    w, h, i: Integer;
  begin
    BeginFormUpdate;

    // on 96dpi's screen at 100% resolution muat be 16px
    imSVGList.RenderSize := Round(Scale96ToScreen(16) * cfg.com.iconsRes / 100);

    // scale font for all forms
    appTunerEx.Scale := cfg.com.fontSize;

    // custom menu drawing setup
    appTunerEx.Form[Self].MenuAddHeight := Scale96ToScreen(2);
    appTunerEx.Form[Self].MenuTune      := True;

    Screen.HintFont.Height := Font.Height;
    Screen.MenuFont.Height := Font.Height;

    chPlotter.Legend.Font.Height := Font.Height;
    chPlotter.Foot.Font.Height   := Font.Height;

    chPlotter.BottomAxis.Marks.LabelFont.Height := Font.Height;
    chPlotter.LeftAxis.Marks.LabelFont.Height   := Font.Height;

    // allow adjusting components with autosize option
    EndFormUpdate;

    BeginFormUpdate;

    h := Canvas.GetTextHeight('0');
    stStatusBar.Height := h + 2;
    SetControlWidth([pLEDTx, pLEDRx], 2 * h);

    w := Canvas.GetTextWidth('0 234 567 ' + TXT_BYTE_KB);
    SetControlWidth([lbTxSize, lbRxSize, lbPlotterSize], w);

    SetControlWidth([cbPortsList], Canvas.GetTextWidth('COM00') + VertScrollBar.Size + 8);
    SetControlWidth([seAutoSendTime], Canvas.GetTextWidth('000000') + VertScrollBar.Size + 8);
    SetControlWidth([seBaudRateCustom], cbBaudrate.Width + 8);
    SetControlWidth([cbTxType, cbRxType], cbRxType.ItemWidth + 8);

    w := Canvas.GetTextWidth('macintosh1') + VertScrollBar.Size + 8;
    SetControlWidth([cbTxEncoding, cbRxEncoding], w);

    w := Canvas.GetTextWidth('0') * 10 + VertScrollBar.Size + 8;
    SetControlWidth([cbPlotterProtocol, cbPlotterView], w, True);

    SetControlSizes([tbTx, tbRx], cbPortsList.Height, cbPortsList.Height + 8);
    SetControlSizes([tbMain, tbPort, tbTxSequences], cbPortsList.Height + 1);

    // set size of toolbar's buttons which depends on size of icons
    h := Max(cbPortsList.Height + 1, round(imSVGList.RenderSize * 1.4));
    w := round(h * 1.3);
    SetToolbarButtonSize([tbMain, tbPlotter, tbPlotterMore, tbPlotterEx, tbPort, tbTxSequences], h);
    SetToolbarButtonSize([tbTx, tbRx], w, h);

    // adjust combos in toolbars: horizontal in center
    pToolbarTxTime.Constraints.MinHeight   := tbTx.ButtonHeight;
    pToolbarTxCombos.Constraints.MinHeight := tbTx.ButtonHeight;
    pToolbarRxCombos.Constraints.MinHeight := tbRx.ButtonHeight;

    psSide1.Constraints.MinHeight := 3 * pMainToolbar.Height;
    psSide2.Constraints.MinHeight := 3 * pMainToolbar.Height;

    // adjust LEDs height
    pLEDTx.Constraints.MinHeight := Scale96ToScreen(8);
    pLEDRx.Constraints.MinHeight := Scale96ToScreen(8);

    EndFormUpdate;
  end;

// adjust colors and some other values according to theme
procedure TfmMain.AdjustThemeDependentValues;
  var
    synEd: TSynEdit;
    panel: TPanel;
    fnt:   TFont;

  procedure SetFont(AFont: TFont; AIndex, ASize: Integer; AColor: TColor);
    begin
      if AIndex < 0 then Exit;
      AFont.Name  := Screen.Fonts[AIndex];
      AFont.Size  := ASize;
      AFont.Color := AColor;
    end;
  begin
    if appTunerEx.IsDarkTheme then
      begin                        // dark theme, if available

      // iconspack for dark theme located in resources
      imSVGList.LoadRes       := 'ICONSPACK-DARK';
      imSVGList.DisabledLevel := 96;

      apAppProperties.HintColor    := $497634;
      Screen.HintFont.Color        := clWindowText;
      FInactiveColor               := Color + $444444;
      sgTxSequences.AlternateColor := $2E4921;
      sgTxSequences.Color          := $263D1B;
      chPlotter.Color              := cfg.plt.dark.bg;
      chPlotter.BackColor          := cfg.plt.dark.bgwork;
      chPlotter.Legend.Font.Color  := cfg.plt.dark.txt;
      chPlotter.Foot.Font.Color    := cfg.plt.dark.txt;
      pPlotterToolbar.Color        := cfg.plt.dark.bgwork;

      chPlotter.BottomAxis.Grid.Color            := cfg.ax.dark;
      chPlotter.BottomAxis.Marks.LabelFont.Color := cfg.plt.dark.txt;
      chPlotter.LeftAxis.Grid.Color              := cfg.ay.dark;
      chPlotter.LeftAxis.Marks.LabelFont.Color   := cfg.plt.dark.txt;

      for synEd in [seTx, seTxHex, seRx, seRxHex] do
        begin
        synEd.LineHighlightColor.Background := Color + $060606;
        synEd.RightEdgeColor                := Color + $222222;
        end;

      for fnt in [seTx.Font, seTxHex.Font, fmASCIIChar.sgChar.Font] do
        with cfg.tx.fontdark do
          SetFont(fnt, index, size, color);

      for fnt in [seRx.Font, seRxHex.Font] do
        with cfg.rx.fontdark do
          SetFont(fnt, index, size, color);
      end
    else
      begin                        // light theme, default

      // iconspack for light theme is loaded in component already

      FInactiveColor               := Color - $444444;
      sgTxSequences.AlternateColor := $CFE8C6;
      sgTxSequences.Color          := $E7F2E1;
      chPlotter.Color              := cfg.plt.color.bg;
      chPlotter.BackColor          := cfg.plt.color.bgwork;
      chPlotter.Legend.Font.Color  := cfg.plt.color.txt;
      chPlotter.Foot.Font.Color    := cfg.plt.color.txt;
      pPlotterToolbar.Color        := cfg.plt.color.bgwork;

      chPlotter.BottomAxis.Grid.Color            := cfg.ax.color;
      chPlotter.BottomAxis.Marks.LabelFont.Color := cfg.plt.color.txt;
      chPlotter.LeftAxis.Grid.Color              := cfg.ay.color;
      chPlotter.LeftAxis.Marks.LabelFont.Color   := cfg.plt.color.txt;

      for synEd in [seTx, seTxHex, seRx, seRxHex] do
        begin
        synEd.LineHighlightColor.Background := Color - $060606;
        synEd.RightEdgeColor                := Color - $222222;
        end;

      for fnt in [seTx.Font, seTxHex.Font, fmASCIIChar.sgChar.Font] do
        with cfg.tx.font do
          SetFont(fnt, index, size, color);

      for fnt in [seRx.Font, seRxHex.Font] do
        with cfg.rx.font do
          SetFont(fnt, index, size, color);
      end;

    chToolPointTracker.CrosshairPen.Color := chPlotter.Legend.Font.Color;
    sgTxSequences.Font.Color              := clWindowText;

    for panel in [pSearch, pSignals, pAppUpdate, pTxTitle, pRxTitle] do
      panel.Color := cl3DLight;

    for synEd in [seTx, seTxHex, seRx, seRxHex] do
      begin
      synEd.Color := clWindow;
      synEd.Gutter.LineNumberPart.MarkupInfo.Foreground := clGrayText;
      end;
  end;


 { ***  Settings and language  *** }

 // save some specific non-component settings
procedure TfmMain.BeforeSaveConfig;
  begin
    cfg.connect.opened := serial.Connected;
    cfg.connect.port   := serial.Port;

    cfg.tx.buffer := cfg.tx.restore.Select(EncodeStringBase64(seTx.Text), '');
    cfg.rx.buffer := cfg.rx.restore.Select(EncodeStringBase64(rx), '');

    cfg.rx.answer.input  := EncodeStringBase64(fmCommands.SequenceList.CommaText);
    cfg.rx.answer.output := EncodeStringBase64(fmCommands.AnswerList.CommaText);

    cfg.tx.sequences    := txSeqList.Save;
    cfg.regexp.listData := cfg.regexp.list.Save;
  end;

// load some specific non-component settings
procedure TfmMain.AfterLoadConfig;
  begin
    serial.PortSettings(cfg.connect.port, 0, 0, 'N', 0);

    seTx.Text := DecodeStringBase64(cfg.tx.buffer);
    rx        := DecodeStringBase64(cfg.rx.buffer);
    seRxChange(nil);

    fmCommands.SequenceList.CommaText := DecodeStringBase64(cfg.rx.answer.input);
    fmCommands.AnswerList.CommaText   := DecodeStringBase64(cfg.rx.answer.output);

    txSeqList.Load(cfg.tx.sequences);
    sgTxSequencesUpdate;

    // на случай, если некорректные параметры положения формы
    if abs(Top) > Screen.Height - Height then Top := (Screen.Height - Height) div 2;
    if abs(Left) > Screen.Width - Width then Left := (Screen.Width - Width) div 2;
  end;

// сброс настроек
procedure TfmMain.acResetExecute(Sender: TObject);
  begin
    if fmConfirm.Show(TXT_RESET, WARN_RESET, [mbYes, mbNo], Self) <> mrYes then Exit;

    Settings.Clear;
    appTunerEx.ClearProperties;
    Close;
  end;

// действие: применение настроек
procedure TfmMain.SettingsApply(Sender: TObject);
  var
    tmpSE: TCustomSynEdit;
    i:     Integer;
  begin
    // set new interface language
    appLocalizerEx.CurrentLanguage := cfg.com.langIndex;

    EncodingsTxRxSet;     // update encodings lists and view mode lists
    AdjustComponentSizes; // adjust sizes of components
    BeginFormUpdate;

      try
      appTunerEx.Form[Self].MenuShow := cfg.com.menu;

      lbTxPosAndSel.Visible := cfg.editor.view.pos;
      stStatusBar.Visible   := cfg.com.status;
      pLEDTx.Visible        := cfg.com.leds;
      pLEDRx.Visible        := cfg.com.leds;
      lbTxSize.Visible      := cfg.editor.view.size;
      seTx.RightEdge        := cfg.editor.right.pos;
      seRx.RightEdge        := cfg.editor.right.pos;
      seTx.TabWidth         := cfg.editor.tab;
      seRx.TabWidth         := cfg.editor.tab;

      // snapping to screen edges
      SnapBuffer                := 24;
      SnapOptions.SnapToMonitor := cfg.com.glued;

      // height of RS-232 indicators
      pSignalBreak.Constraints.MinHeight := cfg.com.RS232.Select(
        Canvas.GetTextHeight('0') + 2, Scale96ToScreen(8));

      for tmpSE in [seTx, seTxHex, seRx, seRxHex] do
        tmpSE.Font.Quality := cfg.editor.quality.Select(fqCleartypeNatural, fqNonAntialiased);

      // применение стиля новой строки, так хитро потому, что
      // в уже созданного SynEdit невозможно изменить стиль напрямую
      System.DefaultTextLineBreakStyle := cfg.editor.linebreak;
      tmpSE      := TCustomSynEdit.Create(nil);
      tmpSE.Text := seTx.Text;
      seTx.ShareTextBufferFrom(tmpSE);
      FreeAndNil(tmpSE);

      serial.Hardflow           := cfg.connect.hardflow;
      serial.BreakDuration      := cfg.tx.breakTime;
      serial.DeadlockTimeout    := cfg.tx.timeout;
      serial.TimestampStrBefore := cfg.rx.timestamp.before;
      serial.TimestampStrAfter  := cfg.rx.timestamp.after;
      serial.RxPacketTime       := cfg.rx.timestamp.timeout;
      serial.EnableRxTimestamp  := cfg.rx.timestamp.enable;

      sbRTS.Enabled := not cfg.connect.hardflow;
      sbDTR.Enabled := not cfg.connect.hardflow;

      chPlotter.Margins.Top    := cfg.ay.offset.t;
      chPlotter.Margins.Bottom := cfg.ay.offset.b;
      chPlotter.Legend.Visible := cfg.legend.enable;

      chPlotter.BottomAxis.Grid.Visible  := cfg.ax.grid;
      chPlotter.BottomAxis.Marks.Visible := cfg.ax.marks;
      chPlotter.LeftAxis.Grid.Visible    := cfg.ay.grid;
      chPlotter.LeftAxis.Marks.Visible   := cfg.ay.marks;

      cbPlotterRegExp.Tag             := cbPlotterRegExp.ItemIndex;
      cbPlotterRegExp.Items.CommaText := cfg.regexp.list.CommaText;
      cbPlotterRegExp.ItemIndex       := cbPlotterRegExp.Tag;

      // change colors only if settings applied
      if fmSettings.ModalResult <> mrCancel then
        for i := 0 to MAX_SERIES - 1 do
          begin
          FLineSerie[i].SeriesColor         := cfg.plt.color.line[i];
          FLineSerie[i].Pointer.Brush.Color := cfg.plt.color.line[i];
          end;

      if cfg.editor.right.enable then
        begin
        seTx.Options := seTx.Options - [eoHideRightMargin];
        seRx.Options := seRx.Options - [eoHideRightMargin];
        end
      else
        begin
        seTx.Options := seTx.Options + [eoHideRightMargin];
        seRx.Options := seRx.Options + [eoHideRightMargin];
        end;

      except
      if fmConfirm.Show(TXT_ERROR, WARN_SETTINGS, mbYesNo, Self) = mrYes then
        Close;
      end;

    actionViewGeneral(acShowTxBox);
    FormChangeBounds(Sender);
    seTxChange(Sender);
    seRxChange(Sender);
    PlotterParserReInit;
    AdjustThemeDependentValues;
    EndFormUpdate;
  end;

// localize some specific items
procedure TfmMain.OnLanguageChange(Sender: TObject);
  begin
    BeginFormUpdate;

    // обновляем подсказки
    acToggleRTS.Hint        := 'Request to Send' + LineEnding + HINT_RTS;
    acToggleDTR.Hint        := 'Data Terminal Ready' + LineEnding + HINT_DTR;
    acToggleBreak.Hint      := 'Break' + LineEnding + HINT_BREAK;
    lbSignalCTS.Hint        := 'Clear to Send' + LineEnding + HINT_CTS;
    lbSignalDSR.Hint        := 'Data Set Ready' + LineEnding + HINT_DSR;
    lbSignalRing.Hint       := 'Ring Indicator' + LineEnding + HINT_RING;
    lbSignalRLSD.Hint       := 'Receive Line Signal Detect' + LineEnding + '(Data Carrier Detect)' + LineEnding + HINT_RLSD;
    pSignalRTS.Hint         := 'Request to Send' + LineEnding + HINT_RTS;
    pSignalDTR.Hint         := 'Data Terminal Ready' + LineEnding + HINT_DTR;
    pSignalBreak.Hint       := 'Break' + LineEnding + HINT_BREAK;
    pSignalCTS.Hint         := 'Clear to Send' + LineEnding + HINT_CTS;
    pSignalDSR.Hint         := 'Data Set Ready' + LineEnding + HINT_DSR;
    pSignalRing.Hint        := 'Ring Indicator' + LineEnding + HINT_RING;
    pSignalRLSD.Hint        := 'Receive Line Signal Detect' + LineEnding + '(Data Carrier Detect)' + LineEnding + HINT_RLSD;
    acAutoSend.Hint         := HINT_AUTOSEND;
    acAutoAnswerEnable.Hint := HINT_AUTOANSWER;

    // update dropdown lists
    appLocalizerEx.Localize(cbTxType, TXT_DATA_VIEW);
    appLocalizerEx.Localize(cbRxType, TXT_DATA_VIEW);
    appLocalizerEx.Localize(cbBaudrate, TXT_BAUDRATE);
    appLocalizerEx.Localize(cbPlotterProtocol, TXT_PLOTTER_PROTOCOL);
    appLocalizerEx.Localize(cbPlotterView, TXT_PLOTTER_VIEW);
    appLocalizerEx.Localize(cbPlotterPenStyle, TXT_PLOTTER_PEN_STYLE);

    sgTxSequences.Columns.Items[1].Title.Caption := TXT_DATA;
    EndFormUpdate;

    appTunerEx.TuneComboboxes := True;
  end;


initialization
  portList  := TStringList.Create;
  serial    := TSerialPortThread.Create;
  txSeqList := TSequencesList.Create;
  plotter   := TPlotterParser.Create(MAX_SERIES);


finalization
  portList.Free;
  serial.Terminate;
  txSeqList.Free;
  plotter.Free;

end.
