unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, IniPropStorage, LCLTranslator, Buttons,
  LazFileUtils, LazUTF8, SynEdit,
  fm_confirm,
  u_encodings, u_utilities, u_common, u_settings, u_settings_record,
  u_plotter_types, u_plotter_regexplist, u_plotter;

resourcestring
  TXT_COLOR_HINT   = 'Цвет %d-й линии плоттера';
  TXT_WIN_THEME    = 'Только Windows 10 1809+';

const
  LANGUAGE_DEFAULT = 'RU, Russian - Русский';
  LANGUAGES_DIR    = DirectorySeparator + 'lang';
  LANGUAGES_FILE   = LANGUAGES_DIR + DirectorySeparator + 'languages.ini';
  LANGUAGE_FILE    = LANGUAGES_DIR + DirectorySeparator + 'uTerminal';
  SETTINGS_FILE    = DirectorySeparator + 'settings.ini';

  // цвета графиков по умолчанию
  DEFAULT_SERIE_COLOR: array[0..MAX_SERIES - 1] of TColor =
    ($FF8000, $00D000, clRed, $C00000, clFuchsia, $0080FF, clGreen, $00E0E0,
    $FF8000, $00D000, clRed, $C00000, clFuchsia, $0080FF, clGreen, $00E0E0);

type

  { TfmSettings }

  TfmSettings = class(TForm)
    {$Include fm_settings_controls.inc}

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ColorButtonsInit;
    procedure cbLanguageChange(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure tvTabsSelectionChanged(Sender: TObject);
    procedure seFontSizeChange(Sender: TObject);
    procedure actionExecute(Sender: TObject);
    procedure edRegExpNameChange(Sender: TObject);
    procedure seRegExpStrChange(Sender: TObject);
    procedure cbRegExpListChange(Sender: TObject);
    procedure seRegExpStrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);

  private
    FLangList: TStringList;
    FLang:     String;
    FREList:   TPlotterRegExpList;
    FREItem:   TPlotterRegExpItem;

    procedure IniStorageLangLoad;
    procedure AddStandardSettings;
    procedure LoadComponentsFromFields;
    procedure LoadFieldsFromComponents;

  public
    function LanguageChangeImmediately: Boolean;

  end;

// public variables, available in other units
var
  fmSettings:  TfmSettings;
  cfg:         TAppConfiguration; // all settings of app
  useStorages: Boolean = True;
  appThemeAvailable: Boolean = False;

function GetAppIniFileName: String;


implementation

var
  Settings:     TSettings;
  cbSerieColor: array[0..MAX_SERIES - 1] of TColorButton;
  lbSerieColor: array[0..MAX_SERIES - 1] of TLabel;
  defaultFont:  Integer;


function GetAppIniFileName: String;
  begin
    Result := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;
  end;

{$R *.lfm}

 { TfmSettings }

procedure TfmSettings.FormCreate(Sender: TObject);
  begin
    {$IFDEF DEBUG}
    cbShowSplash.Checked := False;
    {$ENDIF}

    ColorButtonsInit;
    IniStorageLangLoad;
    AddStandardSettings;

    // font default settings
    cbFontTxName.Items  := screen.Fonts;
    cbFontRxName.Items  := screen.Fonts;
    cbFontTxNameD.Items := screen.Fonts;
    cbFontRxNameD.Items := screen.Fonts;
    defaultFont         := Max(0, Screen.Fonts.IndexOf('Consolas'));

    pcPageCtrl.ActivePageIndex := 0;
    pcPageCtrl.ShowTabs        := not tvTabs.Visible;
    seRegExpStr.Gutter.Visible := True;
    lbRegExpPos.Caption        := '0';

    FREItem := TPlotterRegExpItem.Create;
    FREList := TPlotterRegExpList.Create(GetAppIniFileName);

    // load list of RegExp presets
    if useStorages then FREList.LoadFromIni;
    cbRegExpList.Items.CommaText := FREList.CommaText;
    cbRegExpList.ItemIndex       := 0;

    // load app settings
    if useStorages then Settings.LoadFromIniStorage;
    LoadFieldsFromComponents;
  end;

procedure TfmSettings.FormShow(Sender: TObject);

  procedure FormAutoSize;
    var
      i: Integer;
    begin
      for i := 1 to pcPageCtrl.PageCount do
        begin
        BeginFormUpdate;
        pcPageCtrl.ActivePageIndex       := i - 1;
        pcPageCtrl.Pages[i - 1].AutoSize := True;

        AutoSize := True;
        EndFormUpdate;

        Constraints.MinWidth  := Width;
        Constraints.MinHeight := Height;

        AutoSize := False;
        end;
    end;

  procedure AdjustComponentsSizes;
    var
      i: Integer;
      w: Integer = 0;
    begin  Settings.UpdateComboboxList;
      Settings.AdjustComboItemWidth;

      // get tree view min width
      for i := 0 to tvTabs.Items.Count - 1 do
        w := Max(w, Canvas.GetTextWidth(tvTabs.Items.Item[i].Text));

      // set tree view min sizes
      tvTabs.Constraints.MinWidth  := w + tvTabs.Indent * 2 + VertScrollBar.Size;
      tvTabs.Constraints.MinHeight := tvTabs.Items.Count * tvTabs.DefaultItemHeight;

      // adjust speedbuttons to show as square buttons
      sbRegExpAdd.Constraints.MinWidth := sbRegExpAdd.Height;
      sbRegExpDel.Constraints.MinWidth := sbRegExpDel.Height;

      lbAppRestart.Constraints.MinHeight := cbAppTheme.Height;
    end;

  procedure DarkThemeSupport;
    begin
      if appThemeAvailable then
        begin
        pDarkTheme.BorderStyle := bsNone;
        cbAppTheme.Visible     := True;
        pTitleLT1.Visible      := True;
        pFontDarkBlock.Visible := True;
        end
      else
        lbAppRestart.Caption   := ' ' + TXT_WIN_THEME + ' ';
    end;

  begin
    BeginFormUpdate;
    AdjustComponentsSizes;
    DarkThemeSupport;
    if Assigned(Sender) then LoadComponentsFromFields;
    EndFormUpdate;

    pcPageCtrl.Tag := pcPageCtrl.ActivePageIndex;
    if Assigned(Sender) then FormAutoSize;

    BeginFormUpdate;
    pcPageCtrl.ActivePageIndex        := pcPageCtrl.Tag;
    if Assigned(Sender) then Position := poMainFormCenter;
    EndFormUpdate;

    tmrUpdate.Enabled := True;
  end;

procedure TfmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    tmrUpdate.Enabled := False;
    if ModalResult <> mrOk then acCancel.Execute;
  end;

procedure TfmSettings.FormDestroy(Sender: TObject);
  begin
    if not useStorages then Exit;

    // save settings to storage
    LoadComponentsFromFields;
    FREList.SaveToIni;
    Settings.SaveToIniStorage;
  end;


procedure TfmSettings.AddStandardSettings;
  var
    i: Integer;
  begin
    if Settings = nil then
      Settings := TSettings.Create(Self, ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE);

    with cfg do
      begin
      Settings.Add(cbLanguage, @com.langIndex);
      Settings.Add(seIconsRes, @com.iconsRes);
      Settings.Add(seFontSize, @com.fontSize);
      Settings.Add(cbShowSplash, @com.splash);
      Settings.Add(cbGluedWindow, @com.glued);
      Settings.Add(cbShowIndicators, @com.leds);
      Settings.Add(cbMinimizeToTray, @com.tray);
      Settings.Add(cbShowMenu, @com.menu);
      Settings.Add(cbShowEncoding, @com.encoding);
      Settings.Add(cbShowRS232Captions, @com.RS232);
      Settings.Add(cbShowStatusBar, @com.status);

      Settings.Add(seTabSize, @editor.tab);
      Settings.Add(cbFontQuality, @editor.quality);
      Settings.Add(cbShowRightEdge, @editor.right.enable);
      Settings.Add(seRightEdge, @editor.right.pos);
      Settings.Add(cbShowSizes, @editor.view.size);
      Settings.Add(cbSizesInBytes, @editor.view.inBytes);
      Settings.Add(cbShowPosAndSel, @editor.view.pos);
      Settings.Add(seHEXBlockBytes, @editor.hex.block);
      Settings.Add(seHEXLineBytes, @editor.hex.line);

      Settings.Add(cbAutoconnect, @connect.auto);
      Settings.Add(cbCheckPort, @connect.check);
      Settings.Add(cbHardflow, @connect.hardflow);

      Settings.Add(cbTxRestore, @tx.restore);
      Settings.Add(cbFileLoadWarning, @tx.loadWarn);
      Settings.Add(cbFileDataAdd, @tx.addition);
      Settings.Add(seTxBreakTime, @tx.breakTime);
      Settings.Add(seTxDeadlockTO, @tx.timeout);
      Settings.Add(cbFontTxName, @tx.font.index);
      Settings.Add(seFontTxSize, @tx.font.size);
      Settings.Add(colbFontTx, @tx.font.color);
      Settings.Add(cbFontTxNameD, @tx.fontdark.index);
      Settings.Add(seFontTxSizeD, @tx.fontdark.size);
      Settings.Add(colbFontTxD, @tx.fontdark.color);

      Settings.Add(cbRxRestore, @rx.restore);
      Settings.Add(seRxSizeLimit, @rx.limit, 1024);
      Settings.Add(cbRxTimestamp, @rx.timestamp.enable);
      Settings.Add(edRxTSBefore, @rx.timestamp.before);
      Settings.Add(edRxTSAfter, @rx.timestamp.after);
      Settings.Add(seRxTimeout, @rx.timestamp.timeout);
      Settings.Add(cbFontRxName, @rx.font.index);
      Settings.Add(seFontRxSize, @rx.font.size);
      Settings.Add(colbFontRx, @rx.font.color);
      Settings.Add(cbFontRxNameD, @rx.fontdark.index);
      Settings.Add(seFontRxSizeD, @rx.fontdark.size);
      Settings.Add(colbFontRxD, @rx.fontdark.color);

      Settings.Add(seImageWidth, @png.w);
      Settings.Add(seImageHeight, @png.h);
      Settings.Add(cbImageDialogOne, @png.silent);
      Settings.Add(cbImageCustomSize, @png.custom);
      Settings.Add(cbImageFontProp, @png.font.prop);
      Settings.Add(seImageFontSize, @png.font.size);

      Settings.Add(cbPlotLineRecolor, @plt.recolor);
      Settings.Add(cbPlotLineReactivate, @plt.reactivate);
      Settings.Add(cbPlotCopyRx, @plt.copyRx);
      Settings.Add(cbPlotClearRx, @plt.clearRx);
      Settings.Add(cbPlotAllowCommands, @plt.commands);
      Settings.Add(cbPlotSmooth, @plt.smooth);
      Settings.Add(cbPlotSize, @plt.size);
      Settings.Add(cbPlotMinimap, @plt.view.minimap);
      Settings.Add(cbPlotPanelOnMain, @plt.view.panelOnMain);
      Settings.Add(colbPlotterBG, @plt.color.bg);
      Settings.Add(colbPlotterBGW, @plt.color.bgwork);
      Settings.Add(colbPlotterText, @plt.color.txt);
      Settings.Add(colbPlotterBGd, @plt.dark.bg);
      Settings.Add(colbPlotterBGWd, @plt.dark.bgwork);
      Settings.Add(colbPlotterTextd, @plt.dark.txt);

      Settings.Add(cbPlotGridX, @ax.grid);
      Settings.Add(cbPlotMarksX, @ax.marks);
      Settings.Add(cbPlotLabelX, @ax.labels);
      Settings.Add(cbPlotSampleCount, @ax.counter);
      Settings.Add(sePlotSamples, @ax.samples);
      Settings.Add(sePlotWinClear, @ax.space);
      Settings.Add(colbPlotterGX, @ax.color);
      Settings.Add(colbPlotterGXd, @ax.dark);
      Settings.Add(fsePlotCtrlFactorX, @ax.ctrl.factor);
      Settings.Add(cbPlotXBar, @ax.ctrl.bar);

      Settings.Add(cbPlotGridY, @ay.grid);
      Settings.Add(cbPlotMarksY, @ay.marks);
      Settings.Add(colbPlotterGY, @ay.color);
      Settings.Add(colbPlotterGYd, @ay.dark);
      Settings.Add(fsePlotCtrlFactorY, @ay.ctrl.factor);
      Settings.Add(sePlotOffsetYTop, @ay.offset.t);
      Settings.Add(sePlotOffsetYBot, @ay.offset.b);

      Settings.Add(cbPlotLegendShow, @legend.enable);
      Settings.Add(cbPlotShowUntitled, @legend.untitled);
      Settings.Add(cbPlotLegendColored, @legend.colored);
      Settings.Add(cbPlotLegendColoredB, @legend.coloredframe);
      Settings.Add(cbPlotLegendActive, @legend.interactive);
      Settings.Add(cbPlotLegendFrame, @legend.frame);
      Settings.Add(cbPlotLegendIndex, @legend.index);
      Settings.Add(cbPlotLegendStyle, @legend.style);

      Settings.Add(cbRegExpCaseCare, @re.casecare);
      Settings.Add(cbRegExpList, nil);

      for i := 0 to MAX_SERIES - 1 do
        Settings.Add(cbSerieColor[i], @plt.color.line[i]);

      // для перевода строк в TComboBox задаем массив строк
      Settings.Add(cbPanelsLayout, @com.layoutIndex, TXT_PANELS_LAYOUT);
      Settings.Add(cbAppTheme, @com.theme, TXT_THEMES);
      Settings.Add(cbAppUpdateWay, @com.update.wayIndex, TXT_UPDATE_WAY);
      Settings.Add(cbAppUpdateFreq, @com.update.freqIndex, TXT_UPDATE_FREQ);
      Settings.Add(cbLineBreakStyle, @editor.linebreakIndex, TXT_LINEBREAK);
      Settings.Add(cbCSVDelimiter, @csv.delimiterIndex, TXT_PLOTTER_CSV_DELIM);
      Settings.Add(cbCSVDecDelimiter, @csv.decimalIndex, TXT_PLOTTER_CSV_DECDELIM);
      Settings.Add(cbCSVQuotes, @csv.quotesIndex, TXT_PLOTTER_CSV_QUOTES);
      Settings.Add(cbCSVLineBreak, @csv.linebreakIndex, TXT_LINEBREAK);
      Settings.Add(cbPlotCtrlX, @ax.ctrl.methodIndex, TXT_CTRL_METHOD);
      Settings.Add(cbPlotCtrlY, @ay.ctrl.methodIndex, TXT_CTRL_METHOD);
      end;
  end;

procedure TfmSettings.LoadComponentsFromFields;
  begin
    Settings.LoadCompValues;

    // load current RegExp preset data to fields
    cbRegExpListChange(nil);
  end;

procedure TfmSettings.LoadFieldsFromComponents;
  begin
    Settings.LoadFields;

    // create default regexp if regexp list is empty
    if cbRegExpList.ItemIndex < 0 then
      with TPlotterParser.Create(1) do
        begin
        edRegExpName.Text  := '(default)';
        edRegExpLabel.Text := RegExpLabel;
        edRegExpValue.Text := RegExpValue;
        seRegExpStr.Text   := RegExpString;
        acRegExpAdd.Execute;
        Free;
        end;

    // set values to non-standard settings
    with cfg do
      begin
      com.lang         := FLang;
      com.layout       := TPanelsLayout(com.layoutIndex);
      com.update.way   := TAppUpdateWay(com.update.wayIndex);
      com.update.freq  := TAppUpdateFreq(com.update.freqIndex);
      editor.linebreak := TTextLineBreakStyle(editor.linebreakIndex);
      csv.delimiter    := TPlotterCSVDelim(csv.delimiterIndex);
      csv.decimal      := TPlotterCSVDecDelim(csv.decimalIndex);
      csv.quotes       := TPlotterCSVQuotes(csv.quotesIndex);
      csv.linebreak    := TTextLineBreakStyle(csv.linebreakIndex);
      ax.ctrl.method   := TPlotterCtrl(ax.ctrl.methodIndex);
      ay.ctrl.method   := TPlotterCtrl(ay.ctrl.methodIndex);
      re.list          := FREList;

      if tx.font.index < 0 then tx.font.index := defaultFont;
      if rx.font.index < 0 then rx.font.index := defaultFont;

      if tx.fontdark.index < 0 then tx.fontdark.index := defaultFont;
      if rx.fontdark.index < 0 then rx.fontdark.index := defaultFont;
      end;
  end;


procedure TfmSettings.IniStorageLangLoad;
  var
    i, cnt:    Integer;
    lng, flng: String;
  begin
    if FLangList = nil then
      FLangList := TStringList.Create;

    LazGetLanguageIDs(lng, flng);
    cbLanguage.Clear;
    FLangList.Append('');
    cbLanguage.Items.Append('System or native: ' + flng + ' (' + lng.ToLower + ')');

    with TIniPropStorage.Create(nil) do
      begin
      IniFileName := ExtractFileDir(ParamStrUTF8(0)) + LANGUAGES_FILE;
      Active      := True;
      IniSection  := 'Languages List';

      // если приложение не нашло файл со списком локализаций - создаем его
      if not FileExistsUTF8(IniFileName) then
        begin
        WriteInteger('Count', 1);
        WriteString('L-1', LANGUAGE_DEFAULT);
        end;

      // считываем список локализаций
      cnt := ReadInteger('Count', 1);
      cbLanguage.ItemIndex := 0;

      if cnt > 0 then
        for i := 1 to cnt do
          begin
          FLangList.Append(GetLangCode(ReadString('L-' + i.ToString, '')));
          cbLanguage.Items.Append(GetLangCaption(ReadString('L-' + i.ToString, '')));
          end;

      Free;
      end;
  end;


procedure TfmSettings.actionExecute(Sender: TObject);
  begin
    case TComponent(Sender).Name of

      'acOK':
        begin
        LoadFieldsFromComponents;
        ModalResult := mrOk;
        end;

      'acCancel':
        begin
        LoadComponentsFromFields;
        ModalResult := mrCancel;
        end;

      'acRegExpAdd':
        begin
        if FREItem.Error then Exit;
        if FREList.Add(edRegExpName.Text, seRegExpStr.Text,
          edRegExpLabel.Text, edRegExpValue.Text) < 0 then Exit;

        cbRegExpList.Items.CommaText := FREList.CommaText;
        cbRegExpList.Text            := edRegExpName.Text;
        end;

      'acRegExpDel':
        begin
        cbRegExpList.Tag := cbRegExpList.ItemIndex;
        if FREList.Delete(cbRegExpList.Text) < 0 then Exit;

        cbRegExpList.Items.CommaText := FREList.CommaText;
        cbRegExpList.ItemIndex       := Max(cbRegExpList.Tag - 1, 0);
        end;

      'lbPlotLegendHintBtn':
        fmConfirm.Show('', lbPlotLegendHint.Caption, [], Self);

      end;
  end;


procedure TfmSettings.tmrUpdateTimer(Sender: TObject);
  var
    f: Boolean;
  begin
    f := cbRxTimestamp.Checked;

    lbRxTSBefore.Enabled     := f;
    lbRxTSAfter.Enabled      := f;
    lbRxPacketTime.Enabled   := f;
    edRxTSBefore.Enabled     := f;
    edRxTSAfter.Enabled      := f;
    seRxTimeout.Enabled      := f;
    seRightEdge.Enabled      := cbShowRightEdge.Checked;
    seImageHeight.Enabled    := cbImageCustomSize.Checked;
    seImageWidth.Enabled     := cbImageCustomSize.Checked;
    seImageFontSize.Enabled  := not cbImageFontProp.Checked;
    lbImageDialogOne.Enabled := cbImageDialogOne.Checked;

    f := cbPlotLegendShow.Checked;

    cbPlotShowUntitled.Enabled   := f;
    cbPlotLegendActive.Enabled   := f;
    cbPlotLegendIndex.Enabled    := f;
    cbPlotLegendStyle.Enabled    := f;
    cbPlotLegendFrame.Enabled    := f;
    cbPlotLineReactivate.Enabled := f;
    cbPlotLegendColored.Enabled  := f;
    cbPlotLegendColoredB.Enabled := f and cbPlotLegendFrame.Checked;
    lbPlotLegendHint.Enabled     := f and cbPlotLegendActive.Checked;
  end;

procedure TfmSettings.ColorButtonsInit;
  var
    i:    Integer;
    item: TControl;
  begin
    // draw selector of color of lines
    for i := 0 to MAX_SERIES - 1 do
      begin

      // color button
      cbSerieColor[i]             := TColorButton.Create(fmSettings);
      cbSerieColor[i].Parent      := pSeriesColor;
      cbSerieColor[i].ButtonColor := DEFAULT_SERIE_COLOR[i];
      cbSerieColor[i].Flat        := True;
      cbSerieColor[i].BorderWidth := 0;
      cbSerieColor[i].Name        := Format('cbSerieColor%d', [i + 1]);
      cbSerieColor[i].Hint        := Format('        ' + TXT_COLOR_HINT, [i + 1]);
      cbSerieColor[i].Cursor      := crHandPoint;

      // appropriate label
      lbSerieColor[i]           := TLabel.Create(fmSettings);
      lbSerieColor[i].Parent    := pSeriesColor;
      lbSerieColor[i].Name      := Format('lbSerieColor%d', [i + 1]);
      lbSerieColor[i].Caption   := IntToStr(i + 1);
      lbSerieColor[i].Layout    := tlCenter;
      lbSerieColor[i].Alignment := taCenter;
      end;

    if appThemeAvailable then
      begin
      for item in [lbPlotColorLight, lbPlotColorTheme, lbPlotColorDark,
          colbPlotterBGd, colbPlotterBGWd, colbPlotterTextd,
          colbPlotterGXd, colbPlotterGYd] do
        item.Visible := True;
      end
    else
      begin
      pPlotColors.ChildSizing.ControlsPerLine   := 5;
      pPlotColors.ChildSizing.EnlargeHorizontal := crsAnchorAligning;

      for item in [lbPlotColorBG, lbPlotColorBGW, lbPlotColorText,
          lbPlotColorGX, lbPlotColorGY] do
        (item as TLabel).Alignment := taLeftJustify;
      end;
  end;


procedure TfmSettings.tvTabsSelectionChanged(Sender: TObject);
  var
    i: Integer;
  begin
    i := tvTabs.Selected.AbsoluteIndex;
    if i < pcPageCtrl.PageCount then
      pcPageCtrl.Pages[i].Show;
  end;

procedure TfmSettings.cbLanguageChange(Sender: TObject);
  begin
    // сохраняем позиции списков перед переводом
    Settings.ItemIndexBackup;

    if not LanguageChangeImmediately then Exit;

    // перерисовываем форму, чтобы более длинные метки полностью помещались
    FormShow(nil);

    // восстанавливаем позиции списков
    Settings.ItemIndexRestore;
  end;

procedure TfmSettings.seFontSizeChange(Sender: TObject);
  var
    i: Integer;
  begin
    BeginFormUpdate;
    Font.Height := 0;
    Font.Height := Round(Canvas.Font.GetTextHeight('0') * seFontSize.Value / 100);

    // for labels with custom font
    for i := 0 to ComponentCount - 1 do
      if Components[i].ClassName = TLabel.ClassName then
        TControl(Components[i]).Font.Height := Font.Height;

    EndFormUpdate;
  end;


procedure TfmSettings.edRegExpNameChange(Sender: TObject);
  begin
    acRegExpAdd.Enabled := not FREItem.Error and (edRegExpName.Text <> '');
  end;

procedure TfmSettings.seRegExpStrChange(Sender: TObject);
  begin
    FREItem.RegExp        := seRegExpStr.Text;
    pRegExpError.Visible  := FREItem.Error;
    lbRegExpError.Caption := FREItem.ErrorMsg;
    lbRegExpPos.Caption   := (seRegExpStr.SelStart - 1).ToString;
    edRegExpNameChange(Sender);
  end;

procedure TfmSettings.cbRegExpListChange(Sender: TObject);
  var
    i: Integer;
  begin
    i := FREList.IndexOf(cbRegExpList.Text);
    if i < 0 then Exit;

    with FREList.Items[i] do
      begin
      edRegExpName.Text  := Name;
      edRegExpLabel.Text := RELabel;
      edRegExpValue.Text := REValue;
      seRegExpStr.Text   := RegExp;
      end;
  end;

procedure TfmSettings.seRegExpStrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  begin
    seRegExpStrChange(Sender);
  end;


function TfmSettings.LanguageChangeImmediately: Boolean;
  var
    i:         Integer;
    lng, flng: String;
  begin
    LazGetLanguageIDs(lng, flng);

    // применяем язык интерфейса не выходя из настроек
    FLang := SetDefaultLang(FLangList[cbLanguage.ItemIndex], '', LANGUAGE_FILE);

    if FLang.Length = 2 then
      Result := FLang <> flng
    else
      Result := FLang <> lng;

    // translate tree view tabs
    for i := 0 to pcPageCtrl.PageCount - 1 do
      if i < tvTabs.Items.Count then
        tvTabs.Items.Item[i].Text := pcPageCtrl.Pages[i].Caption;
  end;


end.
