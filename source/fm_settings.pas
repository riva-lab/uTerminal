unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, Buttons, LazFileUtils, LazUTF8, SynEdit,
  AppTuner, AppSettings, AppLocalizer,
  fm_confirm,
  u_common, u_settings_record,
  u_plotter_types, u_plotter_regexplist, u_plotter;

resourcestring
  TXT_COLOR_HINT = 'Цвет %d-й линии плоттера';
  TXT_WIN_THEME  = 'Только Windows 10 1809+';

const
  LANGUAGES_DIR  = 'lang';
  LANGUAGES_FILE = 'languages.ini';
  LANGUAGE_FILE  = 'uTerminal';

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
    procedure ColorButtonsTranslate;
    procedure cbLanguageChange(Sender: TObject);
    procedure tmrUpdateTimer(Sender: TObject);
    procedure tvTabsSelectionChanged(Sender: TObject);
    procedure actionExecute(Sender: TObject);
    procedure edRegExpNameChange(Sender: TObject);
    procedure seRegExpStrChange(Sender: TObject);
    procedure cbRegExpListChange(Sender: TObject);
    procedure seRegExpStrKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormDestroy(Sender: TObject);

  private
    FREItem: TPlotterRegExpItem;

    procedure AddStandardSettings;
    procedure UpdateConfigSpecial;

    procedure OnLanguageChange(Sender: TObject);

  end;

  // public variables, available in other units
var
  fmSettings: TfmSettings;


implementation

var
  cbSerieColor: array[0..MAX_SERIES - 1] of TColorButton;
  lbSerieColor: array[0..MAX_SERIES - 1] of TLabel;


  {$R *.lfm}

  { TfmSettings }

procedure TfmSettings.FormCreate(Sender: TObject);
  begin
    {$IFDEF DEBUG}
    cbShowSplash.Checked := False;
    {$ENDIF}

    ColorButtonsInit;
    AddStandardSettings;

    appLocalizerEx.AddOnLanguageChangeHandler(@OnLanguageChange);
    appLocalizerEx.Load(
      Format('%0:s%1:s%0:s%2:s', [DirectorySeparator, LANGUAGES_DIR, LANGUAGES_FILE]),
      Format('%0:s%1:s%0:s%2:s', [DirectorySeparator, LANGUAGES_DIR, LANGUAGE_FILE]));

    cbLanguage.Items.SetStrings(appLocalizerEx.Languages);
    cbLanguage.ItemIndex := 0;

    // theme selector
    cbAppTheme.Items.AddStrings(CAppTheme);
    cbAppTheme.ItemIndex := Integer(appTunerEx.Theme);
    cbAppTheme.Enabled   := appTunerEx.IsDarkThemeAvailable;

    pcPageCtrl.ActivePageIndex := 0;
    pcPageCtrl.ShowTabs        := not tvTabs.Visible;
    seRegExpStr.Gutter.Visible := True;
    lbRegExpPos.Caption        := '0';

    // load app settings
    Settings.IniFile := appTunerEx.IniFile;
    Settings.SyncValues;     // load default values to fields of 'cfg' record
    Settings.Load;           // load settings from ini file to 'cfg' record

    // font default settings
    if cfg.tx.font.index < 0 then
      begin
      cfg.tx.font.index     := Max(0, Screen.Fonts.IndexOf('Consolas'));
      cfg.rx.font.index     := cfg.tx.font.index;
      cfg.tx.fontdark.index := cfg.tx.font.index;
      cfg.rx.fontdark.index := cfg.tx.font.index;
      end;

    // load font lists
    cbFontTxName.Items  := screen.Fonts;
    cbFontRxName.Items  := screen.Fonts;
    cbFontTxNameD.Items := screen.Fonts;
    cbFontRxNameD.Items := screen.Fonts;

    // load list of RegExp presets
    FREItem         := TPlotterRegExpItem.Create;
    cfg.regexp.list := TPlotterRegExpList.Create;
    cfg.regexp.list.Load(cfg.regexp.listData);

    cbRegExpList.Items.CommaText := cfg.regexp.list.CommaText;
    cbRegExpList.ItemIndex       := 0;

    UpdateConfigSpecial;

    Settings.SyncComponents; // load controls from fields of 'cfg' record
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
    begin
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
      if appTunerEx.IsDarkThemeAvailable then
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
    Settings.SyncComponents; // load controls from fields of 'cfg' record

    // execute this block only once
    if Tag = 0 then
      begin
      Tag := 1;

      Settings.SyncComponents;
      cbLanguageChange(Sender);
      end;

    BeginFormUpdate;
    AdjustComponentsSizes;
    DarkThemeSupport;
    EndFormUpdate;

    pcPageCtrl.Tag := pcPageCtrl.ActivePageIndex;
    if Assigned(Sender) then FormAutoSize;

    BeginFormUpdate;
    pcPageCtrl.ActivePageIndex        := pcPageCtrl.Tag;
    if Assigned(Sender) then Position := poMainFormCenter;
    EndFormUpdate;

    tmrUpdate.Enabled := True;

    // load current RegExp preset data to fields
    cbRegExpListChange(nil);
  end;

procedure TfmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    tmrUpdate.Enabled := False;
    if ModalResult <> mrOk then acCancel.Execute;
  end;

procedure TfmSettings.FormDestroy(Sender: TObject);
  begin
  end;


procedure TfmSettings.AddStandardSettings;
  var
    i: Integer;
  begin
    for i := 0 to MAX_SERIES - 1 do
      Settings.Add(cbSerieColor[i], @cfg.plt.color.line[i]);

    // add settings related to fm_settings
    {$Define inc_fm_settings}
    {$Include i_config.inc}
  end;

procedure TfmSettings.UpdateConfigSpecial;
  begin
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
      end;
  end;

procedure TfmSettings.OnLanguageChange(Sender: TObject);
  var
    i: Integer;
  begin
    BeginFormUpdate;

    appLocalizerEx.Localize(cbAppTheme, TXT_THEMES);
    appLocalizerEx.Localize(cbPanelsLayout, TXT_PANELS_LAYOUT);
    appLocalizerEx.Localize(cbAppUpdateWay, TXT_UPDATE_WAY);
    appLocalizerEx.Localize(cbAppUpdateFreq, TXT_UPDATE_FREQ);
    appLocalizerEx.Localize(cbLineBreakStyle, TXT_LINEBREAK);
    appLocalizerEx.Localize(cbCSVDelimiter, TXT_PLOTTER_CSV_DELIM);
    appLocalizerEx.Localize(cbCSVDecDelimiter, TXT_PLOTTER_CSV_DECDELIM);
    appLocalizerEx.Localize(cbCSVQuotes, TXT_PLOTTER_CSV_QUOTES);
    appLocalizerEx.Localize(cbCSVLineBreak, TXT_LINEBREAK);
    appLocalizerEx.Localize(cbPlotCtrlX, TXT_CTRL_METHOD);
    appLocalizerEx.Localize(cbPlotCtrlY, TXT_CTRL_METHOD);

    // translate tree view tabs
    for i := 0 to pcPageCtrl.PageCount - 1 do
      if i < tvTabs.Items.Count then
        tvTabs.Items.Item[i].Text := pcPageCtrl.Pages[i].Caption;

    ColorButtonsTranslate;
    EndFormUpdate;
  end;


procedure TfmSettings.actionExecute(Sender: TObject);
  begin
    case TComponent(Sender).Name of

      'acOK':
        begin
        Settings.SyncValues;
        UpdateConfigSpecial;
        appTunerEx.Theme := TAppTheme(cbAppTheme.ItemIndex);
        ModalResult      := mrOk;
        end;

      'acCancel':
        begin
        Settings.SyncComponents;
        ModalResult := mrCancel;
        end;

      'acRegExpAdd':
        begin
        if FREItem.Error then Exit;
        if cfg.regexp.list.Add(edRegExpName.Text, seRegExpStr.Text,
          edRegExpLabel.Text, edRegExpValue.Text) < 0 then Exit;

        cbRegExpList.Items.CommaText := cfg.regexp.list.CommaText;
        cbRegExpList.Text            := edRegExpName.Text;
        end;

      'acRegExpDel':
        begin
        cbRegExpList.Tag := cbRegExpList.ItemIndex;
        if cfg.regexp.list.Delete(cbRegExpList.Text) < 0 then Exit;

        cbRegExpList.Items.CommaText := cfg.regexp.list.CommaText;
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
      cbSerieColor[i].Cursor      := crHandPoint;

      // appropriate label
      lbSerieColor[i]           := TLabel.Create(fmSettings);
      lbSerieColor[i].Parent    := pSeriesColor;
      lbSerieColor[i].Name      := Format('lbSerieColor%d', [i + 1]);
      lbSerieColor[i].Caption   := IntToStr(i + 1);
      lbSerieColor[i].Layout    := tlCenter;
      lbSerieColor[i].Alignment := taCenter;
      end;

    if appTunerEx.IsDarkThemeAvailable then
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

    ColorButtonsTranslate;
  end;

procedure TfmSettings.ColorButtonsTranslate;
  var
    i: Integer;
  begin
    for i := 0 to MAX_SERIES - 1 do
      if Assigned(cbSerieColor[i]) then
        cbSerieColor[i].Hint := Format('        ' + TXT_COLOR_HINT, [i + 1]);
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
    appLocalizerEx.CurrentLanguage := cbLanguage.ItemIndex;

    appTunerEx.TuneComboboxes := False;
    appTunerEx.TuneComboboxes := True;  // force tuning
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
    i := cfg.regexp.list.IndexOf(cbRegExpList.Text);
    if i < 0 then Exit;

    with cfg.regexp.list.Items[i] do
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


end.
