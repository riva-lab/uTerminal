unit fm_settings;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Graphics, Dialogs, ComCtrls, Spin, StdCtrls,
  ExtCtrls, ActnList, Controls, IniPropStorage, LCLTranslator, Buttons,
  LazFileUtils, LazUTF8, u_encodings, u_utilities, u_strings;

const
  LANGUAGE_DEFAULT = 'RU, Russian - Русский';
  LANGUAGES_DIR    = DirectorySeparator + 'lang';
  LANGUAGES_FILE   = LANGUAGES_DIR + DirectorySeparator + 'languages.ini';
  SETTINGS_FILE    = DirectorySeparator + 'settings.ini';
  TXT_EXAMPLE      = 'Text Example, 12345 - Пример АБВ';

type
  TPanelsLayout = (plTxTop = 0, plTxDown, plTxLeft, plTxRight);

  { TfmSettings }

  TfmSettings = class(TForm)
    cbLineBreakStyle:    TComboBox;
    cbAutoconnect:       TCheckBox;
    cbFileLoadWarning:   TCheckBox;
    cbFileDataAdd:       TCheckBox;
    cbTxRestore:         TCheckBox;
    cbRxRestore:         TCheckBox;
    cbShowPosAndSel:     TCheckBox;
    cbShowEncoding:      TCheckBox;
    cbShowRS232Captions: TCheckBox;
    edFontTxExample:     TEdit;
    edFontRxExample:     TEdit;

    IniStorageSettings:  TIniPropStorage;
    lbLineBreakStyle:    TLabel;
    lbHEXBlockBytes:     TLabel;
    lbRxSizeLimit:       TLabel;
    lbTxDeadlockTimeout: TLabel;
    lbHEXLineBytes:      TLabel;
    pValues3:            TPanel;
    seRxSizeLimit:       TSpinEdit;
    SettingsActionList:  TActionList;
    acCancel:            TAction;
    acOK:                TAction;
    bbApply:             TBitBtn;
    bbCancel:            TBitBtn;
    bbDefaults:          TBitBtn;
    btnFontRx:           TButton;
    btnFontTx:           TButton;
    cbLanguage:          TComboBox;
    cbPanelsLayout:      TComboBox;
    cbFontRxName:        TComboBox;
    cbFontTxName:        TComboBox;
    cbCheckPort:         TCheckBox;
    cbFontQuality:       TCheckBox;
    cbHardflow:          TCheckBox;
    cbMinimizeToTray:    TCheckBox;
    cbRxTimestamp:       TCheckBox;
    cbShowIndicators:    TCheckBox;
    cbShowMenu:          TCheckBox;
    cbShowRightEdge:     TCheckBox;
    cbShowSizes:         TCheckBox;
    cbShowSplash:        TCheckBox;
    cbShowStatusBar:     TCheckBox;
    dlgFontRx:           TFontDialog;
    dlgFontTx:           TFontDialog;
    edRxTSAfter:         TEdit;
    edRxTSBefore:        TEdit;
    lbFontRx:            TLabel;
    lbFontTx:            TLabel;
    lbLanguage:          TLabel;
    lbPanelsLayout:      TLabel;
    lbRxPacketTime:      TLabel;
    lbRxTSAfter:         TLabel;
    lbRxTSBefore:        TLabel;
    lbTabSize:           TLabel;
    lbTxBreakTime:       TLabel;
    pButtons:            TPanel;
    pControls:           TPanel;
    pLanguage:           TPanel;
    pNaviColumns:        TPanel;
    pValues:             TPanel;
    pValues1:            TPanel;
    pValues2:            TPanel;
    seFontRxSize:        TSpinEdit;
    seFontTxSize:        TSpinEdit;
    seRightEdge:         TSpinEdit;
    seRxTimeout:         TSpinEdit;
    seTabSize:           TSpinEdit;
    seTxBreakTime:       TSpinEdit;
    PageCtrl:            TPageControl;
    seHEXLineBytes:      TSpinEdit;
    seTxDeadlockTimeout: TSpinEdit;
    seHEXBlockBytes:     TSpinEdit;
    tsFont:              TTabSheet;
    tsHEX:               TTabSheet;
    tsConnection:        TTabSheet;
    tsEditor:            TTabSheet;
    tsGeneral:           TTabSheet;
    tsRx:                TTabSheet;
    tsTx:                TTabSheet;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure IniStorageSettingsRestore(Sender: TObject);
    procedure IniStorageSettingsSavingProperties(Sender: TObject);
    procedure acOKExecute(Sender: TObject);
    procedure acCancelExecute(Sender: TObject);
    procedure btnFontClick(Sender: TObject);
    procedure cbFontChange(Sender: TObject);
    procedure cbLanguageChange(Sender: TObject);
    procedure TranslateComboBoxes;

  PRIVATE
    FLanguageIndex:     Integer;
    FLineBreakStyle:    TTextLineBreakStyle;
    FShowRS232Captions: Boolean;
    FTabSize:           Integer;
    FRightEdge:         Integer;
    FBreakTime:         Integer;
    FDeadlockTimeout:   Integer;
    FTimeoutRx:         Integer;
    FRxSizeLimit:       Integer;
    FHEXBlockBytes:     Integer;
    FHEXLineBytes:      Integer;
    FFontTx:            TFont;
    FFontRx:            TFont;
    FCheckPort:         Boolean;
    FFontQuality:       Boolean;
    FHardflow:          Boolean;
    FAutoconnect:       Boolean;
    FMinToTray:         Boolean;
    FShowIndicators:    Boolean;
    FShowMenu:          Boolean;
    FShowEncoding:      Boolean;
    FShowRightEdge:     Boolean;
    FShowSizes:         Boolean;
    FShowStatusBar:     Boolean;
    FSplash:            Boolean;
    FLoadWarning:       Boolean;
    FFileDataAdding:    Boolean;
    FTimestamp:         Boolean;
    FTxRestore:         Boolean;
    FRxRestore:         Boolean;
    FShowPosAndSel:     Boolean;
    FLanguage:          String;
    FTSAfter:           String;
    FTSBefore:          String;
    FPanelsLayout:      TPanelsLayout;

    procedure IniStorageLangLoad;
    procedure LoadComponentsFromFields;
    procedure LoadFieldsFromComponents;

  PUBLIC
    procedure LanguageChangeImmediately;

    property Language: String read FLanguage;
    property PanelsLayout: TPanelsLayout read FPanelsLayout;

    property FontTx: TFont read FFontTx;
    property FontRx: TFont read FFontRx;
    property FontQuality: Boolean read FFontQuality;

    property Splash: Boolean read FSplash;
    property LoadWarning: Boolean read FLoadWarning;
    property FileDataAdding: Boolean read FFileDataAdding;
    property MinToTray: Boolean read FMinToTray;
    property ShowMenu: Boolean read FShowMenu;
    property ShowEncoding: Boolean read FShowEncoding;
    property ShowRS232Captions: Boolean read FShowRS232Captions;
    property ShowStatusBar: Boolean read FShowStatusBar;
    property ShowRightEdge: Boolean read FShowRightEdge;
    property RightEdge: Integer read FRightEdge;
    property TabSize: Integer read FTabSize;
    property LineBreakStyle: TTextLineBreakStyle read FLineBreakStyle;

    property ShowSizes: Boolean read FShowSizes;
    property ShowIndicators: Boolean read FShowIndicators;
    property CheckPort: Boolean read FCheckPort;
    property Hardflow: Boolean read FHardflow;
    property Autoconnect: Boolean read FAutoconnect;

    property ShowPosAndSel: Boolean read FShowPosAndSel;
    property BreakTime: Integer read FBreakTime;
    property DeadlockTimeout: Integer read FDeadlockTimeout;
    property Timestamp: Boolean read FTimestamp;
    property TSBefore: String read FTSBefore;
    property TSAfter: String read FTSAfter;
    property TimeoutRx: Integer read FTimeoutRx;
    property RxSizeLimit: Integer read FRxSizeLimit;
    property TxRestore: Boolean read FTxRestore;
    property RxRestore: Boolean read FRxRestore;

    property HEXLineBytes: Integer read FHEXLineBytes;
    property HEXBlockBytes: Integer read FHEXBlockBytes;

  end;

var
  fmSettings:     TfmSettings;
  IniStorageLang: TIniPropStorage;


implementation

{$R *.lfm}

{ TfmSettings }

procedure TfmSettings.FormCreate(Sender: TObject);
  begin
    FFontTx := TFont.Create;
    FFontRx := TFont.Create;

    edFontTxExample.Caption := 'Tx: ' + TXT_EXAMPLE;
    edFontRxExample.Caption := 'Rx: ' + TXT_EXAMPLE;

    IniStorageSettings.IniFileName := ExtractFileDir(ParamStrUTF8(0)) + SETTINGS_FILE;
    PageCtrl.ActivePageIndex       := 0;

    IniStorageLangLoad;
  end;

procedure TfmSettings.FormShow(Sender: TObject);
  var
    i, tmp: Integer;
  begin
    BeginFormUpdate;

    TranslateComboBoxes;

    if edFontTxExample.Caption = '' then
      edFontTxExample.Caption := 'Tx: ' + TXT_EXAMPLE;

    if edFontRxExample.Caption = '' then
      edFontRxExample.Caption := 'Rx: ' + TXT_EXAMPLE;

    if Sender <> nil then
      LoadComponentsFromFields;

    if Sender <> nil then
      Position := poDefault;

    EndFormUpdate;

    tmp := PageCtrl.ActivePageIndex;

    for i := 1 to PageCtrl.PageCount do
      begin
      BeginFormUpdate;
      PageCtrl.ActivePageIndex := i - 1;
      AutoSize                 := True;
      EndFormUpdate;

      Constraints.MinWidth  := Width;
      Constraints.MinHeight := Height;

      AutoSize := False;
      end;

    BeginFormUpdate;

    if Sender <> nil then
      Position := poMainFormCenter;

    PageCtrl.ActivePageIndex := tmp;
    cbFontTxName.Items       := screen.Fonts;
    cbFontRxName.Items       := screen.Fonts;

    cbLineBreakStyle.ItemWidth := GetListStringsMaxWidth(Self, cbLineBreakStyle.Items);
    cbLanguage.ItemWidth       := GetListStringsMaxWidth(Self, cbLanguage.Items);
    cbFontRxName.ItemWidth     := GetListStringsMaxWidth(Self, cbFontRxName.Items);
    cbFontTxName.ItemWidth     := GetListStringsMaxWidth(Self, cbFontTxName.Items);
    cbPanelsLayout.ItemWidth   := GetListStringsMaxWidth(Self, cbPanelsLayout.Items);

    EndFormUpdate;
  end;

procedure TfmSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
  begin
    if ModalResult <> mrOk then acCancel.Execute;
  end;


procedure TfmSettings.LoadComponentsFromFields;
  begin
    cbLanguage.ItemIndex        := FLanguageIndex;
    cbPanelsLayout.ItemIndex    := Integer(FPanelsLayout);
    cbShowSplash.Checked        := FSplash;
    cbFileLoadWarning.Checked   := FLoadWarning;
    cbFileDataAdd.Checked       := FFileDataAdding;
    cbMinimizeToTray.Checked    := FMinToTray;
    cbShowMenu.Checked          := FShowMenu;
    cbShowEncoding.Checked      := FShowEncoding;
    cbShowRS232Captions.Checked := FShowRS232Captions;
    cbShowStatusBar.Checked     := FShowStatusBar;
    cbShowRightEdge.Checked     := FShowRightEdge;
    seRightEdge.Value           := FRightEdge;
    seTabSize.Value             := FTabSize;
    cbLineBreakStyle.ItemIndex  := Integer(FLineBreakStyle);
    cbFontQuality.Checked       := FFontQuality;
    cbShowSizes.Checked         := FShowSizes;
    cbShowIndicators.Checked    := FShowIndicators;
    cbCheckPort.Checked         := FCheckPort;
    cbHardflow.Checked          := FHardflow;
    cbAutoconnect.Checked       := FAutoconnect;
    cbShowPosAndSel.Checked     := FShowPosAndSel;
    seTxBreakTime.Value         := FBreakTime;
    seTxDeadlockTimeout.Value   := FDeadlockTimeout;
    cbTxRestore.Checked         := FTxRestore;
    cbRxRestore.Checked         := FRxRestore;
    cbRxTimestamp.Checked       := FTimestamp;
    edRxTSBefore.Text           := FTSBefore;
    edRxTSAfter.Text            := FTSAfter;
    seRxTimeout.Value           := FTimeoutRx;
    seRxSizeLimit.Value         := FRxSizeLimit div 1000;
    seHEXBlockBytes.Value       := FHEXBlockBytes;
    seHEXLineBytes.Value        := FHEXLineBytes;

    if FFontTx <> nil then dlgFontTx.Font.Assign(FFontTx);
    if FFontRx <> nil then dlgFontRx.Font.Assign(FFontRx);
    btnFontClick(nil);
  end;

procedure TfmSettings.LoadFieldsFromComponents;
  begin
    FLanguageIndex     := cbLanguage.ItemIndex;
    FLanguage          := LowerCase(Copy(cbLanguage.Text, 1, 2));
    FSplash            := cbShowSplash.Checked;
    FLoadWarning       := cbFileLoadWarning.Checked;
    FFileDataAdding    := cbFileDataAdd.Checked;
    FMinToTray         := cbMinimizeToTray.Checked;
    FShowMenu          := cbShowMenu.Checked;
    FShowEncoding      := cbShowEncoding.Checked;
    FShowRS232Captions := cbShowRS232Captions.Checked;
    FShowStatusBar     := cbShowStatusBar.Checked;
    FShowRightEdge     := cbShowRightEdge.Checked;
    FRightEdge         := seRightEdge.Value;
    FTabSize           := seTabSize.Value;
    FLineBreakStyle    := TTextLineBreakStyle(cbLineBreakStyle.ItemIndex);
    FFontQuality       := cbFontQuality.Checked;
    FShowSizes         := cbShowSizes.Checked;
    FShowIndicators    := cbShowIndicators.Checked;
    FCheckPort         := cbCheckPort.Checked;
    FHardflow          := cbHardflow.Checked;
    FAutoconnect       := cbAutoconnect.Checked;
    FShowPosAndSel     := cbShowPosAndSel.Checked;
    FBreakTime         := seTxBreakTime.Value;
    FDeadlockTimeout   := seTxDeadlockTimeout.Value;
    FTxRestore         := cbTxRestore.Checked;
    FRxRestore         := cbRxRestore.Checked;
    FTimestamp         := cbRxTimestamp.Checked;
    FTSBefore          := edRxTSBefore.Text;
    FTSAfter           := edRxTSAfter.Text;
    FTimeoutRx         := seRxTimeout.Value;
    FRxSizeLimit       := seRxSizeLimit.Value * 1000;
    FPanelsLayout      := TPanelsLayout(cbPanelsLayout.ItemIndex);
    FHEXBlockBytes     := seHEXBlockBytes.Value;
    FHEXLineBytes      := seHEXLineBytes.Value;

    cbFontChange(nil);
  end;


procedure TfmSettings.IniStorageSettingsRestore(Sender: TObject);
  begin
    LoadFieldsFromComponents;
  end;

procedure TfmSettings.IniStorageSettingsSavingProperties(Sender: TObject);
  begin
    LoadComponentsFromFields;
  end;

procedure TfmSettings.IniStorageLangLoad;
  var
    i, cnt: Integer;
  begin
    cbLanguage.Clear;
    cbLanguage.Items.Append(LANGUAGE_DEFAULT);

    IniStorageLang := TIniPropStorage.Create(nil);
    with IniStorageLang do
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

      // считываем список локализаций, кроме 1-го пункта (язык по умолчанию)
      cnt := ReadInteger('Count', 1);
      cbLanguage.ItemIndex := 0;

      if cnt > 1 then
        for i := 2 to cnt do
          cbLanguage.Items.Append(ReadString('L-' + i.ToString, ''));
      end;
  end;


procedure TfmSettings.acOKExecute(Sender: TObject);
  begin
    LoadFieldsFromComponents;

    ModalResult := mrOk;
  end;

procedure TfmSettings.acCancelExecute(Sender: TObject);
  begin
    LoadComponentsFromFields;

    ModalResult := mrCancel;
  end;


procedure TfmSettings.btnFontClick(Sender: TObject);
  var
    dlg: TFontDialog;
  begin
    if Sender <> nil then
      begin
      case TComponent(Sender).Name of
        'btnFontTx': dlg := dlgFontTx;
        'btnFontRx': dlg := dlgFontRx;
        end;

      with dlg do
        if Execute then
          case TComponent(Sender).Name of
            'btnFontTx': dlgFontTx := dlg;
            'btnFontRx': dlgFontRx := dlg;
            end;
      end;

    cbFontTxName.Text  := dlgFontTx.Font.Name;
    seFontTxSize.Value := dlgFontTx.Font.Size;
    cbFontRxName.Text  := dlgFontRx.Font.Name;
    seFontRxSize.Value := dlgFontRx.Font.Size;

    cbFontChange(nil);
  end;

procedure TfmSettings.cbFontChange(Sender: TObject);
  begin
    if Sender <> nil then
      case TComponent(Sender).Name of
        'cbFontTxName': dlgFontTx.Font.Name := cbFontTxName.Text;
        'cbFontRxName': dlgFontRx.Font.Name := cbFontRxName.Text;
        'seFontTxSize': dlgFontTx.Font.Size := seFontTxSize.Value;
        'seFontRxSize': dlgFontRx.Font.Size := seFontRxSize.Value;
        end;

    FFontTx.Assign(dlgFontTx.Font);
    FFontRx.Assign(dlgFontRx.Font);

    FFontTx.Quality := CheckBoolean(cbFontQuality.Checked, fqCleartypeNatural, fqNonAntialiased);
    FFontRx.Quality := CheckBoolean(cbFontQuality.Checked, fqCleartypeNatural, fqNonAntialiased);

    edFontTxExample.Font.Assign(FFontTx);
    edFontRxExample.Font.Assign(FFontRx);
  end;


procedure TfmSettings.cbLanguageChange(Sender: TObject);
  begin
    LanguageChangeImmediately;

    // перерисовываем форму, чтобы более длинные метки полностью помещались
    FormShow(nil);
  end;

procedure TfmSettings.TranslateComboBoxes;
  begin
    ComboBoxUpdateList(cbLineBreakStyle, [
      TXT_LIST_ENTER_UNIX, TXT_LIST_ENTER_WIN, TXT_LIST_ENTER_MAC]);

    ComboBoxUpdateList(cbPanelsLayout, [
      TXT_LIST_LAYOUT_1, TXT_LIST_LAYOUT_2, TXT_LIST_LAYOUT_3, TXT_LIST_LAYOUT_4]);
  end;


procedure TfmSettings.LanguageChangeImmediately;
  var
    strTxEx, strRxEx: String;
  begin
    // примеры текста не надо переводить, запоминаем
    strTxEx := edFontTxExample.Caption;
    strRxEx := edFontRxExample.Caption;

    // применяем язык интерфейса не выходя из настроек
    SetDefaultLang(LowerCase(Copy(cbLanguage.Text, 1, 2)),
      ExtractFileDir(ParamStrUTF8(0)) + LANGUAGES_DIR);

    // восстанавиваем примеры текста
    edFontTxExample.Caption := strTxEx;
    edFontRxExample.Caption := strRxEx;
  end;


end.
