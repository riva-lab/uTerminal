unit u_settings_record;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, AppSettings,
  u_common, u_plotter_types, u_plotter_regexplist;

type

  TThemeConfigFont = record
    index: Integer;
    size:  Integer;
    color: TColor;
  end;

  TThemeConfig = record
    ledInactive: TColor;

    editor: record
      lineSel:   TColor;
      rightEdge: TColor;
      end;

    txSeq: record
      acolor: TColor;
      bcolor: TColor;
      end;

    hint: record
      bg:  TColor;
      txt: TColor;
      end;

    font: record
      tx: TThemeConfigFont;
      rx: TThemeConfigFont;
      end;

    plotter: record
      ax:     TColor;
      ay:     TColor;
      bg:     TColor;
      bgwork: TColor;
      txt:    TColor;
      end;
  end;

  { TAppConfig - Application settings structure }

  TAppConfig = record

    // настройки редактора
    editor: record
      linebreak:      TTextLineBreakStyle;
      linebreakIndex: Integer;
      tab:            Integer;
      quality:        Boolean;
      lineCounts:     Boolean;

      view: record
        size:    Boolean;
        inBytes: Boolean;
        pos:     Boolean;
        end;

      right: record
        pos:    Integer;
        enable: Boolean;
        end;

      hex: record
        enable: Boolean;
        block:  Integer;
        line:   Integer;
        end;
      end;

    // настройки подключения
    connect: record
      auto:     Boolean;
      check:    Boolean;
      hardflow: Boolean;
      opened:   Boolean;
      signals:  Boolean;
      port:     String;
      baudId:   Integer;
      baudrate: Integer;

      dataBits: record
        db5: Boolean;
        db6: Boolean;
        db7: Boolean;
        db8: Boolean;
        end;

      parityBits: record
        e: Boolean;
        m: Boolean;
        n: Boolean;
        o: Boolean;
        s: Boolean;
        end;

      stopBits: record
        sb1:  Boolean;
        sb1h: Boolean;
        sb2:  Boolean;
        end;
      end;

    // настройки передатчика
    tx: record
      breakTime: Integer;
      timeout:   Integer;
      encoding:  Integer;
      view:      Integer;
      showBox:   Boolean;
      cmdEnable: Boolean;
      seqEnable: Boolean;
      restore:   Boolean;
      loadWarn:  Boolean;
      addition:  Boolean;
      sequences: String;
      buffer:    String;
      end;

    // настройки приемника
    rx: record
      limit:     Integer;
      encoding:  Integer;
      view:      Integer;
      showBox:   Boolean;
      enable:    Boolean;
      restore:   Boolean;
      scrollEnd: Boolean;
      buffer:    String;

      timestamp: record
        enable:  Boolean;
        date:    Boolean;
        time:    Boolean;
        size:    Boolean;
        timeout: Integer;
        after:   String;
        before:  String;
        end;

      answer: record
        enable: Boolean;
        input:  String;
        output: String;
        end;
      end;

    // общие настройки приложения
    com: record
      langIndex:   Integer;
      layout:      TPanelsLayout;
      layoutIndex: Integer;
      iconsRes:    Integer;
      fontSize:    Integer;
      theme:       Integer;
      splitter:    Double;
      glued:       Boolean;
      menu:        Boolean;
      status:      Boolean;
      tray:        Boolean;
      leds:        Boolean;
      encoding:    Boolean;
      border:      Boolean;
      RS232:       Boolean;

      update: record
        way:       TAppUpdateWay;
        wayIndex:  Integer;
        freq:      TAppUpdateFreq;
        freqIndex: Integer;
        lastTime:  Int64;
        end;
      end;

    // toolbars visibility
    toolbar: record
      main: Boolean;
      port: Boolean;
      rx:   Boolean;
      tx:   Boolean;
      end;

    // search settings
    search: record
      enable:     Boolean;
      caseSense:  Boolean;
      regex:      Boolean;
      replace:    Boolean;
      replaceAll: Boolean;
      end;

    // настройки плоттера общие
    plt: record
      recolor:    Boolean;
      reactivate: Boolean;
      copyRx:     Boolean;
      clearRx:    Boolean;
      commands:   Boolean;
      smooth:     Boolean;
      size:       Boolean;
      live:       Boolean;
      tracker:    Boolean;
      settings:   Boolean;
      enable:     Boolean;
      viewport:   Integer;
      viewmode:   Integer;
      protocol:   Integer;

      pen: record
        thickness: Integer;
        style:     Integer;
        pointSize: Integer;
        end;

      view: record
        minimap:     Boolean;
        panelOnMain: Boolean;
        end;

      color: record
        line: array[0..MAX_SERIES - 1] of TColor;
        end;
      end;

    // настройки оси X плоттера
    ax: record
      grid:    Boolean;
      marks:   Boolean;
      labels:  Boolean;
      counter: Boolean;
      samples: Integer;
      space:   Integer;

      ctrl: record
        method:      TPlotterCtrl;
        methodIndex: Integer;
        factor:      Double;
        bar:         Boolean;
        end;
      end;

    // настройки оси Y плоттера
    ay: record
      grid:  Boolean;
      marks: Boolean;

      ctrl: record
        method:      TPlotterCtrl;
        methodIndex: Integer;
        factor:      Double;
        end;

      offset: record
        t: Integer;
        b: Integer;
        end;
      end;

    // настройки легенды плоттера
    legend: record
      colored:      Boolean;
      coloredframe: Boolean;
      frame:        Boolean;
      index:        Boolean;
      style:        Boolean;
      interactive:  Boolean;
      enable:       Boolean;
      untitled:     Boolean;
      end;

    // настройки плоттера для экспорта в CSV
    csv: record
      delimiter:      TPlotterCSVDelim;
      delimiterIndex: Integer;
      decimal:        TPlotterCSVDecDelim;
      decimalIndex:   Integer;
      quotes:         TPlotterCSVQuotes;
      quotesIndex:    Integer;
      linebreak:      TTextLineBreakStyle;
      linebreakIndex: Integer;
      end;

    // настройки плоттера для экспорта в PNG
    png: record
      silent: Boolean;
      w:      Integer;
      h:      Integer;
      custom: Boolean;

      font: record
        prop: Boolean;
        size: Integer;
        end;
      end;

    // настройки формата плоттера RegExp
    regexp: record
      selected: Integer;
      casecare: Boolean;
      listData: String;
      list:     TPlotterRegExpList;
      end;

    // theme settings: colors & fonts
    theme: record
      this:  TThemeConfig; // current
      light: TThemeConfig;
      dark:  TThemeConfig;
      end;
  end;


const
  SETTINGS_FILE = 'settings.ini';


var
  Settings: TAppSettings; // class for work with settings
  cfg:      TAppConfig;   // configuration record with project settings


procedure ConfigInitDefaults;


implementation


procedure ConfigInitDefaults;
  const
    // default colors for plotter series
    DEFAULT_SERIE_COLOR: array[0..MAX_SERIES - 1] of TColor =
      ($FF8000, $00D000, clRed, $C00000, clFuchsia, $0080FF, clGreen, $00E0E0,
      $FF8000, $00D000, clRed, $C00000, clFuchsia, $0080FF, clGreen, $00E0E0);
  begin
    with cfg.theme do
      begin
      light.hint.bg          := $DCF3D1;
      light.hint.txt         := clWindowText;
      light.ledInactive      := TColor(clDefault - $444444);
      light.txSeq.acolor     := $CFE8C6;
      light.txSeq.bcolor     := $E7F2E1;
      light.editor.lineSel   := TColor(clWindow - $060606);
      light.editor.rightEdge := TColor(clWindow - $222222);

      dark.hint.bg          := $497634;
      dark.hint.txt         := clWindowText;
      dark.ledInactive      := TColor(clDefault + $444444);
      dark.txSeq.acolor     := $2E4921;
      dark.txSeq.bcolor     := $263D1B;
      dark.editor.lineSel   := TColor(clWindow + $060606);
      dark.editor.rightEdge := TColor(clWindow + $222222);
      end;

    cfg.plt.color.line := DEFAULT_SERIE_COLOR;
  end;


initialization
  Settings := TAppSettings.Create;

end.
