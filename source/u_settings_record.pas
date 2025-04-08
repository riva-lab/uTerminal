unit u_settings_record;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, AppSettings,
  u_common, u_plotter_types, u_plotter_regexplist;

type

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

      font: record
        index: Integer;
        size:  Integer;
        color: TColor;
        end;

      fontdark: record
        index: Integer;
        size:  Integer;
        color: TColor;
        end;
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

      font: record
        index: Integer;
        size:  Integer;
        color: TColor;
        end;

      fontdark: record
        index: Integer;
        size:  Integer;
        color: TColor;
        end;

      timestamp: record
        enable:  Boolean;
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
        bg:     TColor;
        bgwork: TColor;
        txt:    TColor;
        line:   array[0..MAX_SERIES - 1] of TColor;
        end;

      dark: record
        bg:     TColor;
        bgwork: TColor;
        txt:    TColor;
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
      color:   TColor;
      dark:    TColor;

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
      color: TColor;
      dark:  TColor;

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
  end;


const
  SETTINGS_FILE = 'settings.ini';


var
  Settings: TAppSettings; // class for work with settings
  cfg:      TAppConfig;   // configuration record with project settings


implementation


initialization
  Settings := TAppSettings.Create;

end.
