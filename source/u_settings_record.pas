unit u_settings_record;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  u_common, u_plotter_types, u_plotter_regexplist;

type

  { TAppConfiguration - Application settings structure }

  TAppConfiguration = record

    // настройки редактора
    editor: record
      linebreak:      TTextLineBreakStyle;
      linebreakIndex: Integer;
      tab:            Integer;
      quality:        Boolean;

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
        block: Integer;
        line:  Integer;
        end;
      end;

    // настройки подключения
    connect: record
      auto:     Boolean;
      check:    Boolean;
      hardflow: Boolean;
      end;

    // настройки передатчика
    tx: record
      breakTime: Integer;
      timeout:   Integer;
      restore:   Boolean;
      loadWarn:  Boolean;
      addition:  Boolean;

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
      limit:   Integer;
      restore: Boolean;

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
      end;

    // общие настройки приложения
    com: record
      lang:        String;
      langIndex:   Integer;
      layout:      TPanelsLayout;
      layoutIndex: Integer;
      iconsRes:    Integer;
      fontSize:    Integer;
      theme:       Integer;
      splash:      Boolean;
      glued:       Boolean;
      menu:        Boolean;
      status:      Boolean;
      tray:        Boolean;
      leds:        Boolean;
      encoding:    Boolean;
      RS232:       Boolean;

      update:
        record
        way:       TAppUpdateWay;
        wayIndex:  Integer;
        freq:      TAppUpdateFreq;
        freqIndex: Integer;
        end;
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
    re: record
      casecare: Boolean;
      list:     TPlotterRegExpList;
      end;
  end;


implementation

end.
