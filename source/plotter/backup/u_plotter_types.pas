unit u_plotter_types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, FPCanvas;


resourcestring
  PV_VIEW_STD    = 'Стандартный';
  PV_VIEW_2D     = '2D (X - первое значение)';
  PV_VIEW_WINDOW = 'Развертка';

  PCD_COMMA      = 'Запятая';
  PCD_SEMIC      = 'Точка с запятой';
  PCD_TAB        = 'Табуляция';
  PCD_SPACE      = 'Пробел';

  PCDD_DOT       = 'Точка';
  PCDD_COMMA     = 'Запятая';
  PCDD_SYS       = 'Определяется системой';

  PCQ_SINGLE     = 'Одинарные';
  PCQ_DOUBLE     = 'Двойные';

  PPS_SOLID      = 'Сплошная';
  PPS_DASH       = 'Штрихи';
  PPS_DOT        = 'Точки';
  PPS_DASHDOT    = 'Штрих-точка';
  PPS_DASHDOTDOT = 'Штрих-точка-точка';
  PPS_CLEAR      = 'Прозрачная';

const
  PP_RAW_U8      = 'Raw uint8   Big-Endian';
  PP_RAW_U16     = 'Raw uint16 Big-Endian';
  PP_RAW_U24     = 'Raw uint24 Big-Endian';
  PP_RAW_U32     = 'Raw uint32 Big-Endian';
  PP_RAW_I8      = 'Raw int8   Big-Endian';
  PP_RAW_I16     = 'Raw int16 Big-Endian';
  PP_RAW_I24     = 'Raw int24 Big-Endian';
  PP_RAW_I32     = 'Raw int32 Big-Endian';
  PP_ARDUINO     = 'Arduino Advanced';
  PP_REGEXP      = 'RegExp Universal';


type

  TDoublePoint = record
    X, Y: Double;
  end;

  TPlotterView = (
    pvStandard, pv2D, pvSweep);

  TPlotterProtocol = (
    ppArduino, ppRegExp,
    ppRawU8, ppRawU16, ppRawU24, ppRawU32,
    ppRawI8, ppRawI16, ppRawI24, ppRawI32
    );

  TPlotterCSVDelim = (
    pcdComma, pcdSemicolon, pcdTab, pcdSpace);

  TPlotterCSVDecDelim = (
    pcddDot, pcddComma, pcddSystem);

  TPlotterCSVQuotes = (
    pcqSingle, pcqDouble);

  TPlotterCSVLineBreak = (
    pcbUnix, pcbWindows, pcbMac);

  TPlotterPenStyle = (
    ppsSolid, ppsDash, ppsDot, ppsDashDot, ppsDashDotDot, ppsClear);

  TDoublePointArray = array of TDoublePoint;


const

  TXT_PLOTTER_PROTOCOL: array[TPlotterProtocol] of String = (
    PP_ARDUINO, PP_REGEXP,
    PP_RAW_U8, PP_RAW_U16, PP_RAW_U24, PP_RAW_U32,
    PP_RAW_I8, PP_RAW_I16, PP_RAW_I24, PP_RAW_I32);

  TXT_PLOTTER_VIEW: array[TPlotterView] of String = (
    PV_VIEW_STD, PV_VIEW_2D, PV_VIEW_WINDOW);

  TXT_PLOTTER_CSV_DELIM: array[TPlotterCSVDelim] of String = (
    PCD_COMMA, PCD_SEMIC, PCD_TAB, PCD_SPACE);

  TXT_PLOTTER_CSV_DECDELIM: array[TPlotterCSVDecDelim] of String = (
    PCDD_DOT, PCDD_COMMA, PCDD_SYS);

  TXT_PLOTTER_CSV_QUOTES: array[TPlotterCSVQuotes] of String = (
    PCQ_SINGLE, PCQ_DOUBLE);

  TXT_PLOTTER_PEN_STYLE: array[TPlotterPenStyle] of String = (
    PPS_SOLID, PPS_DASH, PPS_DOT, PPS_DASHDOT, PPS_DASHDOTDOT, PPS_CLEAR);


  PLOTTER_CSV_DELIM: array[TPlotterCSVDelim] of Char = (
    ',', ';', #9, ' ');

  PLOTTER_CSV_DECDELIM: array[TPlotterCSVDecDelim] of Char = (
    '.', ',', '-');

  PLOTTER_CSV_QUOTES: array[TPlotterCSVQuotes] of Char = (
    '''', '"');

  PLOTTER_PEN_STYLE: array[TPlotterPenStyle] of TFPPenStyle = (
    psSolid, psDash, psDot, psDashDot, psDashDotDot, psClear);

implementation

end.
