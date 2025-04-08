unit u_common;

{$mode objfpc}{$H+}

interface

resourcestring
  TXT_HELP_HINT       =
    'Здесь отображаются поля ввода и/или вывода данных.' + LineEnding + LineEnding
    + '--- Как работать с терминалом ---' + LineEnding
    + '1. Просканируйте систему на наличие доступных портов.' + LineEnding
    + '2. Выберите порт, задайте настройки, подключитесь.' + LineEnding
    + '3. Прием данных начнется сразу же после подключения к порту.' + LineEnding
    + '4. Можно задать ответные последовательности для приемника.' + LineEnding
    + '5. Передача данных возможна по команде.' + LineEnding
    + '6. Можно задать таймер автоматической передачи данных.' + LineEnding
    + '7. Плоттер позволит визуализировать принятые значения.' + LineEnding
    + '8. Почти все элементы интерфейса имеют подсказку - просто наведите курсор.';

  WARN_SETTINGS       =
    'Не удалось применить все настройки!' + LineEnding
    + 'Возникла критическая ошибка. Перезапустите приложение.' + LineEnding
    + 'Завершить работу приложения?';

  WARN_RESET          =
    'Приложение будет закрыто, а текущие настройки вернутся к значениям по умолчанию.' + LineEnding
    + 'Вы действительно хотите сбросить настройки?';

  WARN_LOAD           =
    'Данные из файла будут загружены в буфер передачи.' + LineEnding
    + 'Текущие данные будут утеряны. Продолжить?';

  WARN_UPDATE         =
    'Сейчас скачивается обновление.' + LineEnding +
    'Прервать и закрыть приложение?';

  TXT_TX_CAPTION      = 'Передача [Tx]';
  TXT_RX_CAPTION      = 'Прием [Rx]';
  TXT_PLOTTER_CAPTION = 'Плоттер';
  TXT_CONNECT         = 'Подключить';
  TXT_CONNECT_HINT    = 'Подключиться к выбранному порту';
  TXT_DISCONNECT_HINT = 'Отключиться от текущего порта';
  TXT_DISCONNECT      = 'Отключить';
  TXT_SPEED           = 'бод/с';
  TXT_PORTS_FINDED    = 'Найдено портов: %d';
  TXT_TYPE_TEXT       = 'Текст';
  TXT_ENCODING        = 'кодировка %s';
  TXT_DLG_TEXT        = 'Текстовый файл';
  TXT_DLG_CSV         = 'Файл CSV';
  TXT_DLG_PNG         = 'PNG - изображение Portable Network Graphics';
  TXT_DLG_ALL         = 'Файл';

  TXT_REDEF_VIEWPORT  = 'Окно';
  TXT_REDEF_GRID      = 'Сетка';
  TXT_REDEF_XGRID     = 'Сетка X';
  TXT_REDEF_YGRID     = 'Сетка Y';
  TXT_REDEF_BGCOLOR   = 'Фон';
  TXT_REDEF_WIDTH     = 'Толщина';
  TXT_REDEF_POINTS    = 'Точки';
  TXT_PLOT_LINE_ST    = 'Стиль линии - %s';
  TXT_PLOT_LINE_WD    = 'Толщина линии - %d пикс.';
  TXT_PLOT_LINE_PS    = 'Радиус точек - %d пикс.';
  TXT_COLOR_HINT      = 'Цвет %d-й линии плоттера';

  TXT_INSERT_CHAR     = '%s, символ <%s>';

  TXT_BYTE_SIZE       = 'Размер: %s';
  TXT_BYTE_SHORT      = 'Б';
  TXT_BYTE_KB         = 'КБ';
  TXT_BYTE_MB         = 'МБ';
  TXT_BYTE_GB         = 'ГБ';

  TXT_CONFIRM         = 'Подтверждение';
  TXT_WARNING         = 'Предупреждение';
  TXT_ERROR           = 'Ошибка';
  TXT_RESET           = 'Сброс';
  TXT_BAUDRATE_OTHER  = 'Другая';
  TXT_DATA            = 'Данные';
  TXT_POSITION        = 'Поз';
  TXT_SELECTION       = 'Выд';

  TXT_UPD_CHECKING    = 'Поиск обновлений...';
  TXT_UPD_NEWVER      = 'Доступна новая версия: v%d.%d.%d.%d (%s)';
  TXT_UPD_UPTODATE    = 'Приложение актуально. Обновление не требуется.';
  TXT_UPD_DOWNLOADING = 'Скачивание обновления, %s...';
  TXT_UPD_UNZIPPING   = 'Распаковка...';
  TXT_UPD_READY       = 'Все готово для обновления. Перезапустите приложение для завершения.';
  TXT_UPD_ERROR       = 'Произошла ошибка при обновлении. Повторите попытку еще раз.';

  TXT_LIST_ENTER_UNIX = 'LF      (0xA,         стиль Unix)';
  TXT_LIST_ENTER_WIN  = 'CRLF (0xD 0xA, стиль Windows)';
  TXT_LIST_ENTER_MAC  = 'CR     (0xD,         стиль MacOS)';

  TXT_LIST_LAYOUT_1   = 'Tx - вверху, Rx - внизу';
  TXT_LIST_LAYOUT_2   = 'Tx - внизу, Rx - вверху';
  TXT_LIST_LAYOUT_3   = 'Tx - слева, Rx - справа';
  TXT_LIST_LAYOUT_4   = 'Tx - справа, Rx - слева';

  TXT_THEME_SYS       = 'Системная';
  TXT_THEME_LIGHT     = 'Светлая';
  TXT_THEME_DARK      = 'Темная';

  TXT_CTRL_WHELL      = 'Колесо';
  TXT_CTRL_CTRLW      = 'Ctrl + колесо';
  TXT_CTRL_SHIFTW     = 'Shift + колесо';
  TXT_CTRL_RIGHTW     = 'Правая кнопка мыши + колесо';

  TXT_UPD_AUTO        = 'Автоматически';
  TXT_UPD_CHECK       = 'Только проверять';
  TXT_UPD_MANUAL      = 'Вручную';

  TXT_UPD_ATSTARTUP   = 'При запуске';
  TXT_UPD_DAYLY       = 'Раз в день';
  TXT_UPD_WEEKLY      = 'Раз в неделю';
  TXT_UPD_MONTHLY     = 'Раз в месяц';
  TXT_UPD_QUARTER     = 'Раз в квартал';

  HINT_CTS            = 'Готовность передачи';
  HINT_DSR            = 'Готовность источника данных';
  HINT_RING           = 'Сигнал вызова';
  HINT_RLSD           = 'Наличие несущей';
  HINT_RTS            = 'Запрос на передачу';
  HINT_DTR            = 'Готовность приемника данных';
  HINT_BREAK          = '(лог. 0 длительностью, указанной в настройках)';

  HINT_AUTOSEND       =
    'Если выбрано, передача будет инициироваться автоматически' + LineEnding
    + 'через заданные промежутки времени';

  HINT_AUTOANSWER     =
    'Если выбрано, то при получении данных будет автоматически' + LineEnding
    + 'отправлен ответ (если имеется, можно редактировать в настройках)';


type
  TDataView      = (dvTxt, dvHex, dvBin, dvDec);
  TPanelsLayout  = (plTxTop, plTxDown, plTxLeft, plTxRight);
  TAppUpdateWay  = (uwAuto, uwCheck, uwManual);
  TAppUpdateFreq = (ufAtStartup, ufDaily, ufWeekly, ufMonthly, ufQuarterly);
  TPlotterCtrl   = (pcWheel, pcCtrlWheel, pcShiftWheel, pcRightWheel);


const

  TXT_BAUDRATE: array[0..20] of String = (
    '110', '300', '600', '1200',
    '2400', '4800', '9600', '14400',
    '19200', '38400', '56000', '57600',
    '74880', '115200', '128000', '230400',
    '256000', '460800', '576000', '921600',
    TXT_BAUDRATE_OTHER);

  TXT_LINEBREAK: array[TTextLineBreakStyle] of String = (
    TXT_LIST_ENTER_UNIX, TXT_LIST_ENTER_WIN, TXT_LIST_ENTER_MAC);

  DATA_LINEBREAK: array[TTextLineBreakStyle] of String = (
    ''#10, ''#13#10, ''#13);

  TXT_DATA_VIEW: array[TDataView] of String = (
    TXT_TYPE_TEXT, 'HEX', 'BIN', 'DEC');

  TXT_PANELS_LAYOUT: array[TPanelsLayout] of String = (
    TXT_LIST_LAYOUT_1, TXT_LIST_LAYOUT_2, TXT_LIST_LAYOUT_3, TXT_LIST_LAYOUT_4);

  TXT_THEMES: array[0..2] of String = (
    TXT_THEME_SYS, TXT_THEME_LIGHT, TXT_THEME_DARK);

  TXT_CTRL_METHOD: array[TPlotterCtrl] of String = (
    TXT_CTRL_WHELL, TXT_CTRL_CTRLW, TXT_CTRL_SHIFTW, TXT_CTRL_RIGHTW);

  TXT_UPDATE_WAY: array[TAppUpdateWay] of String = (
    TXT_UPD_AUTO, TXT_UPD_CHECK, TXT_UPD_MANUAL);

  TXT_UPDATE_FREQ: array[TAppUpdateFreq] of String = (
    TXT_UPD_ATSTARTUP, TXT_UPD_DAYLY, TXT_UPD_WEEKLY, TXT_UPD_MONTHLY, TXT_UPD_QUARTER);

  CAppUpdateDays: array [TAppUpdateFreq] of Integer = (0, 1, 7, 30, 90);

  MAX_SERIES = 16; // макс. число линий плоттера

implementation

end.
