unit u_strings;

{$mode objfpc}{$H+}
{
  Файл содержит многострочные текстовые метки.
  Если используется перевод с помощью LCLTranslator, то
  нельзя использовать в свойствах компонентов и ресурсов
  обычные многострочные метки, т.к. они некорректно будут
  отображены в файле перевода *.po (актуально для Lazarus v1.6.4).
  Чтобы это исправить, используется метки в виде одной строки,
  а необходимые символы переноса или иные служебные экранируются.
  Для использования таких ресурсов в приложении нужно
  обращаться к ним через функцию MultiString, которая
  преобразует эти строки в обычные.
}

interface

resourcestring
  TXT_HELP_HINT =
    'Здесь отображаются поля ввода и/или вывода данных.\n\n' +
    '--- Как работать с терминалом ---\n' +
    '1. Просканируйте систему на наличие доступных портов.\n' +
    '2. Выберите порт, задайте настройки, подключитесь.\n' +
    '3. Прием данных начнется сразу же после подключения к порту.\n' +
    '4. Можно задать ответные последовательности для приемника.\n' +
    '5. Передача данных возможна по команде.\n' +
    '6. Можно задать таймер автоматической передачи данных.';

  WARN_SETTINGS = 'Не удалось применить все настройки!\n'
    + 'Возникла критическая ошибка. Перезапустите приложение.\n'
    + 'Завершить работу приложения?';

  WARN_RESET = 'При следующем запуске приложения будут загружены\n'
    + 'настройки по умолчанию. Продолжить?';

  WARN_LOAD = 'Данные из файла будут загружены в буфер передачи.\n'
    + 'Текущие данные будут утеряны. Продолжить?';

  TXT_CONFIRM  = 'Подтверждение';
  TXT_WARNING  = 'Предупреждение';
  TXT_ERROR    = 'Ошибка';
  TXT_RESET    = 'Сброс';
  TXT_BR_OTHER = 'Другая';
  TXT_DATA     = 'Данные';
  TXT_BYTE_POS = 'позиция';
  TXT_BYTE_SEL = 'выделено';

  TXT_LIST_DATABITS_5 = '5    бит данных в байте';
  TXT_LIST_DATABITS_6 = '6    бит данных в байте';
  TXT_LIST_DATABITS_7 = '7    бит данных в байте';
  TXT_LIST_DATABITS_8 = '8    бит данных в байте';

  TXT_LIST_PARBITS_N = 'None     * Отсутствует';
  TXT_LIST_PARBITS_O = 'Odd       * Дополнение до нечетности';
  TXT_LIST_PARBITS_E = 'Even      * Дополнение до четности';
  TXT_LIST_PARBITS_M = 'Mark     * Всегда 1';
  TXT_LIST_PARBITS_S = 'Space    * Всегда 0';

  TXT_LIST_STOPBITS_1  = '1      стоповый бит';
  TXT_LIST_STOPBITS_15 = '1,5   стоповых бита';
  TXT_LIST_STOPBITS_2  = '2      стоповых бита';

  TXT_LIST_ENTER_UNIX = 'LF      (0xA,         стиль Unix)';
  TXT_LIST_ENTER_WIN  = 'CRLF (0xD 0xA, стиль Windows)';
  TXT_LIST_ENTER_MAC  = 'CR     (0xD,         стиль MacOS)';

  TXT_LIST_LAYOUT_1 = 'Tx - вверху, Rx - внизу';
  TXT_LIST_LAYOUT_2 = 'Tx - внизу, Rx - вверху';
  TXT_LIST_LAYOUT_3 = 'Tx - слева, Rx - справа';
  TXT_LIST_LAYOUT_4 = 'Tx - справа, Rx - слева';

  TXT_LIST_CHART_RAW_8  = '8-битные данные';
  TXT_LIST_CHART_RAW_16 = '16-битные данные';
  TXT_LIST_CHART_RAW_24 = '24-битные данные';
  TXT_LIST_CHART_RAW_32 = '32-битные данные';
  TXT_LIST_CHART_HEX    = 'Данные в ASCII HEX';
  TXT_LIST_CHART_DEC    = 'Данные в ASCII DEC';

  HINT_CTS   = 'Clear to Send\nГотовность передачи';
  HINT_DSR   = 'Data Set Ready\nГотовность источника данных';
  HINT_RING  = 'Ring Indicator\nСигнал вызова';
  HINT_RLSD  = 'Receive Line Signal Detect\nили  Data Carrier Detect\nНаличие несущей';
  HINT_RTS   = 'Request to Send\nЗапрос на передачу';
  HINT_DTR   = 'Data Terminal Ready\nГотовность приемника данных';
  HINT_BREAK = 'Break\n(лог. 0 длительностью, указанной в настройках)';

  HINT_AUTOSEND = 'Если выбрано, передача будет инициироваться автоматически\n'
    + 'через заданные промежутки времени';

  HINT_AUTOANSWER = 'Если выбрано, то при получении данных будет автоматически\n'
    + 'отправлен ответ (если имеется, можно редактировать в настройках)';


function MultiString(input: String): String;

implementation

function MultiString(input: String): String;
  var
    i, f: Integer;
  begin
    Result := '';
    f      := 0;
    for i := 1 to Length(input) do
      begin
      if f = 1 then
        begin
        // https://en.wikipedia.org/wiki/Control_character
        // https://ru.wikipedia.org/wiki/Управляющие_символы
        case input[i] of
          '\': Result += '\';
          '0': Result += #0;
          'b': Result += #8;
          't': Result += #9;
          'n': Result += LineEnding;
          'r': Result += #13;
          'e': Result += #27;
          end;
        f := 0;
        Continue;
        end;

      if input[i] = '\' then
        f      := 1 else
        Result += input[i];
      end;
  end;

end.

