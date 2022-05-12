unit u_utilities;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, LazUTF8, ShellApi, strutils, Graphics, Forms,
  StdCtrls;

resourcestring
  BYTE_SINGLE     = 'байт';
  BYTE_MULTIPLE_1 = 'байт';
  BYTE_MULTIPLE_2 = 'байта';
  BYTE_MULTIPLE_5 = 'байт';


// удаление указанного файла, по умолчанию - в корзину
function FileRemove(AFileName: String; toTrash: Boolean = True): Boolean;

// открытие указанного пути в проводнике
procedure RootOpenInExplorer(ARoot: String);


// вывод массива байт в строку в HEX виде
function BytesToHex(AData: TBytes; ABefore: String = '0x'; AAfter: String = ' ';
  ABytesPerString: Byte = 8): String;

function StringToHex(AText: String; ALineBytes: Integer = 8; ABlockBytes: Integer = 4): String;

function StrToCodes(AString: String; ASysOut: Integer; ALineChars: Integer): String;

function CodesToStr(AData: String; ASysIn, ASysOut: Integer): String;

function SizeStr(AText: String): String;

function HexStringPosition(ACursor: Integer; ALineBytes: Integer = 8;
  ABlockBytes: Integer = 4): Integer;


function CheckBoolean(ABool: Boolean; ValueTrue, ValueFalse: Variant): Variant;

function CheckBooleanPtr(ABool: Boolean; ValueTrue, ValueFalse: Pointer): Pointer;

// проверяет значение переменной на вхождение в диапазон
function InRange(AValue, AMin, AMax: Variant): Boolean;

// проверка расширения файла, регистронезависимая
function FileExtCheck(AFilename, AExtList: String): Boolean;

// получение максимальной ширины строк списка в пикселях
function GetListStringsMaxWidth(AForm: TForm; AItems: TStrings): Integer;

// заполнение выпадающего списка строками из массива
procedure ComboBoxUpdateList(AComboBox: TComboBox; AStrings: array of String);


implementation

function FileRemove(AFileName: String; toTrash: Boolean): Boolean;
  var
    FileOp: TSHFileOpStruct;
  begin
    Result := False;
    if not FileExistsUTF8(AFileName) then Exit;
    if not toTrash then Exit(DeleteFileUTF8(AFileName));

    if AFileName <> '' then
      begin
      FillChar(FileOp, SizeOf(FileOp), 0);
      FileOp.Wnd    := 0;
      FileOp.wFunc  := FO_DELETE;
      FileOp.pFrom  := PChar(UTF8ToWinCP(AFileName) + #0#0);
      FileOp.pTo    := nil;
      FileOp.fFlags := FOF_ALLOWUNDO or FOF_NOERRORUI or FOF_SILENT; // or FOF_NOCONFIRMATION;
      Result        := (SHFileOperation(FileOp) = 0) and (not
        FileOp.fAnyOperationsAborted);
      end;
  end;

procedure RootOpenInExplorer(ARoot: String);
  begin
    if ARoot = '' then
      ARoot := ParamStrUTF8(0);

    if not DirectoryExistsUTF8(ARoot) then
      ARoot := ExtractFileDir(ARoot);

    if DirectoryExistsUTF8(ARoot) then
    {$IfDef WINDOWS}
      ExecuteProcess('explorer.exe', UTF8ToWinCP(ARoot + DirectorySeparator), []);
    {$Else}
    ;
    {$EndIf}
  end;


function BytesToHex(AData: TBytes; ABefore: String; AAfter: String; ABytesPerString: Byte): String;
  var
    i: Integer;
  begin
    Result := '';

    if Length(AData) > 0 then
      for i := 0 to High(AData) do
        begin
        Result += ABefore + IntToHex(AData[i], 2) + AAfter;

        if (ABytesPerString > 0) and (i mod ABytesPerString = ABytesPerString - 1) then
          Result += #13#10;
        end;
  end;

function StringToHex(AText: String; ALineBytes: Integer = 8; ABlockBytes: Integer = 4): String;
  var
    i, m, len: Integer;
  begin
    Result := '';
    len    := AText.Length;
    for i := 1 to len do
        try
        Result += Ord(AText[i]).ToHexString(2);
        finally
        m := i mod ALineBytes;

        if m = 0 then
          Result += LineEnding
        else
          if i < len then
            begin
            Result += ' ';
            if m mod ABlockBytes = 0 then
              Result += ' ';
            end;
        end;
  end;

function StrToCodes(AString: String; ASysOut: Integer; ALineChars: Integer): String;
  var
    i, l, cnt, tmp: Integer;
  begin
    cnt    := 0;
    l      := AString.Length;
    Result := '';

    if l > 0 then
      begin
      for i := 1 to l do
          try

          case ASysOut of
            0:
              begin
              Result += AString[i];
              cnt    += 1;
              end;
            2:
              begin
              Result += IntToBin(Ord(AString[i]), 8);
              cnt    += 9;
              end;
            10:
              begin
              tmp    := Ord(AString[i]);
              Result += tmp.ToString;
              cnt    += 1;
              while tmp > 0 do
                begin
                cnt += 1;
                tmp := tmp div 10;
                end;
              end;
            16:
              begin
              Result += Ord(AString[i]).ToHexString(2);
              cnt    += 3;
              end;
            end;

          finally

          //if (ASysOut > 0) and (ALineChars > 0) then
          if cnt >= ALineChars - 1 then
            begin
            cnt    := 0;
            Result += LineEnding;
            end
          else
            Result += ' ';

          end;

      if (Result.Length > 0) and (ASysOut > 0) then
        while Result[Result.Length] = ' ' do
          Delete(Result, Result.Length, 1);
      end;
  end;

function CodesToStr(AData: String; ASysIn, ASysOut: Integer): String;
  var
    i, l, ch_out: Integer;
    ch:           Char;
    prefix, tmp:  String;
    symbols:      set of Char;
    delimiters:   set of Char = [' ', ',', ';', '-', #13, #10];
  begin
    case ASysIn of
      2:
        begin
        prefix  := '%';
        symbols := ['0'..'1'];
        end;

      10:
        begin
        prefix  := '';
        symbols := ['0'..'9'];
        end;

      16:
        begin
        prefix  := '$';
        symbols := ['0'..'9', 'A'..'F'];
        end;
      end;

    AData  := UpperCase(AData);
    l      := AData.Length;
    Result := '';
    tmp    := '';

    if l > 0 then
      begin
      for i := 1 to l do
        begin
        ch := AData[i];

        if ch in symbols then
          tmp += ch;

        if ((ch in delimiters) or (i = l)) and (tmp <> '') then
            try
            ch_out := (prefix + tmp).ToInteger;
            case ASysOut of
              0: Result  += chr(ch_out);
              2: Result  += IntToBin(ch_out, 8) + ' ';
              10: Result += ch_out.ToString + ' ';
              16: Result += ch_out.ToHexString(2) + ' ';
              end;
            finally
            tmp := '';
            end;
        end;

      if (Result.Length > 0) and (ASysOut > 0) then
        while Result[Result.Length] = ' ' do
          Delete(Result, Result.Length, 1);
      end;
  end;

function SizeStr(AText: String): String;
  var
    size: Integer;
  begin
    size   := AText.Length;
    Result := size.ToString + ' ';

    if size > 1000000 then
      Result := Result.Insert(Result.Length - 7, ' ');

    if size > 1000 then
      Result := Result.Insert(Result.Length - 4, ' ');

    if size = 1 then
      Result += BYTE_SINGLE
    else
      if (size mod 100) in [10..20] then
        Result += BYTE_MULTIPLE_5
      else
        case size mod 10 of
          1:
            Result += BYTE_MULTIPLE_1;
          2..4:
            Result += BYTE_MULTIPLE_2
          else
            Result += BYTE_MULTIPLE_5;
          end;
  end;

function HexStringPosition(ACursor: Integer; ALineBytes: Integer = 8;
  ABlockBytes: Integer = 4): Integer;
  var
    x, s: Integer;
  begin
    x      := ALineBytes * 3 - 1 + Length(LineEnding);
    x      += (ALineBytes - 1) div ABlockBytes;
    s      := ACursor div x;
    Result := s * ALineBytes;
    s      *= -x;
    s      += ACursor - 1;
    s      -= s div (ABlockBytes * 3 + 1);
    Result += s div 3;
  end;


function CheckBoolean(ABool: Boolean; ValueTrue, ValueFalse: Variant): Variant;
  begin
    if ABool then
      Result := ValueTrue else
      Result := ValueFalse;
  end;

function CheckBooleanPtr(ABool: Boolean; ValueTrue, ValueFalse: Pointer): Pointer;
  begin
    if ABool then
      Result := ValueTrue else
      Result := ValueFalse;
  end;

function InRange(AValue, AMin, AMax: Variant): Boolean;
  begin
    Result := (AValue >= AMin) and (AValue <= AMax);
  end;

function FileExtCheck(AFilename, AExtList: String): Boolean;
  var
    i: Integer;
    s: String = '';
  begin
    AExtList += ' ';

    for i := 1 to Length(AExtList) do
      if AExtList[i] in [' ', ';', ','] then
        begin
        if CompareFileExt(AFileName, s, False) = 0 then Exit(True);
        s := '';
        end
      else
        s += AExtList[i];

    Result := False;
  end;

function GetListStringsMaxWidth(AForm: TForm; AItems: TStrings): Integer;
  var
    i, w: Integer;
  begin
    Result := 0;
    for i := 0 to AItems.Count - 1 do
      begin
      w := AForm.Canvas.GetTextWidth(AItems.Strings[i] + '  ');
      if Result < w then Result := w;
      end;

    Result += AForm.VertScrollBar.Size;
  end;

procedure ComboBoxUpdateList(AComboBox: TComboBox; AStrings: array of String);
  var
    i, tmp: Integer;
  begin
    with AComboBox do
      begin
      tmp := ItemIndex;

      Items.Clear;
      for i := Low(AStrings) to High(AStrings) do
        Items.Add(AStrings[i]);

      // восставнавливаем выделенный пункт списка
      ItemIndex := CheckBoolean(tmp < 0, 0, tmp);
      end;
  end;


end.
