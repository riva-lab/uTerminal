unit u_encodings;

{$mode objfpc}{$H+}

interface

uses
  Classes, LConvEncoding;

resourcestring
  ENC_ANSI    = 'ANSI (ASCII 7-битная)';
  ENC_1250    = '1250 (ANSI - центрально-европейская)';
  ENC_1251    = '1251 (ANSI - кириллица)';
  ENC_1252    = '1252 (ANSI - западно-европейская, лат. 1)';
  ENC_1253    = '1253 (ANSI - греческая)';
  ENC_1254    = '1254 (ANSI - турецкая)';
  ENC_1255    = '1255 (ANSI - иврит)';
  ENC_1256    = '1256 (ANSI - арабская)';
  ENC_1257    = '1257 (ANSI - балтийская)';
  ENC_1258    = '1258 (ANSI - вьетнамская)';
  ENC_437     = '437 (DOS - латиница)';
  ENC_850     = '850 (DOS - западно-европейская, лат.)';
  ENC_852     = '852 (DOS - центрально-европейская, лат.)';
  ENC_866     = '866 (DOS - кириллица)';
  ENC_874     = '874 (DOS - тайская)';
  ENC_88591   = '8859-1 (ISO/IEC - западно-европейская, лат. 1)';
  ENC_88592   = '8859-2 (ISO/IEC - центрально-европейская, лат. 2)';
  ENC_885915  = '8859-15 (ISO/IEC - западно-европейская новая, лат. 9)';
  ENC_KOI8R   = 'KOI8-R (русская)';
  ENC_KOI8U   = 'KOI8-U (русско-украинская)';
  ENC_KOI8RU  = 'KOI8-RU (русско-белорусско-украинская)';
  ENC_MACR    = 'Macintosh (MAC - латиница)';
  ENC_UTF8    = '65001 (UTF8 без BOM)';
  ENC_UTF8BOM = '65001 (UTF8)';


type
  TEncCustom = record
    Enc:     String[40];
    Caption: String[80];
  end;

var
  ENCODINGS_LIST: array of TEncCustom;
  encCount:       Integer;


procedure EncodingItemAdd(AEncoding, ACaption: String);
procedure EncodingsListUpdate;

// получение списка доступных кодировок
procedure EncodingsListAssign(AStringList: TStrings);

function GetEncodingByIndex(AIndex: Integer): String;
function GetIndexOfEncoding(AEncoding: String): Integer;
function GetEncodingCaption(AEncoding: String): String;

function UTF8ToEncoding(AStr: String; AEncoding: String): String;
function UTF8ToEncodingByIndex(AStr: String; AIndex: Integer): String;

function EncodingToUTF8(AStr: String; AEncoding: String): String;
function EncodingToUTF8ByIndex(AStr: String; AIndex: Integer): String;

function UTF8ToAnsiCustom(s: String): String;

implementation

procedure EncodingItemAdd(AEncoding, ACaption: String);
  begin
    encCount += 1;
    if encCount > Length(ENCODINGS_LIST) then
      SetLength(ENCODINGS_LIST, encCount);

    ENCODINGS_LIST[High(ENCODINGS_LIST)].Enc     := AEncoding;
    ENCODINGS_LIST[High(ENCODINGS_LIST)].Caption := ACaption;
  end;

procedure EncodingsListUpdate;
  begin
    encCount := 0;
    SetLength(ENCODINGS_LIST, encCount);

    EncodingItemAdd(EncodingAnsi, ENC_ANSI);

    EncodingItemAdd(EncodingCP1250, ENC_1250);
    EncodingItemAdd(EncodingCP1251, ENC_1251);
    EncodingItemAdd(EncodingCP1252, ENC_1252);
    EncodingItemAdd(EncodingCP1253, ENC_1253);
    EncodingItemAdd(EncodingCP1254, ENC_1254);
    EncodingItemAdd(EncodingCP1255, ENC_1255);
    EncodingItemAdd(EncodingCP1256, ENC_1256);
    EncodingItemAdd(EncodingCP1257, ENC_1257);
    EncodingItemAdd(EncodingCP1258, ENC_1258);

    EncodingItemAdd(EncodingCP437, ENC_437);
    EncodingItemAdd(EncodingCP850, ENC_850);
    EncodingItemAdd(EncodingCP852, ENC_852);
    EncodingItemAdd(EncodingCP866, ENC_866);
    EncodingItemAdd(EncodingCP874, ENC_874);

    EncodingItemAdd(EncodingCPIso1, ENC_88591);
    EncodingItemAdd(EncodingCPIso2, ENC_88592);
    EncodingItemAdd(EncodingCPIso15, ENC_885915);

    EncodingItemAdd(EncodingCPKOI8R, ENC_KOI8R);
    EncodingItemAdd(EncodingCPKOI8U, ENC_KOI8U);
    EncodingItemAdd(EncodingCPKOI8RU, ENC_KOI8RU);

    EncodingItemAdd(EncodingCPMac, ENC_MACR);

    EncodingItemAdd(EncodingUTF8, ENC_UTF8);
    EncodingItemAdd(EncodingUTF8BOM, ENC_UTF8BOM);
  end;

procedure EncodingsListAssign(AStringList: TStrings);
  var
    i: Integer;
  begin
    if AStringList = nil then Exit;
    if Length(ENCODINGS_LIST) = 0 then
      EncodingsListUpdate;

    AStringList.Clear;
    for i := Low(ENCODINGS_LIST) to High(ENCODINGS_LIST) do
      AStringList.Add(ENCODINGS_LIST[i].Caption);
  end;


function GetEncodingByIndex(AIndex: Integer): String;
  begin
    if AIndex < Low(ENCODINGS_LIST) then Exit('');
    if AIndex > High(ENCODINGS_LIST) then Exit('');
    Result := ENCODINGS_LIST[AIndex].Enc;
  end;

function GetIndexOfEncoding(AEncoding: String): Integer;
  var
    i: Integer;
  begin
    Result := -1;
    for i := Low(ENCODINGS_LIST) to High(ENCODINGS_LIST) do
      if AEncoding = ENCODINGS_LIST[i].Enc then Exit(i);
  end;

function GetEncodingCaption(AEncoding: String): String;
  var
    i: Integer;
  begin
    Result := '';
    i      := GetIndexOfEncoding(AEncoding);
    if i >= 0 then
      Result := ENCODINGS_LIST[i].Caption;
  end;


function UTF8ToEncoding(AStr: String; AEncoding: String): String;
  var
    b: Boolean;
  begin
    if AEncoding = EncodingUTF8 then Exit(AStr);
    Result := ConvertEncodingFromUTF8(AStr, AEncoding, b);
    if not b then Result := UTF8ToAnsiCustom(AStr);
  end;

function UTF8ToEncodingByIndex(AStr: String; AIndex: Integer): String;
  begin
    Result := UTF8ToEncoding(AStr, GetEncodingByIndex(AIndex));
  end;


function EncodingToUTF8(AStr: String; AEncoding: String): String;
  var
    b: Boolean;
  begin
    if AEncoding = EncodingUTF8 then Exit(AStr);
    Result := ConvertEncodingToUTF8(AStr, AEncoding, b);
  end;

function EncodingToUTF8ByIndex(AStr: String; AIndex: Integer): String;
  begin
    Result := EncodingToUTF8(AStr, GetEncodingByIndex(AIndex));
  end;

function UTF8ToAnsiCustom(s: String): String;
  var
    i, l: Integer;
  begin
    s      := UTF8ToCP1251(s);
    l      := Length(s);
    Result := '';

    for i := 1 to l do
      Result += chr(Byte($7F and Ord(s[i])));
  end;


end.

