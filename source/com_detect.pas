unit com_detect;

{$mode objfpc}{$H+}

interface

uses
  Classes;

procedure DetectComPorts(AList: TStrings);

// http://forum.sources.ru/index.php?showtopic=284035
implementation

uses
  StrUtils, Windows, SysUtils, Variants;

 //Достает из строки с нуль-терминированными подстроками следующую нуль-терминированную
 //подстроку начиная с позиции aStartPos, потом устанавливает aStartPos на символ
 //следующий за терминирующим #0.
function GetNextSubstring(aBuf: String; var aStartPos: Integer): String;
  var
    vLastPos: Integer;
  begin
    if (aStartPos < 1) then
      begin
      raise ERangeError.Create('aStartPos должен быть больше 0');
      end;

    if (aStartPos > Length(aBuf)) then
      begin
      Result := '';
      Exit;
      end;

    vLastPos  := PosEx(#0, aBuf, aStartPos);
    Result    := Copy(aBuf, aStartPos, vLastPos - aStartPos);
    aStartPos := aStartPos + (vLastPos - aStartPos) + 1;
  end;

//Заполняет список aList наденными в системе COM портами
procedure GetComPorts(aList: TStrings; aNameStart: String);
  var
    vBuf:          String;
    vRes:          Integer;
    vErr:          Integer;
    vBufSize:      Integer;
    vNameStartPos: Integer;
    vName:         String;
  begin
    vBufSize := 1024 * 5;
    vRes     := 0;

    while vRes = 0 do
      begin
      setlength(vBuf, vBufSize);
      SetLastError(ERROR_SUCCESS);
      vRes := QueryDosDevice(nil, @vBuf[1], vBufSize);
      vErr := GetLastError();

      //Вариант для двухтонки
      if (vRes <> 0) and (vErr = ERROR_INSUFFICIENT_BUFFER) then
        begin
        vBufSize := vRes;
        vRes     := 0;
        end;

      if (vRes = 0) and (vErr = ERROR_INSUFFICIENT_BUFFER) then
        begin
        vBufSize := vBufSize + 1024;
        end;

      if (vErr <> ERROR_SUCCESS) and (vErr <> ERROR_INSUFFICIENT_BUFFER) then
        begin
        //raise Exception.Create(SysErrorMessage(vErr));
        end;
      end;
    setlength(vBuf, vRes);

    vNameStartPos := 1;
    vName         := GetNextSubstring(vBuf, vNameStartPos);

    aList.BeginUpdate();
      try
      aList.Clear();
      while vName <> '' do
        begin
        if AnsiStartsStr(aNameStart, vName) then
          aList.Add(vName);
        vName := GetNextSubstring(vBuf, vNameStartPos);
        end;
      finally
      aList.EndUpdate();
      end;
  end;


procedure DetectComPorts(AList: TStrings);
  begin
    GetComPorts(AList, 'COM');
  end;

end.
