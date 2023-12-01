@chcp 1251
echo off



echo.
echo Задаем пути исполняемых файлов утилит
echo Не используем сжатие бинарников - слетает темная тема: 
echo.

rem set UPX_EXECUTABLE=upx
set SEVENZIP_EXECUTABLE=7z
set PO_UTILITY=scripts\tools\poFileUtility.EXE



echo.
echo Настройки проекта
echo.

set PROJNAME=uTerminal
set BUILD=Release
set LANGDIR=bin\lang
set COMMONFILES=%LANGDIR%\*.ini %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.??.po readme.* license.* versions.* help\* bin\openssl-license.txt


echo.
echo Создаем файл html справки
echo.
call "gen-help-html.bat"

echo.
echo Создаем файл html readme
echo.
call "gen-readme-html.bat"

echo.
echo Копируем SVG файлы из light в dark
echo.
call "help-copy-light-svg.bat"

echo.
echo Создаем выходной каталог для текущей версии:

FOR /F "delims=" %%i IN ('get-version.bat "%cd%\..\bin\uTerminal-win32-Release.exe"') DO set EXEVER=%%i
set DEST=install\v%EXEVER%
echo  - каталог %DEST%



cd ..

echo.
echo Удаление файлов перевода win32
echo.
del /f /q %LANGDIR%\*win32-*.po?

echo.
echo Копирование win64 файлов перевода в общие для всех бинарников
echo.
copy %LANGDIR%\%PROJNAME%-win64-%BUILD%.*.po  %LANGDIR%\%PROJNAME%.*.po

echo.
echo Копирование win64 шаблона локализации перевода в общий шаблон
echo.
copy %LANGDIR%\%PROJNAME%-win64-%BUILD%.pot   %LANGDIR%\%PROJNAME%.pot

echo.
echo Перенос строк в файле перевода для языка оригинала и сохранение в .ru.po
echo.
%PO_UTILITY% %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.ru.po transfer


setlocal enabledelayedexpansion
for %%a in (32,64) do (
    if %%a==32 set SPECIFIC=bin\libcrypto-1_1.dll     bin\libssl-1_1.dll
    if %%a==64 set SPECIFIC=bin\libcrypto-1_1-x64.dll bin\libssl-1_1-x64.dll

    set PROJARC=win%%a
    set BINARY=bin\*!PROJARC!-%BUILD%.exe
    set FILES=!BINARY! %COMMONFILES% !SPECIFIC!
    set FILENAME=%DEST%\%PROJNAME%-!PROJARC!-Portable.zip

    echo.
    echo Создаем архив:
    echo  - ZIP: {!FILENAME!}
    echo  - Files: {!FILES!}

    del /f /q "!FILENAME!"
    
    "%UPX_EXECUTABLE%" --lzma !BINARY!
    "%SEVENZIP_EXECUTABLE%" a -tzip -mx5 !FILENAME! !FILES!
)

