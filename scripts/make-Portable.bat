@chcp 1251
echo off



echo.
echo Текущие дата и время
echo.

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%



echo.
echo Задаем пути исполняемых файлов утилит
echo Не используем сжатие бинарников - слетает темная тема: set UPX_EXECUTABLE=upx
echo.

set SEVENZIP_EXECUTABLE=7z
set PO_UTILITY=scripts\tools\poFileUtility.EXE



echo.
echo Настройки проекта
echo.

set PROJNAME=uTerminal

set BUILD=Release
set DEST=install\%DATE_STAMP%

set LANGDIR=bin\lang
set LANGINI=%LANGDIR%\*.ini
set LANG=%LANGDIR%\%PROJNAME%.??.po
set LANGTMP=%LANGDIR%\%PROJNAME%.pot
set FILES_ADDITION=readme.* license.* versions.* help/*


echo.
echo Создаем файл html справки
echo.
cmd /c "gen-help-html.bat"

echo.
echo Создаем файл html readme
echo.
cmd /c "gen-readme-html.bat"

echo.
echo Копируем SVG файлы из light в dark
echo.
cmd /c "help-copy-light-svg.bat"



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
echo Копирование win64 файлов шаблона перевода в общий
echo.
copy %LANGDIR%\%PROJNAME%-win64-%BUILD%.pot   %LANGDIR%\%PROJNAME%.pot

echo.
echo Перенос строк в файле перевода для языка оригинала в .ru.po
echo.
%PO_UTILITY% %LANGDIR%\%PROJNAME%.pot %LANGDIR%\%PROJNAME%.ru.po transfer


echo.
echo Создаем архив для win64
echo.

set PROJARC=win64

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LIBS=bin\libcrypto-1_1-x64.dll bin\libssl-1_1-x64.dll bin\openssl-license.txt

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" %FILES_ADDITION% %LIBS%

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --lzma         "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5   %FILENAME%  %FILES%



echo.
echo Создаем архив для win32
echo.

set PROJARC=win32

set BINARY=bin\*%PROJARC%-%BUILD%.exe
set LIBS=bin\libcrypto-1_1.dll bin\libssl-1_1.dll bin\openssl-license.txt

set FILENAME="%DEST%\%PROJNAME%-%PROJARC%-Portable.zip"
set FILES="%BINARY%" "%LANG%" "%LANGTMP%" "%LANGINI%" %FILES_ADDITION% %LIBS%

del /f /q %FILENAME%

"%UPX_EXECUTABLE%"       --lzma         "%BINARY%"
"%SEVENZIP_EXECUTABLE%"  a -tzip -mx5   %FILENAME%  %FILES%
