@chcp 1251
echo off

set MDPATH=..\help
set LIGHT=%MDPATH%\light\screenshots\*.svg
set DARK=%MDPATH%\dark\screenshots\*.svg

copy %LIGHT% %DARK% /y