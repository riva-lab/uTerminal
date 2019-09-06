@chcp 1251

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%

set PROJNAME=uTerminal
set FILENAME="%PROJNAME%-%DATE_STAMP%-Archive.zip"


%homedrive%\lazarus\UPX\upx.exe --best *.exe

del /f /q %FILENAME%
del /f /q *.dbg
del /f /q log_*.txt
rd  /s /q "source\lib"

"%homedrive%\Program Files\7-Zip\7z.exe" a -tzip -mx5 %FILENAME% "."