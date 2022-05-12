@chcp 1251

set DD=%DATE:~0,2%
set MM=%DATE:~3,2%
set YY=%DATE:~8,2%
set YYYY=%DATE:~6,4%
set HH=%TIME:~0,2%
set MN=%TIME:~3,2%

set DATE_STAMP=%YYYY%-%MM%-%DD%_%HH%-%MN%

set PROJNAME=uTerminal
set FILENAME="snapshots\%PROJNAME%-%DATE_STAMP%-snapshot.zip"
set INCLUDE="."
set EXCLUDE1="snapshots"
set EXCLUDE2="install"
set EXCLUDE3=".git"

cd ..

%homedrive%\lazarus\UPX\upx.exe --best bin\*.exe

del /f /q %FILENAME%
del /f /q *.dbg
del /f /q log_*.txt
rd  /s /q "source\lib"

"%homedrive%\Program Files\7-Zip\7z.exe" a -tzip -mx5 %FILENAME% %INCLUDE% -x!%EXCLUDE1% -x!%EXCLUDE2% -x!%EXCLUDE3%