@chcp 1251
echo off

set PROJNAME=uTerminal

:: Get current executable version to variable EXEVER
FOR /F "delims=" %%i IN ('get-version.bat "%cd%\..\bin\%PROJNAME%-win64-Release.exe"') DO set EXEVER=%%i

:: Go to executables location
cd ..\install\v%EXEVER%

del /q sha256sums.txt
echo Project '%PROJNAME%' MD5 and SHA hashes for v%EXEVER% executables> hashes.txt
echo:>> hashes.txt

echo Executables location:>> hashes.txt
cd   >> hashes.txt
echo:>> hashes.txt
echo:>> hashes.txt
echo:>> hashes.txt

:: Place tags for Gitlab release note
echo ^<details^>^<summary^>Files and checksums:^<^/summary^>>> hashes.txt

:: Make table header
echo:>> hashes.txt
echo ^| File   ^| Size   ^| SHA256 checksum ^|>> hashes.txt
echo ^| ------ ^| ------ ^| --------------- ^|>> hashes.txt

:: Calc hashes for all project executables
forfiles /m %PROJNAME%* /c "cmd /c ..\..\scripts\get-file-hashes.bat @file"

echo:>> hashes.txt

:: Place tags for Gitlab release note
echo ^<^/details^>>> hashes.txt

:: Open hashes file
hashes.txt