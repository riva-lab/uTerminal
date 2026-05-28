echo off

setlocal
set "item=%~1"
set "item=%item:\=\\%"


echo:>> hashes.txt
echo %item%>> hashes.txt


:: Write file size in bytes
<nul set /p="`">> hashes.txt
for %%i in ("%item%") do set "myfilesize=%%~zi"
<nul set /p="%myfilesize% ">> hashes.txt
echo bytes`>> hashes.txt


:: Trick to avoid CRLF at the end of line
<nul set /p="- MD5: `">> hashes.txt

for /f "delims=" %%i in ('
    CertUtil -hashfile "%item%" MD5  ^|  findstr /i /r /c:"^[a-f0-9]*$"
') do <nul set /p="%%i">> hashes.txt

echo `>> hashes.txt


<nul set /p="- SHA: `">> hashes.txt

for /f "delims=" %%i in ('
    CertUtil -hashfile "%item%" SHA1  ^|  findstr /i /r /c:"^[a-f0-9]*$"
') do <nul set /p="%%i">> hashes.txt

echo `>> hashes.txt