echo off

setlocal
set "item=%~1"
set "item=%item:\=\\%"


:: Trick to avoid CRLF at the end of line
<nul set /p="| ">> hashes.txt
<nul set /p="%item% | ">> hashes.txt

:: Write file size in bytes
for %%i in ("%item%") do set "myfilesize=%%~zi"
<nul set /p="%myfilesize% | ">> hashes.txt

:: Get SHA256 checksum for file and output it to files
for /f "delims=" %%i in ('
    CertUtil -hashfile "%item%" SHA256  ^|  findstr /i /r /c:"^[a-f0-9]*$"
') do @(
    <nul set /p="%%i ">> sha256sums.txt
    echo %%i ^|>> hashes.txt
)

:: Add filename to sha256sums.txt
echo %item%>> sha256sums.txt