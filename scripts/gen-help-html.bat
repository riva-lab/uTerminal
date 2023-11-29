@chcp 1251
echo off

set MDPATH=..\help
set TEMPLATE=%MDPATH%\uTerminal-help-template.html
set MARKDOWN=%MDPATH%\uTerminal-help.md
set OUTHTML=%MDPATH%\uTerminal-help.html

copy /b %TEMPLATE% + %MARKDOWN% %OUTHTML% /y