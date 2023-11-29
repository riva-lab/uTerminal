@chcp 1251
echo off

set MDPATH=..\
set TEMPLATE=%MDPATH%\readme-template.html
set MARKDOWN=%MDPATH%\readme.md
set OUTHTML=%MDPATH%\readme.html

copy /b %TEMPLATE% + %MARKDOWN% %OUTHTML% /y