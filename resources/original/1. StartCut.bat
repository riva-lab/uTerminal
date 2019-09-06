@echo off
chcp 65001

set PATHX=D:\Рабочая папка\FPC\uTerminal\resources\original

"matri-X-cuter.exe" "-file=%PATHX%\icons_1.png" "-out=Source" -x-grid=16 -y-grid=11 -x-start=66 -y-start=66 -x-end=6926 -y-end=4800 -x-space=56 -y-space=56 
echo Вырезка окончена!
