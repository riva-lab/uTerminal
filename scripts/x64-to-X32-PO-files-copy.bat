@chcp 1251

set PROJECT=uTerminal


set EXTENSION=po
set FILE=bin\lang\%PROJECT%-x64-Release
set FILEOUT=bin\lang\%PROJECT%-x32-Release

cd ..

copy /y %FILE%.%EXTENSION% %FILEOUT%.%EXTENSION%
copy /y %FILE%.??.%EXTENSION% %FILEOUT%.??.%EXTENSION%
