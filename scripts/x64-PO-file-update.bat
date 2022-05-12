@chcp 1251

set PROJECT=uTerminal
set LOCALE=ru

set EXTENSION=po
set UTILITY=resources\poFileUtility.EXE
set FILE=bin\lang\%PROJECT%-x64-Release
set FILEOUT=%FILE%.%LOCALE%.%EXTENSION%

cd ..

copy /y %FILE%.%EXTENSION% %FILEOUT%

%UTILITY% %FILEOUT% %FILEOUT% transfer
