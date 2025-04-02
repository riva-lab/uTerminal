set TEMPDIR=tempPs9dA2D45gpn

md   .\%TEMPDIR%
copy /b %MDPATH%\%MARKDOWN%  %TEMPDIR%\%MARKDOWN% /y
cd   %TEMPDIR%
del  *.html

for %%i in (%MARKDOWN%.md) do (
  copy /b ..\%TEMPLATE% + %%i %%i.html /y
  del  %%i
)

ren  *.html *.
ren  *.md *.html
copy /b * ..\%MDPATH% /y
cd   ..
rd   %TEMPDIR% /s /q
