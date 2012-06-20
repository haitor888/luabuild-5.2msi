@echo off
cd /d %~dp0

:start
del lua52.msi
set path=%WIX%\bin;%path%
candle lua52.wxs
if errorlevel 1 goto error
light -ext WixUIExtension -cultures:en-us lua52.wixobj
if errorlevel 1 goto error
echo Installing...
start /wait msiexec /i lua52.msi /quiet
cd /d "%WIX%\..\Lua\5.2" 
c:\cygwin\bin\find
cd c:\
d:
echo Will uninstall now
pause
start /wait msiexec /x lua52.msi /quiet
goto ok

:error
echo *************
echo Error occured
echo *************
pause
goto start

:ok
pause
goto start
