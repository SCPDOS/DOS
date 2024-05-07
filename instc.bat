@echo off
echo Installing SCP/DOS on C:
format c:
sys c:
c:
mklbl
a:
md c:dos
@echo on
copy *.com c:dos
copy *.exe c:dos
echo @echo off > c:\autoexec.bat
echo path=c:\dos >> c:\autoexec.bat
echo cls >> c:\autoexec.bat
echo dir >> c:\autoexec.bat
echo buffers=99 > c:\config.sys
echo files=254 >> c:\config.sys
echo lastdrive=z >> c:\config.sys
echo shell=c:\dos\command.com c:\dos /e:2048 /p >> c:\config.sys
@echo off
echo Install complete