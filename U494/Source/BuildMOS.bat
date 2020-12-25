@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\MOS.asm -P Procs -O Bin
if errorlevel 1 goto end
notepad++ Source\MOS.lst
:end

