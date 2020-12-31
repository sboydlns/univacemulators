@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\MOS.asm -P Procs -O Bin
if errorlevel 1 goto end
start /b notepad++ Source\MOS.lst
win32\debug\u494spurt -tab # -sep * Source\MONITOR6.spurt -O Bin
if errorlevel 1 goto end
start /b notepad++ Source\MONITOR6.lst
:end

