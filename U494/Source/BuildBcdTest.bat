@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\BcdTest.asm -obj -P Procs -O Obj
if errorlevel 1 goto end
win32\debug\u494link Obj\BcdTest.obj -exe -o Bin\BcdTest.mem
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\BcdTest.mem -f BCDTEST -lp
notepad Source\BcdTest.lst
:end

