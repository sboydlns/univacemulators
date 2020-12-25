@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\FloatTest.asm -exe -P Procs -O Bin
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\FloatTest.mem -f FLOATTEST -lp
notepad Source\FloatTest.lst
:end

