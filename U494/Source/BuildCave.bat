@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\Cave.asm -exe -noproc -P Procs -O Bin 
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\Cave.mem -f CAVE -lp
notepad Source\Cave.lst
:end

