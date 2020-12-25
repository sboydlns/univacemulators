@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\FileUtil.asm -exe -P Procs -O Bin 
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\FileUtil.mem -f FILEUTIL -lp
notepad Source\FileUtil.lst
:end

