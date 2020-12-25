@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Source\HelloWorld.asm -exe -P Procs -O Bin 
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\HelloWorld.mem -f HELLOWORLD -lp
notepad Source\HelloWorld.lst
:end

