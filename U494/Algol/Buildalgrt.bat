@echo off
c:
cd \development\emulators\u494
win32\debug\u494asm Algol\RTTest.asm -obj -P Procs -O Obj
start /b notepad++ Algol\RTTest.lst
if errorlevel 1 goto end

win32\debug\u494asm Algol\algrt.asm -obj -P Procs -O Obj -noproc
if errorlevel 1 goto end
start /b notepad++ Algol\algrt.lst

win32\debug\u494asm Algol\algio.asm -obj -P Procs -O Obj -noproc
if errorlevel 1 goto end
start /b notepad++ Algol\algio.lst

win32\debug\u494asm Algol\algmath.asm -obj -P Procs -O Obj -noproc
if errorlevel 1 goto end
start /b notepad++ Algol\algmath.lst

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj Obj\RTTest.obj -exe -o Bin\RTTest.mem -l Algol\RTTest.map
start /b notepad++ Algol\RTTest.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\RTTest.mem -f RTTEST -lp

..\AlgolV2\win32\debug\algol ..\Algol\test.alg
if errorlevel 1 goto end
win32\debug\u494asm ..\Algol\test.s -obj -P Procs -O ..\AlgolV2\Obj -noproc
if errorlevel 1 goto end
start /b notepad++ ..\Algol\test.lst
if errorlevel 1 goto end

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj ..\AlgolV2\Obj\test.obj -exe -o ..\AlgolV2\Bin\test.mem -l ..\AlgolV2\test.map
start /b notepad++ ..\AlgolV2\test.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i ..\AlgolV2\Bin\test.mem -f TEST -lp

..\AlgolV2\win32\debug\algol ..\AlgolV2\Tests\test1.alg
if errorlevel 1 goto end
win32\debug\u494asm ..\AlgolV2\Tests\test1.s -obj -P Procs -O ..\AlgolV2\Tests\Obj -noproc
start /b notepad++ ..\AlgolV2\Tests\test1.lst
if errorlevel 1 goto end

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj ..\AlgolV2\Tests\Obj\test1.obj -exe -o ..\AlgolV2\Tests\Bin\test1.mem -l ..\AlgolV2\Tests\test1.map
start /b notepad++ ..\AlgolV2\TestS\test1.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i ..\AlgolV2\Tests\Bin\test1.mem -f TEST1 -lp

..\AlgolV2\win32\debug\algol ..\AlgolV2\Tests\test2.alg
if errorlevel 1 goto end
win32\debug\u494asm ..\AlgolV2\Tests\test2.s -obj -P Procs -O ..\AlgolV2\Tests\Obj -noproc
start /b notepad++ ..\AlgolV2\Tests\test2.lst
if errorlevel 1 goto end

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj ..\AlgolV2\Tests\Obj\test2.obj -exe -o ..\AlgolV2\Tests\Bin\test2.mem -l ..\AlgolV2\Tests\test2.map
start /b notepad++ ..\AlgolV2\TestS\test2.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i ..\AlgolV2\Tests\Bin\test2.mem -f TEST2 -lp

..\AlgolV2\win32\debug\algol ..\AlgolV2\Tests\test3.alg
if errorlevel 1 goto end
win32\debug\u494asm ..\AlgolV2\Tests\test3.s -obj -P Procs -O ..\AlgolV2\Tests\Obj -noproc
start /b notepad++ ..\AlgolV2\Tests\test3.lst
if errorlevel 1 goto end

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj ..\AlgolV2\Tests\Obj\test3.obj -exe -o ..\AlgolV2\Tests\Bin\test3.mem -l ..\AlgolV2\Tests\test3.map
start /b notepad++ ..\AlgolV2\TestS\test3.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i ..\AlgolV2\Tests\Bin\test3.mem -f TEST3 -lp

..\AlgolV2\win32\debug\algol ..\AlgolV2\Tests\test4.alg
if errorlevel 1 goto end
win32\debug\u494asm ..\AlgolV2\Tests\test4.s -obj -P Procs -O ..\AlgolV2\Tests\Obj -noproc
start /b notepad++ ..\AlgolV2\Tests\test4.lst
if errorlevel 1 goto end

win32\debug\u494link Obj\algrt.obj Obj\algio.obj Obj\algmath.obj ..\AlgolV2\Tests\Obj\test4.obj -exe -o ..\AlgolV2\Tests\Bin\test4.mem -l ..\AlgolV2\Tests\test4.map
start /b notepad++ ..\AlgolV2\TestS\test4.map
if errorlevel 1 goto end
win32\debug\FH880Util -d Drums\sysvol.drum -i ..\AlgolV2\Tests\Bin\test4.mem -f TEST4 -lp

:end

