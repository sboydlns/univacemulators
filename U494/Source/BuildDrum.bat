@echo off
c:
cd \development\emulators\u494
win32\debug\FH880Util -d Drums\sysvol.drum -c
win32\debug\FH880Util -d Drums\sysvol.drum -i Cave\u494_adventure.txt -f CAVE.DAT -lt
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\BcdTest.mem -f BCDTEST -lp
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\FloatTest.mem -f FLOATTEST -lp
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\HelloWorld.mem -f HELLOWORLD -lp
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\Cave.mem -f CAVE -lp
win32\debug\FH880Util -d Drums\sysvol.drum -i Bin\FileUtil.mem -f FILEUTIL -lp
win32\debug\FH880Util -d Drums\sysvol.drum -i CardFiles\1005_christmas_train.fd -f XMASTR.DAT -lt
win32\debug\FH880Util -d Drums\sysvol.drum -i CardFiles\chanukah.fd -f HANUKA.DAT -lt
win32\debug\FH880Util -d Drums\sysvol.drum -i CardFiles\merry_christmas.fd -f XMAS.DAT -lt
