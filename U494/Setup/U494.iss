; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Univac 494 Emulator"
#define MyAppVerName "Univac 494 Emulator V1.0"
#define MyAppPublisher "LNS Software Systems"
#define MyAppExeName "U494.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{BC220213-18C2-46F8-8EEF-36393AE07F17}
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
DefaultDirName={pf}\{#MyAppName}
DefaultGroupName={#MyAppName}
OutputBaseFilename=U494
SetupIconFile=C:\Development\Emulators\U9200\Images\SperryStar.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: C:\Development\Emulators\U494\Win32\Debug\U494.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U494\Win32\Debug\FH880Util.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U494\Win32\Debug\U494Asm.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U494\Win32\Debug\U494Console.exe; DestDir: {app}; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: C:\Development\Emulators\U494\Manuals\494\UP-4049r2_494pgmRef_1969.pdf; DestDir: {app}\Manuals
Source: C:\Development\Emulators\U494\Manuals\494\UP-4133r1_Univac494_Assembler_Ref.pdf; DestDir: {app}\Manuals
Source: C:\Development\Emulators\U494\Manuals\494\PrelimOMEGADesignMan_Mar66.pdf; DestDir: {app}\Manuals
Source: C:\Development\Emulators\U494\Manuals\494\UP-4032r2_Univac494sys_1969.pdf; DestDir: {app}\Manuals
Source: C:\Development\Emulators\U494\Manuals\490\UT-2474r1_FH-880_drum_Mar63.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\494\UNIVAC 494 Real-Time System Reference Card-I (UP-7530, Rev. 1).PDF; DestDir: {app}\Manuals
Source: ..\Manuals\494\UNIVAC 494 Real-Time System Reference Card-II (Software) [UP-7577].PDF; DestDir: {app}\Manuals
Source: C:\Development\Emulators\U494\Source\MOS.asm; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BcdTest.asm; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BuildBcdTest.bat; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BuildCave.bat; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BuildFloatTest.bat; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BuildHelloWorld.bat; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\BuildMOS.bat; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\Cave.asm; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\FloatTest.asm; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Source\HelloWorld.asm; DestDir: {app}\Source
Source: ..\Source\BuildFileUtil.bat; DestDir: {app}\Source
Source: ..\Source\FileUtil.asm; DestDir: {app}\Source
Source: C:\Development\Emulators\U494\Docs\U494.pdf; DestDir: {app}\Docs
Source: ..\Drums\sysvol.drum; DestDir: {commonappdata}\Univac 494 Emulator\Data; Permissions: users-full; Tasks: ; Languages: 
Source: ..\Bin\FloatTest.mem; DestDir: {app}\Bin
Source: ..\Bin\HelloWorld.mem; DestDir: {app}\Bin
Source: ..\Bin\MOS.mem; DestDir: {app}\Bin
Source: ..\Bin\MOS.mem; DestDir: {commonappdata}\Univac 494 Emulator\Data
Source: ..\Bin\BcdTest.mem; DestDir: {app}\Bin
Source: ..\Bin\Cave.mem; DestDir: {app}\Bin
Source: ..\Bin\FileUtil.mem; DestDir: {app}\Bin
Source: ..\CardFiles\slash_asterix.fd; DestDir: {app}\CardFiles
Source: ..\CardFiles\1005_christmas_train.fd; DestDir: {app}\CardFiles
Source: ..\CardFiles\chanukah.fd; DestDir: {app}\CardFiles
Source: ..\CardFiles\merry_christmas.fd; DestDir: {app}\CardFiles
Source: ..\Procs\misc.proc; DestDir: {app}\Procs
Source: ..\Procs\Print.proc; DestDir: {app}\Procs
Source: ..\Procs\Tasks.proc; DestDir: {app}\Procs
Source: ..\Procs\card.proc; DestDir: {app}\Procs
Source: ..\Procs\ExecReturn.proc; DestDir: {app}\Procs
Source: ..\Procs\FH880.proc; DestDir: {app}\Procs
Source: ..\Procs\files.proc; DestDir: {app}\Procs
Source: ..\Procs\IFR.proc; DestDir: {app}\Procs
Source: ..\Procs\Interrupts.proc; DestDir: {app}\Procs
Source: ..\Procs\IoReq.proc; DestDir: {app}\Procs
Source: ..\Procs\Lists.proc; DestDir: {app}\Procs
Source: ..\Procs\Mem.proc; DestDir: {app}\Procs
Source: ..\Procs\Mfd.proc; DestDir: {app}\Procs
Source: ..\Obj\BcdTest.obj; DestDir: {app}\Obj
Source: ..\U490.cfg; DestDir: {app}
Source: ..\U494.cfg; DestDir: {app}
Source: ..\U1230.cfg; DestDir: {app}
Source: ..\Win32\Debug\U494Spurt.exe; DestDir: {app}
Source: ..\Source\Monitor6.spurt; DestDir: {app}\Source
Source: ..\Manuals\490\UP-3900_SPURT_refMan_1963.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\1230\PX3892_Programming_Manual_for_1230_Computer_Feb66.pdf; DestDir: {app}\Manuals
Source: ..\Bin\monitor6.mem; DestDir: {app}\Bin
Source: ..\Bin\monitor6.mem; DestDir: {commonappdata}\Univac 494 Emulator\Data
Source: ..\Docs\Monitor6.pdf; DestDir: {app}\Docs
Source: ..\Source\SpurtTest.spurt; DestDir: {app}\Source
Source: ..\Bin\SpurtTest.pt; DestDir: {app}\Bin
Source: ..\Source\iolib.spurt; DestDir: {app}\Source
Source: ..\Source\RTExec.spurt; DestDir: {app}\Source
Source: ..\Bin\iolib.mem; DestDir: {app}\Bin
Source: ..\Bin\RTExec.mem; DestDir: {app}\Bin
Source: ..\JoeCousins\a060228.pdf; DestDir: {app}\Manuals; DestName: RTExec.pdf
Source: ..\Bin\642b_upac.mem; DestDir: {app}\Bin
Source: ..\Manuals\642\CP-642B Utility Package Function Repertoire ((NAVSHIPS 0967-011-2030).PDF; DestDir: {app}\Manuals
Source: ..\Bin\642b_upac.mem; DestDir: {commonappdata}\Univac 494 Emulator\Data
Source: ..\Docs\Monitor6.pdf; DestDir: {app}\Manuals

[Icons]
Name: {group}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; WorkingDir: {app}
Name: {commondesktop}\{#MyAppName}; Filename: {app}\{#MyAppExeName}; Tasks: desktopicon; WorkingDir: {app}
Name: {group}\FH880 Utilities; Filename: {app}\FH880Util.exe; WorkingDir: {app}; IconIndex: 0
Name: {group}\U494 Console; Filename: {app}\U494Console.exe; WorkingDir: {app}; IconIndex: 0
Name: {group}\U494 Emulator Docs; Filename: {app}\Docs\U494.pdf
Name: {group}\Univac 1230 Emulator; Filename: {app}\U494.exe; Parameters: -c U1230.cfg; WorkingDir: {app}
Name: {group}\Univac 490 Emulator; Filename: {app}\U494.exe; Parameters: -c U490.cfg; WorkingDir: {app}; IconIndex: 0

[Run]
Filename: {app}\{#MyAppExeName}; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: nowait postinstall skipifsilent

[Dirs]
Name: {app}\Manuals
Name: {app}\Source
Name: {app}\Docs
Name: {app}\Bin
Name: {commonappdata}\Univac 494 Emulator\Data
Name: {app}\CardFiles
Name: {app}\Procs
Name: {app}\Obj
