; Script generated by the Inno Setup Script Wizard.
; SEE THE DOCUMENTATION FOR DETAILS ON CREATING INNO SETUP SCRIPT FILES!

#define MyAppName "Univac 90/30 Emulator"
#define MyAppVerName "Univac 90/30 Emulator Univac 90/30 Emulator1.0"
#define MyAppPublisher "LNS Software Systems"
#define MyAppExeName "U9030.exe"

[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{712D2864-DCDA-43A9-9839-BB28F4F489BC}
AppName={#MyAppName}
AppVerName={#MyAppVerName}
AppPublisher={#MyAppPublisher}
DefaultDirName={pf}\Univac 9030 Emulator
DefaultGroupName=Univac 9030 Emulator
OutputBaseFilename=U9030
SetupIconFile=C:\Development\Emulators\U9200\Images\SperryStar.ico
Compression=lzma
SolidCompression=yes

[Languages]
Name: english; MessagesFile: compiler:Default.isl

[Tasks]
Name: desktopicon; Description: {cm:CreateDesktopIcon}; GroupDescription: {cm:AdditionalIcons}; Flags: unchecked

[Files]
Source: C:\Development\Emulators\U9030\Win32\Debug\U9030.exe; DestDir: {app}; Flags: ignoreversion
; NOTE: Don't use "Flags: ignoreversion" on any shared system files
Source: C:\Development\Emulators\U9030\Win32\Debug\U9030Console.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U9030\Win32\Debug\U9030DisAsm.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U9030\Win32\Debug\U8418Util.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U9030\Win32\Debug\DmpRst.exe; DestDir: {app}; Flags: ignoreversion
Source: C:\Development\Emulators\U9030\Win32\Debug\U9030.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Win32\Debug\U200TN.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Win32\Debug\U9030Print.exe; DestDir: {app}; Flags: ignoreversion
Source: ..\Disks\SDIVSB.8418; DestDir: {commonappdata}\Univac 9030 Emulator\Data; Permissions: users-full
Source: ..\Disks\VSBRES.8418; DestDir: {commonappdata}\Univac 9030 Emulator\Data; Permissions: users-full
Source: ..\Disks\REL042.8418; DestDir: {commonappdata}\Univac 9030 Emulator\Data; Permissions: users-full
Source: C:\Development\Emulators\U9030\REL042.cfg.release; DestDir: {app}; DestName: REL042.cfg
Source: C:\Development\Emulators\U9030\VSB.cfg.release; DestDir: {app}; DestName: VSB.cfg
Source: ..\Manuals\Univac_90_30_System_Brochure_Mar74.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8041r1-A 9030 System - Integrated Peripheral Channel Programmer Reference.pdf.orig; DestDir: {app}\Manuals; DestName: UP-8041r1-A 9030 System - Integrated Peripheral Channel Programmer Reference.pdf
Source: ..\Manuals\UP-8052r1 90-30 System - Processor Programmer Reference.pdf.orig; DestDir: {app}\Manuals; DestName: UP-8052r1 90-30 System - Processor Programmer Reference.pdf
Source: ..\Manuals\UP-8060_90_30_System_Description_1974.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8061r3 Assembler - User Guide.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8068_Rev4d_Systems_90-25_90-30_90-40_Operating_System_3_(OS3)_Basic_Data_Management_User_Guide_Jun83.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8072r7-B Series 90 Operations Handbook - Operator Reference.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8075_Rev3a_Systems_90-25_90-30_90-40_Operating_System_3_(OS3)_Supervisor_User_Guide_Sep82.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8227r2 Series 90 Assembler - Programmer Reference.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8831-Rev3_OS3_Supervisor_Technical_Overview_Aug87.pdf; DestDir: {app}\Manuals
Source: ..\Manuals\UP-8832_OS3_Supervisor_Macroinstructions_ProgRefManual_Aug87.pdf; DestDir: {app}\Manuals
Source: ..\VSB Data\VSB007.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB008.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB012.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB014.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB015.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB018.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB022.asc; DestDir: {app}\VSB Data
Source: ..\VSB Data\VSB004.asc; DestDir: {app}\VSB Data
Source: ..\Tests\Decimal.tst; DestDir: {app}\Tests
Source: ..\Tests\FixedPt.tst; DestDir: {app}\Tests
Source: ..\Tests\LoadSave.tst; DestDir: {app}\Tests
Source: ..\Tests\Logical.tst; DestDir: {app}\Tests
Source: ..\Tests\Character.tst; DestDir: {app}\Tests
Source: ..\Tests\Compare.tst; DestDir: {app}\Tests
Source: ..\Docs\U9030.pdf; DestDir: {app}\Docs
Source: ..\Source\icam.asc; DestDir: {app}\Source
Source: ..\Source\IMS.jcl; DestDir: {app}\Source
Source: ..\Source\IMSVSB.jcl; DestDir: {app}\Source
Source: ..\Source\netims6.jcl; DestDir: {app}\Source
Source: ..\Source\supgen.asc; DestDir: {app}\Source
Source: ..\Source\PREP18.jcl; DestDir: {app}\Source

[Icons]
Name: {group}\Univac 9030 Emulator; Filename: {app}\U9030.exe; Parameters: -c REL042.cfg; IconIndex: 0
Name: {commondesktop}\Univac 9030 Emulator; Filename: {app}\U9030.exe; Tasks: desktopicon; Parameters: -c REL042.cfg; IconIndex: 0
Name: {group}\U8418 Utilities; Filename: {app}\U8418Util.exe; IconIndex: 0
Name: {group}\U9030 Disassembler; Filename: {app}\U9030DisAsm.exe; IconIndex: 0
Name: {group}\U9030 Console; Filename: {app}\U9030Console.exe; IconIndex: 0
Name: {group}\Dump Restore; Filename: {app}\DmpRst.exe; IconIndex: 0
Name: {group}\Univac 9030 Emulator (VSB); Filename: {app}\U9030.exe; Parameters: -c VSB.cfg; IconIndex: 0
Name: {group}\Emulator Documentatiopn; Filename: {app}\Docs\U9030.pdf
Name: {group}\U9030 Print; Filename: {app}\U9030Print.exe; IconIndex: 0
Name: {group}\U200 Emulator; Filename: {app}\U200TN.exe; IconIndex: 0

[Run]
Filename: {app}\{#MyAppExeName}; Description: {cm:LaunchProgram,{#MyAppName}}; Flags: nowait postinstall skipifsilent; Parameters: -c REL042.cfg

[Dirs]
Name: {commonappdata}\Univac 9030 Emulator\Data
Name: {app}\Manuals
Name: {app}\VSB Data
Name: {app}\Tests
Name: {app}\Docs
Name: {app}\Source
