unit U9030Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom,
  Xml.XMLDoc;

const
  START_MSG = WM_USER + 1;

type
  TU9030Form = class(TForm)
    CpuTestBtn: TButton;
    Panel: TPanel;
    Label1: TLabel;
    PSWEdt: TEdit;
    Timer: TTimer;
    Label2: TLabel;
    InstEdt: TEdit;
    Label3: TLabel;
    BootBtn: TButton;
    BootDvcEdt: TEdit;
    StopBtn: TButton;
    RunBtn: TButton;
    StepBtn: TButton;
    StateLbl: TLabel;
    SR0: TEdit;
    Label4: TLabel;
    Label5: TLabel;
    SR1: TEdit;
    SR2: TEdit;
    SR3: TEdit;
    SR4: TEdit;
    SR5: TEdit;
    SR6: TEdit;
    SR7: TEdit;
    SR8: TEdit;
    SR9: TEdit;
    SR10: TEdit;
    SR11: TEdit;
    SR12: TEdit;
    SR13: TEdit;
    SR14: TEdit;
    SR15: TEdit;
    PR0: TEdit;
    PR1: TEdit;
    PR2: TEdit;
    Pr3: TEdit;
    PR4: TEdit;
    PR5: TEdit;
    PR6: TEdit;
    PR7: TEdit;
    PR8: TEdit;
    PR9: TEdit;
    PR10: TEdit;
    PR11: TEdit;
    PR12: TEdit;
    PR13: TEdit;
    PR14: TEdit;
    PR15: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    RelRegEdt: TEdit;
    DebugBtn: TButton;
    DisableTimerBox: TCheckBox;
    Bevel1: TBevel;
    Label39: TLabel;
    PrnSaveBtn: TButton;
    OpenDlg: TOpenDialog;
    Bevel2: TBevel;
    Label40: TLabel;
    RdrLoadBtn: TButton;
    ConfigXml: TXMLDocument;
    PrintBtn: TButton;
    ReaderStatusLbl: TLabel;
    RdrEmptyBtn: TButton;
    RdrAttnBtn: TButton;
    ResetBtn: TButton;
    PrinterStatusLbl: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure StepBtnClick(Sender: TObject);
    procedure RunBtnClick(Sender: TObject);
    procedure BootBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure DebugBtnClick(Sender: TObject);
    procedure DisableTimerBoxClick(Sender: TObject);
    procedure CpuTestBtnClick(Sender: TObject);
    procedure PrnSaveBtnClick(Sender: TObject);
    procedure RdrLoadBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure RdrEmptyBtnClick(Sender: TObject);
    procedure RdrAttnBtnClick(Sender: TObject);
    procedure ResetBtnClick(Sender: TObject);
  private
    FConsoleStarted: Boolean;
    FConfigFile: String;
    function Hex(s: String): Integer;
    procedure Initialize;
    procedure ParseCmdLine;
    procedure StartMsg(var Message: TMessage); message START_MSG;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  U9030Form: TU9030Form;

implementation

{$R *.dfm}

uses Winapi.MMSystem, DebuggerFrm, U9030Types, Globals, Channels, Cpu, Memory, IDA, IPC, Console, Trace,
     U0717, U0773, CpuTestFrm, Config, UniscopeAdapter;

var
    Cons: TConsole;
    Printer: T0773;
    Reader: T0717;
{ TU9030Form }

procedure TU9030Form.BootBtnClick(Sender: TObject);
// Boot from an IDA disk
const
// BCW to read first 2 sectors to address zero
    bcw: array [0..15] of Byte = (
      2, 0, 0, 0,
      0, 0, 0, 2,
      0, 0, 0, 0,
      0, 0, 1, 0
    );
var
    dvc: Integer;
begin
    dvc := Hex(BootDvcEdt.Text);
    if ((dvc < $300) or (dvc > $307)) then
        raise Exception.Create('Invalid boot device address');
    Processor.Reset;
    Core.Copy(@bcw, $f0, 16);                       // copy BCW to low mem

    Adapters.Channel[3].SIO(dvc);                   // Issue read to dev 300
    while (not Adapters.Channel[3].IntPending) do   // wait for I/O to complete
        Sleep(100);
    if (Assigned(Opcodes.FindOpcode('LD').Proc)) then
        Core.StoreByte(0, MACHINE_ID, 2)
    else
        Core.StoreByte(0, MACHINE_ID, 0);
    Core.StoreByte(0, LOAD_ID, 0);
    Core.StoreByte(0, REVISION_LEVEL, 0);
    Core.StoreByte(0, REVISION_LEVEL + 1, 0);
    PSW.IOSTIntEnabled := True;                     // enabled IOST interrupts
    Processor.Run;                                  // GO 4 IT!!!!!
end;

procedure TU9030Form.CpuTestBtnClick(Sender: TObject);
begin
    CpuTestForm.ShowModal;
end;

constructor TU9030Form.Create(AOwner: TComponent);
var
    tc: TIMECAPS;
begin
    inherited;
    // This is needed to make Sleep(1) closer to 1MS than it would
    // be using the default clock tick interval which is closer to 15MS.
    timeGetDevCaps(@tc, SizeOf(tc));
    timeBeginPeriod(tc.wPeriodMin);
end;

procedure TU9030Form.DebugBtnClick(Sender: TObject);
begin
    DebuggerForm.Show;
    Processor.OnDebug := DebuggerForm.DoDebug;
end;

destructor TU9030Form.Destroy;
var
    tc: TIMECAPS;
begin
    // Be sure to undo the timeBeginPeriod that was done in Create.
    timeGetDevCaps(@tc, SizeOf(tc));
    timeEndPeriod(tc.wPeriodMin);
    try
        // This might barf if the user closes the console window
        // before we get here.
        Cons.Shutdown;
    except
        ;
    end;
    FreeAndNil(Opcodes);
    FreeAndNil(Adapters);
    FreeAndNil(Processor);
    FreeAndNil(PSW);
    FreeAndNil(CurInst);
    FreeAndNil(Core);
    FreeAndNil(IOTraceFile);
    FreeAndNil(SvcTraceFile);
    inherited;
end;

procedure TU9030Form.DisableTimerBoxClick(Sender: TObject);
begin
    Processor.InhibitTimer := DisableTimerBox.Checked;
end;

procedure TU9030Form.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    if (DebuggerForm.Visible) then
        DebuggerForm.Close;
    Processor.Stop;
    CanClose := True;
end;

procedure TU9030Form.FormShow(Sender: TObject);
begin
    PostMessage(Handle, START_MSG, 0, 0);
end;

function TU9030Form.Hex(s: String): Integer;
var
    c: Char;
begin
    s := UpperCase(s);
    Result := 0;
    for c in s do
    begin
        if ((c >= '0') and (c <= '9')) then
            Result := (Result shl 4) or (Ord(c) - Ord('0'))
        else if ((c >= 'A') and (c <= 'F')) then
            Result := (Result shl 4) or (Ord(c) - Ord('A'))
        else
            raise Exception.Create('Invalid hex digit in device address');
    end;
end;

procedure TU9030Form.Initialize;
var
    fname: String;
    disk: TConfigDisk;
    dtype: TIDAType;
    idaDisk: TIDADisk;
begin
    Opcodes := TOpcodeList.Create;
    Core := TMemory.Create;
    Processor := TCpu.Create;
    PSW := TPSW.Create;
    CurInst := TInstruction.Create;
    Adapters := TChannelList.Create;
    Configuration := TConfig.Create;
    // Create all channels
    Adapters.Channel[0] := TIPC.Create(0);
    Adapters.Channel[1] := TChannel.Create(1);
    Adapters.Channel[3] := TIDA.Create(3);
    Adapters.Channel[4] := TChannel.Create(4);
    Adapters.Channel[6] := TChannel.Create(6);
    // Create required IPC devices
    Cons := TConsole.Create(0);
    Adapters.Channel[0].AddDevice(Cons);
    Reader := T0717.Create(1);
    Adapters.Channel[0].AddDevice(Reader);
    Printer := T0773.Create(2);
    Adapters.Channel[0].AddDevice(Printer);
    // Create a uniscope comm adapter for testing
    Adapters.Channel[0].AddDevice(TUniscopeAdapter.Create(4, $21));

    ParseCmdLine;
    if (FConfigFile = '') then
    begin
        // Create a default config if no config file given
        IOTraceEnabled := True;
        SvcTraceEnabled := True;

    //    Adapters.Channel[3].AddDevice(TIDADisk.Create(0, it8418, '..\..\Disks\REL042.8418'));
        Adapters.Channel[3].AddDevice(TIDADisk.Create(0, it8418, '..\..\Disks\SDIVSB.8418'));
        Adapters.Channel[3].AddDevice(TIDADisk.Create(1, it8418, '..\..\Disks\VSBRES.8418'));
    end else
    begin
        // Try to load the config XML document
        fname := FConfigFile;
        if (not FileExists(fname)) then
        begin
            fname := Format('%s%s', [ExtractFilePath(Application.ExeName), FConfigFile]);
            if (not FileExists(fname)) then
            begin
                fname := Format('%s..\..\%s', [ExtractFilePath(Application.ExeName), FConfigFile]);
            end;
        end;
        ConfigXml.LoadFromFile(fname);
        ConfigXml.Active;
        Configuration.Load(ConfigXml);
        IODelay := Configuration.IODelay;
        IOTraceEnabled := Configuration.IOTraceEnabled;
        SvcTraceEnabled := Configuration.SvcTraceEnabled;
        for disk in Configuration.Disks do
        begin
            case disk.DiskType of
              cdt8416:  dtype := it8416;
              cdt8418:  dtype := it8418;
              else      raise Exception.Create('Invalid disk type');
            end;
            idaDisk := TIDADisk.Create(disk.DeviceNum, dtype, disk.DiskFile);
            Adapters.Channel[disk.ChannelNum].AddDevice(idaDisk);
            if (not idaDisk.HasFile) then
                raise Exception.CreateFmt('Could not open file for disk %d%2.2d', [disk.ChannelNum, disk.DeviceNum]);
        end;
    end;
    //
    if (IOTraceEnabled or SvcTraceEnabled) then
    begin
        if (not DirectoryExists(DataDir)) then
            ForceDirectories(DataDir);
    end;
    if (IOTraceEnabled) then
        IOTraceFile := TTraceFile.Create(DataDir + '\U9030IO.trc', fmCreate or fmShareDenyWrite);
    if (SvcTraceEnabled) then
        SvcTraceFile := TTraceFile.Create(DataDir + '\U9030Svc.trc', fmCreate or fmShareDenyWrite);
end;

procedure TU9030Form.ParseCmdLine;
var
    i: Integer;
begin
    i := 1;
    while (i <= ParamCount) do
    begin
        if (ParamStr(i) = '-c') then
        begin
            Inc(i);
            FConfigFile := ParamStr(i);
        end;
        Inc(i);
    end;
end;

procedure TU9030Form.PrintBtnClick(Sender: TObject);
var
    err: String;
    cmd: String;
    si: TStartupInfo;
    pi: TProcessInformation;
begin
    Printer.SaveAs(DataDir + '\print.tmp');

    FillChar(si, SizeOf(si), 0);
    FillChar(pi, SizeOf(pi), 0);
    cmd := 'U9030Print.exe -f "' + DataDir + '\print.tmp"';
    if (not CreateProcess(nil,
                          PWideChar(cmd),
                          nil,
                          nil,
                          False,
                          0,
                          nil,
                          nil,
                          si,
                          pi)) then
    begin
        err := WinError;
        raise Exception.Create(err);
    end;
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
end;

procedure TU9030Form.PrnSaveBtnClick(Sender: TObject);
var
    err: String;
    cmd: String;
    si: TStartupInfo;
    pi: TProcessInformation;
begin
    OpenDlg.Filter := 'Print Files|*.prn';
    OpenDlg.Options := OpenDlg.Options - [ofAllowMultiSelect, ofFileMustExist];
    if (not OpenDlg.Execute) then
        Exit;
    Printer.SaveAs(OpenDlg.FileName);

    FillChar(si, SizeOf(si), 0);
    FillChar(pi, SizeOf(pi), 0);
    cmd := 'notepad.exe ' + OpenDlg.FileName;
    if (not CreateProcess(nil,
                          PWideChar(cmd),
                          nil,
                          nil,
                          False,
                          0,
                          nil,
                          nil,
                          si,
                          pi)) then
    begin
        err := WinError;
        raise Exception.Create(err);
    end;
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
end;

procedure TU9030Form.RdrAttnBtnClick(Sender: TObject);
begin
    Reader.SendAttention;
end;

procedure TU9030Form.RdrEmptyBtnClick(Sender: TObject);
begin
    Reader.Empty;
end;

procedure TU9030Form.RdrLoadBtnClick(Sender: TObject);
var
    fname: String;
begin
    OpenDlg.Filter := 'All Card Files|*.h16;*.h80;*.asc;*.asm;*.rpg;*.jcl|' +
                      'Hollerith 16-bit|*.h16|' +
                      'Hollerith 12-bit|h80|' +
                      'ASCII|*.asc|' +
                      'Assembler Source|*.asm|' +
                      'RPG Source|*.rpg|' +
                      'Job Control|*.jcl';
    OpenDlg.Options := OpenDlg.Options + [ofAllowMultiSelect, ofFileMustExist];
    if (not OpenDlg.Execute) then
        Exit;
    for fname in OpenDlg.Files do
        Reader.AddFile(fname);
end;

procedure TU9030Form.ResetBtnClick(Sender: TObject);
begin
    Processor.Reset;
end;

procedure TU9030Form.RunBtnClick(Sender: TObject);
begin
    if ((Processor.State = []) or ((Processor.State * [psHalted, psError]) <> [])) then
        Processor.Run;
end;

procedure TU9030Form.StartMsg(var Message: TMessage);
begin
    Initialize;
    Timer.Enabled := True;
end;

procedure TU9030Form.StepBtnClick(Sender: TObject);
begin
    if (psSingleStep in Processor.State) then
        Processor.Step(False)
    else
        Processor.Step(True);
end;

procedure TU9030Form.StopBtnClick(Sender: TObject);
begin
    Processor.Stop;
end;

procedure TU9030Form.TimerTimer(Sender: TObject);
var
    s, msg: String;
    r: Integer;
    edt: TEdit;
    si: TStartupInfo;
    pi: TProcessInformation;
    exeDir: String;
begin
    if (not FConsoleStarted) then
    begin
        FConsoleStarted := True;
        FillChar(si, SizeOf(si), 0);
        FillChar(pi, SizeOf(pi), 0);
        exeDir := ExtractFilePath(Application.ExeName);
        if (not CreateProcess(PWideChar(exeDir + 'U9030Console.exe'),
                              nil,
                              nil,
                              nil,
                              False,
                              0,
                              nil,
                              nil,
                              si,
                              pi)) then
        begin
            msg := WinError;
            raise Exception.CreateFmt('Could not start console process. %s', [msg]);
        end;
    end;

    PSWEdt.Text := Format('%1d %1d %x %1d %1d %1d %x %d %2.2x %x %x %1d %1d %1d %1d %6.6x',
                          [Integer(PSW.TimerIntEnabled),
                           Integer(PSW.IOSTIntEnabled),
                           PSW.Key,
                           Integer(PSW.Ascii),
                           Integer(PSW.RegisterSet),
                           Integer(PSW.Mode),
                           Integer(PSW.Emulation),
                           Integer(PSW.MonitorMode),
                           PSW.IntCode,
                           PSW.InstLength,
                           PSW.CondCode,
                           Integer(PSW.FixedOvflExcp),
                           Integer(PSW.DecOvflExcp),
                           Integer(PSW.CharacteristicOvflExcp),
                           Integer(PSW.SignificantExcp),
                           PSW.InstAddr]);
    InstEdt.Text := Format('%12.12x', [CurInst.AsDblWord]);
    RelRegEdt.Text := Format('%6.6x', [Processor.RelocateReg]);
    s := '';
    if (psHalted in Processor.State) then
        s := 'Halted ';
    if (psSingleStep in Processor.State) then
        s := s + 'Step ';
    if (psError in Processor.State) then
        s := s + 'Error ';
    StateLbl.Caption := s;
    for r := 0 to 15 do
    begin
        edt := TEdit(FindComponent(Format('SR%d', [r])));
        edt.Text := Format('%8.8x', [Processor.Registers[rsSupervisor, r]]);
        edt := TEdit(FindComponent(Format('PR%d', [r])));
        edt.Text := Format('%8.8x', [Processor.Registers[rsProgram, r]]);
    end;

    if (Assigned(Reader.CurrentFile)) then
    begin
        ReaderStatusLbl.Caption :=
            Format('%s - %%%d', [ExtractFileName(Reader.CurrentFileName),
                                 Round(((Reader.CurrentFile.Size - Reader.CurrentFile.Position) /
                                       Reader.CurrentFile.Size) * 100)]);
    end else
    begin
        ReaderStatusLbl.Caption := 'Empty';
    end;
    if (Assigned(Printer.PrintFile)) then
    begin
        PrinterStatusLbl.Caption :=
            Format('%dKB', [Round(Printer.PrintFile.Size / 1000)]);
    end else
    begin
        PrinterStatusLbl.Caption := 'Empty';
    end;
end;

end.
