unit U494Panel;

interface

uses
  Winapi.Windows, Winapi.Messages, Winapi.MMSystem, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, U494System, Vcl.ExtCtrls,
  Generics.Collections, U494ReaderFram, Vcl.ComCtrls, U494PunchFram, U494PrinterFram;

type
  TU494DebuggerState = ( udsStep, udsContinue );

  TDebugWatch = packed record
    Address: UInt32;
    Value: UInt32;
  end;

  TU494PanelFrm = class(TForm)
    MaintenancePanel: TPanel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    CpuStatusLbl: TLabel;
    Bevel1: TBevel;
    Label15: TLabel;
    AuditMemo: TMemo;
    CpuRegPanel: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label9: TLabel;
    Label4: TLabel;
    Label16: TLabel;
    IfrEdt: TEdit;
    PlrEdt: TEdit;
    RirEdt: TEdit;
    PEdt: TEdit;
    OperandEdt: TEdit;
    CsrEdt: TEdit;
    IasrEdt: TEdit;
    Panel1: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    AEdt: TEdit;
    QEdt: TEdit;
    Panel2: TPanel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ExecB1Edt: TEdit;
    ExecB2Edt: TEdit;
    ExecB3Edt: TEdit;
    ExecB4Edt: TEdit;
    ExecB5Edt: TEdit;
    ExecB6Edt: TEdit;
    ExecB7Edt: TEdit;
    UserBPanel: TPanel;
    Label19: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    UserB1Edt: TEdit;
    UserB2Edt: TEdit;
    UserB3Edt: TEdit;
    UserB4Edt: TEdit;
    UserB5Edt: TEdit;
    UserB6Edt: TEdit;
    UserB7Edt: TEdit;
    InputEdt: TEdit;
    StartBtn: TButton;
    StopBtn: TButton;
    Switch1Btn: TCheckBox;
    Switch2Btn: TCheckBox;
    Switch3Btn: TCheckBox;
    Switch4Btn: TCheckBox;
    Switch5Btn: TCheckBox;
    Switch6Btn: TCheckBox;
    Switch7Btn: TCheckBox;
    ClearBtn: TButton;
    InterruptsCheck: TCheckBox;
    Timer: TTimer;
    PeripheralsPanel: TPanel;
    PeripheralPages: TPageControl;
    CardPage: TTabSheet;
    Panel4: TPanel;
    ReaderPanel: TPanel;
    PunchPanel: TPanel;
    PrinterPage: TTabSheet;
    PrinterPanel: TPanel;
    PaperTapePage: TTabSheet;
    PaperTapePanel: TPanel;
    Label31: TLabel;
    PTFileNameEdt: TEdit;
    PTBrowseBtn: TButton;
    PTOpenDlg: TOpenDialog;
    PTMountBtn: TButton;
    PTUnmountBtn: TButton;
    PTLoadedLbl: TLabel;
    SR0Edt: TEdit;
    SR1Edt: TEdit;
    SR2Edt: TEdit;
    SR0Lbl: TLabel;
    Label30: TLabel;
    Label32: TLabel;
    procedure TimerTimer(Sender: TObject);
    procedure InputEdtKeyPress(Sender: TObject; var Key: Char);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Switch1BtnClick(Sender: TObject);
    procedure Switch2BtnClick(Sender: TObject);
    procedure Switch3BtnClick(Sender: TObject);
    procedure Switch4BtnClick(Sender: TObject);
    procedure Switch5BtnClick(Sender: TObject);
    procedure Switch6BtnClick(Sender: TObject);
    procedure Switch7BtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure PTBrowseBtnClick(Sender: TObject);
    procedure PTMountBtnClick(Sender: TObject);
    procedure PTUnmountBtnClick(Sender: TObject);
  private
    FConfigFile: String;
    FInTimer: Boolean;
    FConsoleStarted: Boolean;
    FSystem: T494System;
    FOsLoaded: Boolean;
    FDebuggerState: TU494DebuggerState;
    FDebugWaiting: Boolean;
    FBreakpoints: TList<UInt32>;
    FWatches: TList<TDebugWatch>;
    FSkipInterrupts: Boolean;
    FDebugOS: Boolean;
    FTrace: Boolean;
    FLastDispAddr: UInt32;
    FReaderFrame: TU494ReaderFrame;
    FPunchFrame: TU494PunchFrame;
    FPrinterFrame: TU494PrinterFrame;
//    FCmdBuffer: String;
    procedure Breakpoint;
    procedure DisplayMemory;
    procedure DebugContinue;
    procedure DebugNext;
    procedure DebugWait;
    procedure DoDebug(Sender: TObject; E: Exception);
    procedure DoLog(Sender: TObject; addr: UInt32);
    procedure ExecuteCmd;
    function FindWatch(addr: UInt32): Integer;
    procedure Go2;
    procedure IgnoreInterrupt;
    procedure LoadMemory; overload;
    procedure LoadMemory(fname: String); overload;
    procedure ModifyMemory;
    procedure ModifyRegister(words: TStringList);
    procedure ParseCmdLine;
    procedure SetDebug;
    procedure ShowHelp;
    procedure Watch;
    procedure WriteAudit(msg: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  U494PanelFrm: TU494PanelFrm;

implementation

uses U494Memory, U494CPU, U494Util, U494Opcodes, ObjFile, EmulatorTypes, U494Config;

{$R *.dfm}

procedure TU494PanelFrm.Breakpoint;
var
    words: TStringList;
    addr, i: Integer;
begin
    if (not Assigned(FSystem.Cpu.OnDebug)) then
        Exit;

    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        try
            if (words.Count >= 2) then
            begin
                if (words[1][Length(words[1])] = 'D') then
                    addr := StrToInt(Copy(words[1], 1, Length(words[1]) - 1))
                else
                    addr := Octal(words[1]);
                text := '';
                i := FBreakpoints.IndexOf(addr);
                if (i = -1) then
                begin
                    FBreakpoints.Add(addr);
                    WriteAudit(Format('Breakpoint added at %s', [Copy(FormatOctal(addr), 5)]));
                end else
                begin
                    FBreakpoints.Delete(i);
                    WriteAudit(Format('Breakpoint at %s cleared', [Copy(FormatOctal(addr), 5)]));
                end;
            end else
            begin
                WriteAudit('Breakpoints:');
                for i := 0 to FBreakpoints.Count - 1 do
                begin
                    WriteAudit(Format('%s', [Copy(FormatOctal(FBreakpoints[i]), 5)]));
                end;
            end;
        except
          on E: Exception do
          begin
              WriteAudit(E.Message);
              Exit;
          end;
        end;
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.ClearBtnClick(Sender: TObject);
begin
    if (cshalted in FSystem.Cpu.State) then
        FSystem.Cpu.Clear;
end;

constructor TU494PanelFrm.Create(AOwner: TComponent);
var
    tc: TIMECAPS;
begin
    inherited;
    try
        ParseCmdLine;
        if (FConfigFile <> '') then
            gConfig.Load(Self, FConfigFile);
        FSystem := T494System.Create;
        FBreakpoints := TList<UInt32>.Create;
        FWatches := TList<TDebugWatch>.Create;
        PeripheralPages.ActivePageIndex := 0;
        if (gConfig.RdrPunChan <> -1) then
        begin
            FReaderFrame := TU494ReaderFrame.Create(Self, FSystem.Reader);
            FReaderFrame.Parent := ReaderPanel;
            FReaderFrame.Align := alClient;
            FPunchFrame := TU494PunchFrame.Create(Self, FSystem.Punch);
            FPunchFrame.Parent := PunchPanel;
            FPunchFrame.Align := alClient;
        end else
            CardPage.TabVisible := False;
        if (gConfig.PrinterChan <> -1) then
        begin
            FPrinterFrame := TU494PrinterFrame.Create(Self, FSystem.Printer);
            FPrinterFrame.Parent := PrinterPanel;
            FPrinterFrame.Align := alClient;
        end else
            PrinterPage.TabVisible := False;
        if (gConfig.Mode <> m494) then
            UserBPanel.Visible := False;
        FDebuggerState := udsContinue;
        FSystem.Cpu.OnLog := DoLog;
        FDebugOS := True;
        // Set the Sleep timer to the minimum resolution available. This
        // will allow the real time clock interrupt to occur as frequently as
        // possible. It still won't be 200 microseconds but we will just have
        // to adapt as well as possible.
        timeGetDevCaps(@tc, SizeOf(tc));
        timeBeginPeriod(tc.wPeriodMin);
    except
      on E: Exception do
      begin
        ShowException(E, ExceptAddr);
        Halt(1);
      end;
    end;
end;

procedure TU494PanelFrm.DebugContinue;
begin
    if (Assigned(FSystem.Cpu.OnDebug)) then
    begin
        FDebuggerState := udsContinue;
        if (FDebugWaiting) then
            FDebugWaiting := False
        else
            FSystem.Cpu.Start;
    end;
end;

procedure TU494PanelFrm.DebugNext;
begin
    if (Assigned(FSystem.Cpu.OnDebug)) then
    begin
        FDebuggerState := udsStep;
        if (FDebugWaiting) then
            FDebugWaiting := False
        else
            FSystem.Cpu.Start;
    end;
end;

procedure TU494PanelFrm.DebugWait;
begin
    FDebugWaiting := True;
    while (FDebugWaiting) do
    begin
        Sleep(10);
        Application.ProcessMessages;
    end;
end;

destructor TU494PanelFrm.Destroy;
var
    tc: TIMECAPS;
begin
    FreeAndNil(FSystem);
    FreeAndNil(FBreakpoints);
    FreeAndNil(FWatches);
    // Reset timer interval to system default
    timeGetDevCaps(@tc, SizeOf(tc));
    timeEndPeriod(tc.wPeriodMin);
    inherited;
end;

procedure TU494PanelFrm.DisplayMemory;
var
    words: TStringList;
    count, count1: Integer;
    addr, value: UInt32;
    text, ascii: String;
begin
    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        try
            if (words.Count >= 2) then
            begin
                if (words[1][Length(words[1])] = 'D') then
                    addr := StrToInt(Copy(words[1], 1, Length(words[1]) - 1))
                else
                    addr := Octal(words[1]);
            end else
                addr := FLastDispAddr;
            addr := addr + FSystem.Memory.RIR.Value;
            text := Format('%s/%s: ',
                           [Copy(FormatOctal(FSystem.Memory.RIR.Value), 5),
                            Copy(FormatOctal(addr - FSystem.Memory.RIR.Value), 5)]);
            ascii := StringOfChar(' ', 15);
            for count := 1 to 5 do
            begin
                value := FSystem.Memory.Fetch(addr, True).Value;
                text := text + FormatOctal(value) + ' ';
                for count1 := 1 to 5 do
                    ascii := ascii + ' ' + String(TCodeTranslator.FieldataToAscii((value shr (30 - (6 * count1))) and $3f));
                ascii := ascii + ' ';
                Inc(addr);
            end;
            WriteAudit(text);
            WriteAudit(ascii);
            FLastDispAddr := addr - FSystem.Memory.RIR.Value;
        except
          on E: Exception do
          begin
              WriteAudit(E.Message);
              Exit;
          end;
        end;
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.DoDebug(Sender: TObject; E: Exception);
const
    kdesig_read: array [0..7] of String = ( '', ',L', ',U', ',W', ',X', ',LX', ',UX', ',A' );
    kdesig_store: array [0..7] of String = ( ',Q', ',L', ',U', ',W', ',A', ',CPL', ',CPU', ',CPW' );
    kdesig_repl: array [0..7] of String = ( '', ',L', ',U', ',W', '', ',LX', ',UX', '' );
    bdesig: array [0..7] of String = ( '', ',B1', ',B2', ',B3', ',B4', ',B5', ',B6', ',B7' );
var
    p: UInt32;
    f, g: Byte;
    op: T494Opcode;
    j, k, b: String;
    mnemonic: String;
    val: UInt32;
    w: TDebugWatch;

    procedure DisAssemble;
    begin
        b := bdesig[FSystem.Memory.Inst.b];
        f := FSystem.Memory.Inst.f;
        if (f = $3f) then
        begin
            g := FSystem.Memory.Inst.g;
            case gConfig.Mode of
              m494:
              begin
                op := U494ExtOpcodes[g];
                mnemonic := op.AsmMnemonic;
              end;
              m490:
              begin
                op := U494ExtOpcodes[g];
                mnemonic := op.SpurtMnemonic;
              end;
              m1230:
              begin
                op := U1230ExtOpcodes[g];
                mnemonic := op.SpurtMnemonic;
              end;
            end;
            WriteAudit(Format('%s/%s: %s %-10.10s %s%s',
                              [Copy(FormatOctal(FSystem.Memory.RIR.Value), 5),
                               Copy(FormatOctal(p - FSystem.Memory.RIR.Value), 5),
                               FormatOctal(FSystem.Memory.Inst.Value),
                               mnemonic,
                               Copy(FormatOctal(FSystem.Memory.Inst.y.Value15), 6),
                               b]));
        end else
        begin
            op := U494StdOpcodes[f];
            case gConfig.Mode of
              m494:     mnemonic := op.AsmMnemonic;
              m490:     mnemonic := op.SpurtMnemonic;
              m1230:    mnemonic := op.SpurtMnemonic;
            end;
            if (op.InstType = itRead) then
                k := kdesig_read[FSystem.Memory.Inst.k]
            else if (op.InstType = itStore) then
                k := kdesig_store[FSystem.Memory.Inst.k]
            else if (op.InstType = itReplace) then
                k := kdesig_repl[FSystem.Memory.Inst.k]
            else
            begin
                if (FSystem.Memory.Inst.b <> 0) then
                    k := Format(',%d', [FSystem.Memory.Inst.k]);
            end;
            if (op.OperandType = otBRegister) then
            begin
                if (FSystem.Memory.Inst.j = 0) then
                    j := 'B0,'
                else
                    j := Copy(bdesig[FSystem.Memory.Inst.j], 2) + ',';
                WriteAudit(Format('%s/%s: %s %-10.10s %s%s%s',
                                  [Copy(FormatOctal(FSystem.Memory.RIR.Value), 5),
                                   Copy(FormatOctal(p - FSystem.Memory.RIR.Value), 5),
                                   FormatOctal(FSystem.Memory.Inst.Value),
                                   mnemonic + k,
                                   j,
                                   Copy(FormatOctal(FSystem.Memory.Inst.y.Value15), 6),
                                   b]));
            end else
            begin
                if (FSystem.Memory.Inst.j <> 0) then
                    j := Format(',%d', [FSystem.Memory.Inst.j]);
                if ((j <> '') and (b = '')) then
                    b := ',';
                WriteAudit(Format('%s/%s: %s %-10.10s %s%s%s',
                                  [Copy(FormatOctal(FSystem.Memory.RIR.Value), 5),
                                   Copy(FormatOctal(p - FSystem.Memory.RIR.Value), 5),
                                   FormatOctal(FSystem.Memory.Inst.Value),
                                   mnemonic + k,
                                   Copy(FormatOctal(FSystem.Memory.Inst.y.Value15), 6),
                                   b,
                                   j]));
            end;
        end;
    end;

begin
    if (Assigned(E)) then
        WriteAudit(Format('**** %s', [E.Message]));
    //
    InputEdt.Text := '';
    j := '';
    k := '   ';
    b := '';
    p := FSystem.Memory.P.Value;

    if (FSkipInterrupts and FSystem.Cpu.InterruptActive) then
        Exit;

    if (FDebugOS and (FSystem.Memory.RIR.Value <> 0)) then
        Exit;

    if ((not FDebugOS) and (FSystem.Memory.RIR.Value = 0)) then
        Exit;

    if (FBreakPoints.IndexOf(p - FSystem.Memory.RIR.Value) <> -1) then
    begin
        FDebuggerState := udsStep;
        WriteAudit(Format('Breakpoint @ %s', [Copy(FormatOctal(p - FSystem.Memory.RIR.Value), 6)]));
    end;

    for w in FWatches do
    begin
        val := FSystem.Memory.Fetch(w.Address, True).value;
        if (w.Value <> val) then
        begin
            FDebuggerState := udsStep;
            WriteAudit(Format('Watch @ %s has changed value', [Copy(FormatOctal(w.Address), 6)]));
        end;
    end;

    if (FDebuggerState = udsStep) then
    begin
        DisAssemble;
        DebugWait;
    end else if (FTrace) then
    begin
        DisAssemble;
    end;
end;

procedure TU494PanelFrm.DoLog(Sender: TObject; addr: UInt32);
var
    len, count, word: UInt32;
    value: T494Word;
    b: Byte;
    s, stemp: AnsiString;
begin
    s := '';
    len := FSystem.Memory.Fetch(addr, True);
    Inc(addr);
    while (len > 0) do
    begin
        value := FSystem.Memory.Fetch(addr, True);
        word := value.Value;
        stemp := '';
        for count := 1 to 5 do
        begin
            b := word and $3f;
            word := word shr 6;
            stemp := TCodeTranslator.FieldataToAscii(b) + stemp;
        end;
        s := s + stemp;
        Inc(addr);
        Dec(len);
    end;
    WriteAudit(String(s));
end;

procedure TU494PanelFrm.ExecuteCmd;
var
    cmd: Char;
begin
    InputEdt.Text := UpperCase(Trim(InputEdt.Text));
    if (InputEdt.Text = '') then
        Exit;
    WriteAudit(InputEdt.Text);
    cmd := InputEdt.Text[1];
    case cmd of
      'B':  SetDebug;
      'C':  DebugContinue;
      'D':  DisplayMemory;
      'G':  Go2;
      'I':  IgnoreInterrupt;
      'L':  LoadMemory;
      'M':  ModifyMemory;
      'N':  DebugNext;
      'R':  Breakpoint;
      'W':  Watch;
      else  WriteAudit('Unrecognized command');
    end;
end;

function TU494PanelFrm.FindWatch(addr: UInt32): Integer;
begin
    for Result := 0 to FWatches.Count - 1 do
    begin
        if (FWatches[Result].Address = addr) then
            Exit;
    end;
    Result := -1;
end;

procedure TU494PanelFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    FSystem.Cpu.Stop;
    FSystem.Cpu.OnDebug := nil;
    FDebugWaiting := False;
    CanClose := True;
end;

procedure TU494PanelFrm.FormShow(Sender: TObject);
begin
    InputEdt.SetFocus;
    Timer.Enabled := True;
end;

procedure TU494PanelFrm.Go2;
var
    words: TStringList;
    addr: Integer;
begin
    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        if (words.Count < 2) then
        begin
            WriteAudit('Not enough parameters');
            Exit;
        end;
        try
            if (words[1][Length(words[1])] = 'D') then
                addr := StrToInt(Copy(words[1], 1, Length(words[1]) - 1))
            else
                addr := Octal(words[1]);
            if (addr > MemSize) then
                raise Exception.Create('Address out of range');
            FSystem.Memory.P := addr;
        except
          on E: Exception do
          begin
              WriteAudit(E.Message);
              Exit;
          end;
        end;
        StartBtnClick(nil);
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.SetDebug;
var
    words: TStringList;
    msg: String;
    i: Integer;
begin
    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        for i := 1 to words.Count - 1 do
        begin
            if (words[i] = 'ON') then
                FSystem.Cpu.OnDebug := DoDebug
            else if (words[i] = 'OFF') then
                FSystem.Cpu.OnDebug := nil
            else if (words[i] = 'TRACE') then
                FTrace := True
            else if (words[i] = 'NOTRACE') then
                FTrace := False
            else if (words[i] = 'OS') then
                FDebugOS := True
            else if (words[i] = 'JOB') then
            begin
                FDebugOS := False;
                FSkipInterrupts := True;
            end else
            begin
                WriteAudit(Format('Invalid option (%s)', [words[i]]));
            end;
        end;
        msg := 'Debug ';
        if (Assigned(FSystem.Cpu.OnDebug)) then
            msg := msg + 'ON '
        else
            msg := msg + 'OFF ';
        if (FTrace) then
            msg := msg + 'TRACE '
        else
            msg := msg + 'NOTRACE ';
        if (FDebugOS) then
            msg := msg + 'OS '
        else
            msg := msg + 'JOB ';
        if (FSkipInterrupts) then
            msg := msg + 'Interrupts skipped'
        else
            msg := msg + 'Interrupts not skipped';
        WriteAudit(msg);
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.ShowHelp;
const
    help: array [1..20] of String = (
        'Commands: ',
        '',
        '(g)oto address',
        '(m)odify address value',
        '(m)odify reg value',
        '(d)isplay [address]',
        '(i)nterrupt_skip',
        '(l)oad memory_image_file',
        'de(b)ug on|off trace|notrace os|job',
        'The following are only effective debugging is on:',
        '(n)ext',
        '(c)ontinue',
        'b(r)eakpoint [address]',
        '(w)atch address',
        '',
        'Addresses and values can be given as ether decimal or octal values.',
        'Decimal values must end with a D i.e. 123D.',
        '',
        'The reg paramter may be any of the register names shown in the panel above.',
        ''
    );
var
    i: Integer;
begin
    for i := Low(help) to High(help) do
        WriteAudit(help[i]);
end;

procedure TU494PanelFrm.StartBtnClick(Sender: TObject);
begin
    InputEdt.Enabled := False;
    try
        FSystem.Cpu.Start;
    finally
        InputEdt.Enabled := True;
    end;
end;

procedure TU494PanelFrm.StopBtnClick(Sender: TObject);
begin
    FSystem.Cpu.Stop;
end;

procedure TU494PanelFrm.Switch1BtnClick(Sender: TObject);
begin
    if (Switch1Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps1]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps1]
end;

procedure TU494PanelFrm.Switch2BtnClick(Sender: TObject);
begin
    if (Switch2Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps2]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps2]
end;

procedure TU494PanelFrm.Switch3BtnClick(Sender: TObject);
begin
    if (Switch3Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps3]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps3]
end;

procedure TU494PanelFrm.Switch4BtnClick(Sender: TObject);
begin
    if (Switch4Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps4]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps4]
end;

procedure TU494PanelFrm.Switch5BtnClick(Sender: TObject);
begin
    if (Switch5Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps5]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps5]
end;

procedure TU494PanelFrm.Switch6BtnClick(Sender: TObject);
begin
    if (Switch6Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps6]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps6]
end;

procedure TU494PanelFrm.Switch7BtnClick(Sender: TObject);
begin
    if (Switch7Btn.Checked) then
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches + [ps7]
    else
        FSystem.Cpu.PanelSwitches := FSystem.Cpu.PanelSwitches - [ps7]
end;

procedure TU494PanelFrm.IgnoreInterrupt;
begin
    if (Assigned(FSystem.Cpu.OnDebug)) then
    begin
        FSkipInterrupts := not FSkipInterrupts;
        if (FSkipInterrupts) then
            WriteAudit('Interrupts will be skipped')
        else
            WriteAudit('Interrupts will not be skipped');
    end;
end;

procedure TU494PanelFrm.InputEdtKeyPress(Sender: TObject; var Key: Char);
begin
    case Key of
      '?':
      begin
        if (Length(InputEdt.Text) = 0) then
        begin
            ShowHelp;
            Key := Chr(0);
        end;
      end;
      'C',
      'c':
      begin
        if (Length(InputEdt.Text) = 0) then
        begin
            InputEdt.Text := '';
            DebugContinue;
            Key := Chr(0);
        end;
      end;
      'I',
      'i':
      begin
        if (Length(InputEdt.Text) = 0) then
        begin
            InputEdt.Text := '';
            IgnoreInterrupt;
            Key := Chr(0);
        end;
      end;
      'N',
      'n':
      begin
        if (Length(InputEdt.Text) = 0) then
        begin
            InputEdt.Text := '';
            DebugNext;
            Key := Chr(0);
        end;
      end;
      Chr(13),
      Char(10):
      begin
        if (Length(InputEdt.Text) > 0) then
            ExecuteCmd;
        InputEdt.Text := '';
        Key := Chr(0);
      end;
    end;
end;

procedure TU494PanelFrm.LoadMemory(fname: String);
var
    fobj: TMemImageStream;
    rel: TRelocatableType;
    value, addr: UInt32;
    word: T494Word;
begin
    AuditMemo.Lines.Add(Format('Loading %s', [fname]));
    fobj := nil;
    try
        fobj := TMemImageStream.Create(fname, fmOpenRead or fmShareDenyNone);
        while (fobj.FetchWord(addr, rel, value)) do
        begin
            word.Value := value;
            FSystem.Memory.Store(addr, word, True);
        end;
        FSystem.Memory.P.Value := fobj.TransAddr;
        fobj.Free;
    except
      on E: Exception do
      begin
          WriteAudit(E.Message);
          fobj.Free;
          Exit;
      end;
    end;
end;

procedure TU494PanelFrm.LoadMemory;
var
    words: TStringList;
    fname: String;
begin
    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        if (words.Count < 2) then
        begin
            WriteAudit('Not enough parameters');
            Exit;
        end;
        fname := words[1];
        LoadMemory(fname);
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.ModifyMemory;
var
    words: TStringList;
    addr: UInt32;
    value: T494Word;
begin
    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        if (words.Count < 3) then
        begin
            WriteAudit('Not enough parameters');
            Exit;
        end;
        try
            if ((words[1][1] < '0') or (words[1][1] > '9')) then
            begin
                ModifyRegister(words);
                Exit;
            end;
            if (words[1][Length(words[1])] = 'D') then
                addr := StrToInt(Copy(words[1], 1, Length(words[1]) - 1))
            else
                addr := Octal(words[1]);
            addr := addr + FSystem.Memory.RIR.Value;
            if (words[2][Length(words[2])] = 'D') then
                value := StrToInt(Copy(words[2], 1, Length(words[2]) - 1))
            else
                value.Value := Octal(words[2]);
            if (addr > MemSize) then
                raise Exception.Create('Address out of range');
            FSystem.Memory.Store(addr, value, True);
        except
          on E: Exception do
          begin
              WriteAudit(E.Message);
              Exit;
          end;
        end;
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.ModifyRegister(words: TStringList);
var
    value: T494Word;
begin
    try
        if (words[2][Length(words[2])] = 'D') then
            value := StrToInt(Copy(words[2], 1, Length(words[2]) - 1))
        else
            value.Value := Octal(words[2]);
        if (words[1] = 'IFR') then
            FSystem.Memory.IFR.Value := value.Value
        else if (words[1] = 'PLR') then
             FSystem.Memory.PLR.Value := value.Value
        else if (words[1] = 'RIR') then
             FSystem.Memory.RIR.Value := value.Value
        else if (words[1] = 'P') then
             FSystem.Memory.P.Value := value.Value
        else if (words[1] = 'A') then
             FSystem.Memory.A := value
        else if (words[1] = 'Q') then
             FSystem.Memory.Q := value
        else if (words[1] = 'SR0') then
             FSystem.Memory.SR[0].Value := value.Value
        else if (words[1] = 'SR1') then
             FSystem.Memory.SR[1].Value := value.Value
        else if (words[1] = 'SR2') then
             FSystem.Memory.SR[2].Value := value.Value
        else if (words[1] = 'K') then
             FSystem.Memory.K := value
        else if (words[1] = 'EB1') then
             FSystem.Memory.B[0, 1].Value := value.Value
        else if (words[1] = 'EB2') then
             FSystem.Memory.B[0, 2].Value := value.Value
        else if (words[1] = 'EB3') then
             FSystem.Memory.B[0, 3].Value := value.Value
        else if (words[1] = 'EB4') then
             FSystem.Memory.B[0, 4].Value := value.Value
        else if (words[1] = 'EB5') then
             FSystem.Memory.B[0, 5].Value := value.Value
        else if (words[1] = 'EB6') then
             FSystem.Memory.B[0, 6].Value := value.Value
        else if (words[1] = 'EB7') then
             FSystem.Memory.B[0, 7].Value := value.Value
        else if (words[1] = 'UB1') then
             FSystem.Memory.B[1, 1].Value := value.Value
        else if (words[1] = 'UB2') then
             FSystem.Memory.B[1, 2].Value := value.Value
        else if (words[1] = 'UB3') then
             FSystem.Memory.B[1, 3].Value := value.Value
        else if (words[1] = 'UB4') then
             FSystem.Memory.B[1, 4].Value := value.Value
        else if (words[1] = 'UB5') then
             FSystem.Memory.B[1, 5].Value := value.Value
        else if (words[1] = 'UB6') then
             FSystem.Memory.B[1, 6].Value := value.Value
        else if (words[1] = 'UB7') then
             FSystem.Memory.B[1, 7].Value := value.Value;
    except
      on E: Exception do
      begin
          WriteAudit(E.Message);
          Exit;
      end;
    end;
end;

procedure TU494PanelFrm.ParseCmdLine;
var
    i: Integer;
begin
    for i := 0 to ParamCount do
    begin
        if (ParamStr(i) = '-c') then
            FConfigFile := ParamStr(i + 1);
    end;
end;

procedure TU494PanelFrm.PTBrowseBtnClick(Sender: TObject);
begin
    if (not PTOpenDlg.Execute) then
        Exit;
    PTFileNameEdt.Text := PTOpenDlg.FileName;
end;

procedure TU494PanelFrm.PTMountBtnClick(Sender: TObject);
begin
    if (PTFileNameEdt.Text = '') then
        raise Exception.Create('Please enter the paper tape file name');
    FSystem.Console.PunchFile := PTFileNameEdt.Text;
    PTLoadedLbl.Caption := Format('%s loaded', [PTFileNameEdt.Text]);
    PTLoadedLbl.Visible := True;
end;

procedure TU494PanelFrm.PTUnmountBtnClick(Sender: TObject);
begin
    FSystem.Console.PunchFile := '';
    PTLoadedLbl.Visible := False;
end;

procedure TU494PanelFrm.TimerTimer(Sender: TObject);
var
    msg, fname: String;
    i: Integer;
    si: TStartupInfo;
    pi: TProcessInformation;
begin
    if (FInTimer) then
        Exit;

    if (not FOsLoaded) then
    begin
        case gConfig.Mode of
          m490:     AuditMemo.Lines.Add('490 mode');
          m494:     AuditMemo.Lines.Add('494 mode');
          m1230:    AuditMemo.Lines.Add('1230 mode');
        end;
        for i := 0 to gConfig.LoadFileCount - 1 do
        begin
            // Try production folder first, then development folder if not found
            fname := PublicDataDir + '\Univac 494 Emulator\Data\' + gConfig.LoadFiles[i];
            if (not FileExists(fname)) then
                fname := '..\..\Bin\' + gConfig.LoadFiles[i];
            LoadMemory(fname);
            InputEdt.Text := '';
            FOsLoaded := True;
        end;
    end;

    if (not FConsoleStarted) then
    begin
        FConsoleStarted := True;
        if (not CreateProcess('U494Console.exe',
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

    FInTimer := True;
    try
        IfrEdt.Text := FormatOctal(FSystem.Memory.IFR.Value);
        PlrEdt.Text := FormatOctal(FSystem.Memory.PLR.Value);
        RirEdt.Text := Copy(FormatOctal(FSystem.Memory.RIR.ActualValue), 5);
        OperandEdt.Text := Copy(FormatOctal(FSystem.Memory.Operand.Value), 5);
        PEdt.Text := Copy(FormatOctal(FSystem.Memory.P.Value), 5);
        CsrEdt.Text := Copy(FormatOctal(FSystem.Memory.CSR.Value), 9);
        IasrEdt.Text := Copy(FormatOctal(FSystem.Memory.IASR.Value), 9);
        AEdt.Text := FormatOctal(FSystem.Memory.A.Value);
        QEdt.Text := FormatOctal(FSystem.Memory.Q.Value);
        SR0Edt.Text := Copy(FormatOctal(FSystem.Memory.SR[0].Value), 9);
        SR1Edt.Text := Copy(FormatOctal(FSystem.Memory.SR[1].Value), 9);
        SR2Edt.Text := Copy(FormatOctal(FSystem.Memory.SR[2].Value), 9);
        ExecB1Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 1].Value), 5);
        ExecB2Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 2].Value), 5);
        ExecB3Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 3].Value), 5);
        ExecB4Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 4].Value), 5);
        ExecB5Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 5].Value), 5);
        ExecB6Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 6].Value), 5);
        ExecB7Edt.Text := Copy(FormatOctal(FSystem.Memory.B[0, 7].Value), 5);
        UserB1Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 1].Value), 5);
        UserB2Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 2].Value), 5);
        UserB3Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 3].Value), 5);
        UserB4Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 4].Value), 5);
        UserB5Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 5].Value), 5);
        UserB6Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 6].Value), 5);
        UserB7Edt.Text := Copy(FormatOctal(FSystem.Memory.B[1, 7].Value), 5);
        InterruptsCheck.Checked := FSystem.Cpu.InterruptLockout;
        if (csHalted in FSystem.Cpu.State) then
        begin
            CpuStatusLbl.Font.Color := clRed;
            CpuStatusLbl.Caption := 'Halted';
        end else
        begin
            CpuStatusLbl.Caption := 'Running';
            CpuStatusLbl.Font.Color := clWindowText;
        end;
    finally
        FInTimer := False;
    end;
end;

procedure TU494PanelFrm.Watch;
var
    words: TStringList;
    addr, i: Integer;
    w: TDebugWatch;
begin
    if (not Assigned(FSystem.Cpu.OnDebug)) then
        Exit;

    words := TStringList.Create;
    try
        words.Delimiter := ' ';
        words.StrictDelimiter := False;
        words.DelimitedText := InputEdt.Text;
        if (words.Count < 2) then
        begin
            WriteAudit('Not enough parameters');
            Exit;
        end;
        try
            if (words[1][Length(words[1])] = 'D') then
                addr := StrToInt(Copy(words[1], 1, Length(words[1]) - 1))
            else
                addr := Octal(words[1]);
            if (addr > MemSize) then
                raise Exception.Create('Address out of range');
            w.Address := addr;
            i := FindWatch(addr);
            if (i = -1) then
            begin
                w.Value := FSystem.Memory.Fetch(addr, True).Value;
                FWatches.Add(w);
                WriteAudit(Format('Watch added at %s', [Copy(FormatOctal(addr), 5)]));
            end else
            begin
                FWatches.Delete(i);
                WriteAudit(Format('Watch at %s cleared', [Copy(FormatOctal(addr), 5)]));
            end;
        except
          on E: Exception do
          begin
              WriteAudit(E.Message);
              Exit;
          end;
        end;
    finally
        words.Free;
    end;
end;

procedure TU494PanelFrm.WriteAudit(msg: String);
begin
    AuditMemo.Lines.Add(msg);
    while (AuditMemo.Lines.Count > 25) do
        AuditMemo.Lines.Delete(0);
end;

end.
