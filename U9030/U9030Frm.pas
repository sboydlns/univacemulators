unit U9030Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

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
    PrtBrkptBtn: TButton;
    PrtFileNameEdt: TEdit;
    PrtBrowseBtn: TButton;
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
  private
    FConsoleStarted: Boolean;
    function Hex(s: String): Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  U9030Form: TU9030Form;

implementation

{$R *.dfm}

uses DebuggerFrm, U9030Types, Globals, Channels, Cpu, Memory, IDA, IPC, Console, Trace,
     U0773, CpuTestFrm;

var
    Cons: TConsole;
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
begin
    inherited;

    IOTraceEnabled := True;
    SvcTraceEnabled := True;

    Opcodes := TOpcodeList.Create;
    Core := TMemory.Create;
    Processor := TCpu.Create;
    PSW := TPSW.Create;
    CurInst := TInstruction.Create;
    Adapters := TChannelList.Create;

    Adapters.Channel[0] := TIPC.Create(0);
    Cons := TConsole.Create(0);
    Adapters.Channel[0].AddDevice(Cons);
    Adapters.Channel[0].AddDevice(T0773.Create(2));

    Adapters.Channel[1] := TChannel.Create(1);

    Adapters.Channel[3] := TIDA.Create(3);
    Adapters.Channel[3].AddDevice(TIDADisk.Create(0, it8418, '..\..\Disks\REL042.8418'));

    Adapters.Channel[4] := TChannel.Create(4);

    Adapters.Channel[6] := TChannel.Create(6);
end;

procedure TU9030Form.DebugBtnClick(Sender: TObject);
begin
    DebuggerForm.Show;
    Processor.OnDebug := DebuggerForm.DoDebug;
end;

destructor TU9030Form.Destroy;
begin
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
    Timer.Enabled := True;
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

procedure TU9030Form.RunBtnClick(Sender: TObject);
begin
    if ((Processor.State = []) or ((Processor.State * [psHalted, psError]) <> [])) then
        Processor.Run;
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
begin
    if (not FConsoleStarted) then
    begin
        FConsoleStarted := True;
        FillChar(si, SizeOf(si), 0);
        FillChar(pi, SizeOf(pi), 0);
        if (not CreateProcess('C:\Development\Emulators\U9030\Win32\Debug\U9030Console.exe',
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
end;

end.
