unit U494Cpu;

interface

uses SysUtils, Classes, Forms, Generics.Collections, U494Util, U494Memory, U494Opcodes,
     U494Interrupts, SyncObjs, CardFile;

type
  TDebugEvent = procedure(Sender: TObject; E: Exception) of object;
  TLogEvent = procedure(Sender: TObject; addr: UInt32) of object;

  TInstProc = procedure of object;

  TPanelSwitch = ( ps1, ps2, ps3, ps4, ps5, ps6, ps7 );

  TPanelSwitches = set of TPanelSwitch;

  T494Cpu = class;

  T494Device = class(TThread)
  protected
    FCpu: T494Cpu;
    FMemory: T494Memory;
    FFunction: T494Word;
    FChannel: Byte;
    FStatus: UInt32;
    FEvent: TEvent;
    FCrit: TCriticalSection;
    FWaitExec: Boolean;
    FExternalActive: Boolean;
    FInputActive: Boolean;
    FOutputActive: Boolean;
    FExternalMonitor: Boolean;
    FInputMonitor: Boolean;
    FOutputMonitor: Boolean;
    FInpIntLockout: Boolean;
    FOutIntLockout: Boolean;
    FExtIntLockout: Boolean;
    function FetchInputBcr: T494Word;
    function FetchOutputBcr: T494Word;
    procedure Lock;
    procedure QueueInterrupt(itype: T494InterruptType; vector: Smallint; status: UInt32); virtual;
    procedure StoreInputBcr(Value: T494Word);
    procedure StoreOutputBcr(Value: T494Word);
    procedure Unlock;
public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); virtual;
    destructor Destroy; override;
    procedure ActivateExternal(withMon: Boolean); virtual;
    procedure ActivateInput(withMon: Boolean); virtual;
    procedure ActivateOutput(withMon: Boolean); virtual;
    procedure Clear; virtual; abstract;
    procedure ExternalFunction(func: T494Word); virtual; abstract;
    procedure ExternalFunctionWithMonitor(func: T494Word); virtual;
    function ExternalActive: Boolean; virtual;
    function InputActive: Boolean; virtual;
    function OutputActive: Boolean; virtual;
    procedure Terminate; reintroduce;
    procedure TerminateExternal; virtual;
    procedure TerminateInput; virtual;
    procedure TerminateOutput; virtual;
    property InpIntLockout: Boolean read FInpIntLockout write FInpIntLockout;
    property OutIntLockout: Boolean read FOutIntLockout write FOutIntLockout;
    property ExtIntLockout: Boolean read FExtIntLockout write FExtIntLockout;
  end;

  T494CardDevice = class(T494Device)
  protected
    FFiles: TCardFileList;
    FCurrentFile: TCardFileStream;
    FInputCount: Integer;
    FHopperEmpty: Boolean;
    function OpenNextFile: Boolean; virtual;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure AddFile(fname: String; rpgType: String = ''); virtual;
    procedure AddBlankCards(count: Integer); virtual;
    property HopperEmpty: Boolean read FHopperEmpty;
    property InputCount: Integer read FInputCount;
  end;

  T494ChannelList = class(TList<T494Device>)
  public
    constructor Create;
  end;

  T494Cpu = class
  private
    FState: T494CpuState;
    FMemory: T494Memory;
    FPanelSwitches: TPanelSwitches;
    FStdInstProcs: array [0..63] of TInstProc;
    FExtInstProcs: array [0..63] of TInstProc;
    FCurOpcode: T494Opcode;
    FCurInstProc: TInstProc;
    FInterrupts: T494InterruptQueue;
    FChannels: T494ChannelList;
    FInterruptLockout: Boolean;
    FExtIntLockout: Boolean;                                // External interrupt lockout
    FInterruptPending: Boolean;                             // Unconditional interrupt pending flag
    FInterruptActive: Boolean;                              // I/O interrupt active
    FInterruptVector: T494Address;                          //    "      "     "    "  vector address
    FPLockedOut: Boolean;                                   // P register locked out for 1st inst of interrupt
    FRepeatDelay: Boolean;
    FExecRemotePending: Boolean;
    FExecRemoteAddr: T494Address;
    FOnDebug: TDebugEvent;
    FOnLog: TLogEvent;
    procedure AddQSkip;
    procedure AQStore(value: T494Word);
    procedure FloatToNative(r: Double; var w1, w2: UInt32);
    procedure IllegalInst;
    function IOFetch: T494Word;
    function LeftShift(value: UInt32; count: Integer): UInt32; overload;
    function LeftShift(value: UInt64; count: Integer): UInt64; overload;
    procedure LogicalProductSkip;
    function LogicalRightShift(value: UInt32; count: Integer): UInt32;
    function NativeToFloat(w1, w2: UInt32): Double;
    procedure NormalSkip;
    procedure NotImplemented;
    procedure Plus1Skip;
    function RightShift(value: UInt32; count: Integer): UInt32;
    function StdFetch: T494Word;
    procedure StdStore(value: T494Word);
    // Instruction implementation methods
    procedure A;
    procedure ALP;
    procedure AN;
    procedure ANLP;
    procedure ANQ;
    procedure AQ;
    procedure CPL;
    procedure CPU;
    procedure CUL;
    procedure CUU;
    procedure D;
    procedure D17;
    procedure DA;
    procedure DAC;
    procedure DAN;
    procedure DANB;
    procedure DCL;
    procedure DCU;
    procedure DICDM;
    procedure DN;
    procedure DOCDM;
    procedure DPA;
    procedure DPAN;
    procedure DPL;
    procedure DPN;
    procedure DPS;
    procedure DPTE;
    procedure DPTL;
    procedure DT;
    procedure DTE;
    procedure DTL;
    procedure E17;
    procedure ECSR;
    procedure EEIO;
    procedure EESR;
    procedure EICDM;
    procedure EIR;
    procedure EISR;
    procedure EOCDM;
    procedure EOSR;
    procedure ER;
    procedure ERIR;
    procedure ESR;
    procedure EXF;
    procedure EXF490;
    procedure EXF1230;
    procedure EXRN;
    procedure FA;
    procedure FAN;
    procedure FD;
    procedure FM;
    procedure FP;
    procedure FU;
    procedure INMON;
    procedure INMON490;
    procedure INN;
    procedure INN490;
    procedure J;
    procedure JACTI;
    procedure JACTI490;
    procedure JACTO;
    procedure JACTO490;
    procedure JBD;
    procedure JT;
    procedure LA;
    procedure LANQ;
    procedure LAQ;
    procedure LB;
    procedure LBPJB0;
    procedure LBPJB1;
    procedure LBPJB2;
    procedure LBPJB3;
    procedure LBPJB4;
    procedure LBPJB5;
    procedure LBPJB6;
    procedure LBPJB7;
    procedure LBW;
    procedure LLP;
    procedure LOG;
    procedure LPLR;
    procedure LQ;
    procedure LRSA;
    procedure LRSAQ;
    procedure LRSQ;
    procedure LSA;
    procedure LSAQ;
    procedure LSQ;
    procedure M;
    procedure MATE;
    procedure MATL;
    procedure NORM;
    procedure NOTT;
    procedure ORR;
    procedure OUTMON;
    procedure OUTMON490;
    procedure OUTMON1230;
    procedure OUT;
    procedure OUT490;
    procedure OUT1230;
    procedure R;
    procedure RA;
    procedure RALP;
    procedure RAN;
    procedure RANLP;
    procedure RANQ;
    procedure RAQ;
    procedure RD;
    procedure RI;
    procedure RLP;
    procedure RNOT;
    procedure ROR;
    procedure RSA;
    procedure RSAQ;
    procedure RSQ;
    procedure RSSU;
    procedure RXOR;
    procedure SA;
    procedure SAND;
    procedure SANQ;
    procedure SAQ;
    procedure SB;
    procedure SBW;
    procedure SC;
    procedure SC1230;
    procedure SCN;
    procedure SESR;
    procedure SFS;
    procedure SIFR;
    procedure SISR;
    procedure SLJ;
    procedure SLJT;
    procedure SOSR;
    procedure SQ;
    procedure SSR;
    procedure SSU;
    procedure TA;
    procedure TBI;
    procedure TERMIN;
    procedure TERMIN1230;
    procedure TERMIN490;
    procedure TERMOT;
    procedure TERMOT1230;
    procedure TERMOT490;
    procedure TLP;
    procedure TSET;
    procedure XORR;
    procedure SetInterruptLockout(const Value: Boolean);
    procedure SetInterruptActive(const Value: Boolean);
  public
    constructor Create(mem: T494Memory);
    procedure Clear;
    procedure Execute;
    procedure Fetch;
    procedure PreFetch(addr: T494Address);
    procedure Start;
    procedure Stop;
    property Channels: T494ChannelList read FChannels;
    property InterruptActive: Boolean read FInterruptActive write SetInterruptActive;
    property InterruptLockout: Boolean read FInterruptLockout write SetInterruptLockout;
    property Interrupts: T494InterruptQueue read FInterrupts;
    property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
    property OnLog: TLogEvent read FOnLog write FOnLog;
    property PanelSwitches: TPanelSwitches read FPanelSwitches write FPanelSwitches;
    property State: T494CpuState read FState;
  end;

implementation

{ T494Cpu }

uses FmtBcd, Bcd, U494Config;

procedure T494Cpu.A;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := FMemory.A + operand;
end;

procedure T494Cpu.AddQSkip;
// Skip processing for AQ and ANQ instructions
begin
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (not FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if (FMemory.Q.Value = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if (FMemory.Q.Value <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (not FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.ALP;
var
    op1, op2: T494Word;
begin
    op1 := StdFetch;
    op2.Value := (FMemory.Q.Value and op1.Value);
    FMemory.A := FMemory.A + op2;
end;

procedure T494Cpu.AQ;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.Q := FMemory.Q + operand;
end;

procedure T494Cpu.AQStore(value: T494Word);
var
    mem: T494Word;
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    case FMemory.Inst.k of
      0:
      begin
        FMemory.Q := value;
      end;
      1:
      begin
        mem := FMemory.Fetch(addr);
        mem.H2 := value.H2;
        FMemory.Store(addr, mem);
      end;
      2:
      begin
        mem := FMemory.Fetch(addr);
        mem.H1 := value.H2;
        FMemory.Store(addr, mem);
      end;
      3:
      begin
        FMemory.Store(addr, value);
      end;
      5:
      begin
        mem := FMemory.Fetch(addr);
        mem.H2 := not value.H2;
        FMemory.Store(addr, mem);
      end;
      6:
      begin
        mem := FMemory.Fetch(addr);
        mem.H1 := not value.H2;
        FMemory.Store(addr, mem);
      end;
      7:
      begin
        FMemory.Store(addr, not value);
      end;
    end;
end;

procedure T494Cpu.JBD;
var
    operand: T494Word;
    addr: UInt32;
    j: Byte;
begin
    operand := StdFetch;
    addr := operand.Value and BITS17;
    j := FMemory.Inst.j;
    if (j = 0) then
        Exit;
    if (FMemory.B[FMemory.IFR.f6, j].Value <> 0) then
    begin
        FMemory.B[FMemory.IFR.f6, j].Value := FMemory.B[FMemory.IFR.f6, j].Value - 1;
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
    end;
end;

procedure T494Cpu.TBI;
var
    operand, mem: T494Word;
    hw: T494HalfWord;
    j: Byte;
begin
    // Get value to test into operand
    j := FMemory.Inst.j;
    if (j = 0) then
        Exit;
    case FMemory.Inst.k of
      0:
      begin
        operand := 0;
        case gConfig.Mode of
          m494:
          begin
            if ((FMemory.IFR.f7 = 0) or (j <= 3)) then
            begin
                hw.Value := FMemory.Operand.Value15;
                operand.H2 := hw;
            end else
                operand.Value := FMemory.Operand.Value and BITS17;
          end;
          m490:
          begin
            hw.Value := FMemory.Operand.Value15;
            operand.H2 := hw;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
            begin
                hw.Value := FMemory.Operand.Value15;
                operand.H2 := hw;
            end else
                operand.Value := FMemory.Operand.Value and BITS17;
          end;
        end;
      end;
      1:
      begin
        operand.H2 := FMemory.Fetch(FMemory.Operand.Value).H2;
        operand.H1 := 0;
      end;
      2:
      begin
        operand.H2 := FMemory.Fetch(FMemory.Operand.Value).H1;
        operand.H1 := 0;
      end;
      3:
      begin
        mem := FMemory.Fetch(FMemory.Operand.Value);
        case gConfig.Mode of
          m494:
          begin
            if ((FMemory.IFR.f7 = 0) or (j <= 3)) then
                operand.H2 := mem.H2
            else
                operand.Value := mem.Value and BITS17;
          end;
          m490:
          begin
            operand.H2 := mem.H2
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                operand.H2 := mem.H2
            else
                operand.Value := mem.Value and BITS17;
          end;
        end;
      end;
      4:
      begin
        operand.H2.Value := FMemory.Operand.Value;
        if (operand.H2 < 0) then
        begin
            hw.Value := BITS15;
            operand.H1 := hw;
        end else
        begin
            hw.Value := 0;
            operand.H1 := hw;
        end;
      end;
      5:
      begin
        operand.H2 := FMemory.Fetch(FMemory.Operand.Value).H2;
        if (operand.H2 < 0) then
        begin
            hw.Value := BITS15;
            operand.H1 := hw;
        end else
        begin
            hw.Value := 0;
            operand.H1 := hw;
        end;
      end;
      6:
      begin
        operand.H2 := FMemory.Fetch(FMemory.Operand.Value).H1;
        if (operand.H2 < 0) then
        begin
            hw.Value := BITS15;
            operand.H1 := hw;
        end else
        begin
            hw.Value := 0;
            operand.H1 := hw;
        end;
      end;
      7:
      begin
        mem := FMemory.A;
        case gConfig.Mode of
          m494:
          begin
            if ((FMemory.IFR.f7 = 0) or (j <= 3)) then
                operand.H2 := mem.H2
            else
                operand.Value := mem.Value and BITS17;
          end;
          m490:
          begin
            operand.H2 := mem.H2;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                operand.H2 := mem.H2
            else
                operand.Value := mem.Value and BITS17;
          end;
        end;
      end;
    end;
    if (operand.Value <> FMemory.B[FMemory.IFR.f6, j].Value) then
    begin
        FMemory.B[FMemory.IFR.f6, j].Value := FMemory.B[FMemory.IFR.f6, j].Value + 1;
    end else
    begin
        FMemory.B[FMemory.IFR.f6, j].Value := 0;
        FMemory.P := FMemory.P + 1;
    end;
end;

procedure T494Cpu.TERMIN;
var
    chan: Byte;
begin
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    if (Assigned(FChannels[chan])) then
        FChannels[chan].TerminateInput;
end;

procedure T494Cpu.TERMIN1230;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    case FMemory.Inst.khat of
      0:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].TerminateInput;
      end;
      1:
      begin
        InterruptLockout := (FMemory.Inst.b <> 0);
      end;
      2:
      begin
        FExtIntLockout := (FMemory.Inst.b <> 0);
      end;
      3:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExtIntLockout := (FMemory.Inst.b <> 0);
      end
    end;
end;

procedure T494Cpu.TERMIN490;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    if (Assigned(FChannels[chan])) then
        FChannels[chan].TerminateInput;
end;

procedure T494Cpu.TERMOT;
var
    chan: Byte;
begin
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    if (Assigned(FChannels[chan])) then
        FChannels[chan].TerminateOutput;
end;

procedure T494Cpu.TERMOT1230;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    case FMemory.Inst.khat of
      0:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].TerminateOutput;
      end;
      1:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].TerminateExternal;
      end;
      2:
      begin
        if (Assigned(FChannels[chan])) then
        begin
            FChannels[chan].TerminateExternal;
            FChannels[chan].TerminateOutput;
        end;
      end
      else
      begin
        raise Exception.CreateFmt('TERMOT1230 k designator %d not implemented', [FMemory.Inst.khat]);
      end;
    end;
end;

procedure T494Cpu.TERMOT490;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    if (Assigned(FChannels[chan])) then
        FChannels[chan].TerminateOutput;
end;

procedure T494Cpu.Clear;
var
    i: Integer;
begin
    InterruptLockout := False;
    for i := 0 to FChannels.Count - 1 do
    begin
        if (Assigned(FChannels[i])) then
            FChannels[i].Clear;
    end;
end;

procedure T494Cpu.CPL;
var
    addr: T494Address;
    a: UInt32;
begin
    addr := FMemory.Operand;
    a := ((FMemory.Fetch(addr.Value).Value and $3f) shl 24) or
         ((FMemory.Fetch(addr.Value + 1).Value and $3f) shl 18) or
         ((FMemory.Fetch(addr.Value + 2).Value and $3f) shl 12) or
         ((FMemory.Fetch(addr.Value + 3).Value and $3f) shl 6) or
         (FMemory.Fetch(addr.Value + 4).Value and $3f);
    FMemory.A.Value := a;
end;

procedure T494Cpu.CPU;
var
    addr: T494Address;
    a: UInt32;
begin
    addr := FMemory.Operand;
    a := ((FMemory.Fetch(addr.Value).Value and $1f8000) shl 9) or
         ((FMemory.Fetch(addr.Value + 1).Value and $1f8000) shl 3) or
         ((FMemory.Fetch(addr.Value + 2).Value and $1f8000) shr 3) or
         ((FMemory.Fetch(addr.Value + 3).Value and $1f8000) shr 9) or
         ((FMemory.Fetch(addr.Value + 4).Value and $1f8000) shr 15);
    FMemory.A.Value := a;
end;

procedure T494Cpu.TA;
var
    operand: T494Word;
begin
    operand := StdFetch;
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (operand <= FMemory.Q) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (operand > FMemory.Q) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if ((FMemory.A < operand) and (operand <= FMemory.Q)) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if ((operand > FMemory.Q) or (operand <= FMemory.A)) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (operand <= FMemory.A) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (operand > FMemory.A) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.TLP;
var
    operand, lp: T494Word;
    test: Integer;
begin
    operand := StdFetch;
    lp.Value := operand.Value and FMemory.Q.Value;
    test := FMemory.A - lp;
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (not FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if (test = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if (test <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (test >= 0) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (test < 0) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.TSET;
var
    test: T494Word;
    hw: T494HalfWord;
begin
    test := FMemory.Fetch(FMemory.Operand.Value);
    if ((test.Value and BIT14) = 0) then
    begin
        hw.Value := BITS15;
        test.H2 := hw;
    end else
    begin
        FInterruptPending := True;
        FInterruptVector := ITestAndSet;
    end;
end;

constructor T494Cpu.Create(mem: T494Memory);
var
    i: Integer;
begin
    inherited Create;
    FInterrupts := T494InterruptQueue.Create;
    FChannels := T494ChannelList.Create;
    FMemory := mem;
    Include(FState, csHalted);
    // Initialize all opcodes to 'not implemented'. This will be
    // overridden as each instruction is implemented.
    for i := 0 to 63 do
    begin
        FStdInstProcs[i] := NotImplemented;
        FExtInstProcs[i] := NotImplemented;
    end;
    // Override proc addresses for implemented instructions
    //
    // Standard instructions
    FStdInstProcs[1] := RSQ;
    FStdInstProcs[2] := RSA;
    FStdInstProcs[3] := RSAQ;
    FStdInstProcs[4] := TA;
    FStdInstProcs[5] := LSQ;
    FStdInstProcs[6] := LSA;
    FStdInstProcs[7] := LSAQ;
    FStdInstProcs[8] := LQ;
    FStdInstProcs[9] := LA;
    FStdInstProcs[10] := LB;
    FStdInstProcs[12] := SQ;
    FStdInstProcs[13] := SA;
    FStdInstProcs[14] := SB;
    FStdInstProcs[16] := A;
    FStdInstProcs[17] := AN;
    FStdInstProcs[18] := M;
    FStdInstProcs[19] := D;
    FStdInstProcs[20] := RA;
    FStdInstProcs[21] := RAN;
    FStdInstProcs[22] := AQ;
    FStdInstProcs[23] := ANQ;
    FStdInstProcs[24] := LAQ;
    FStdInstProcs[25] := LANQ;
    FStdInstProcs[26] := SAQ;
    FStdInstProcs[27] := SANQ;
    FStdInstProcs[28] := RAQ;
    FStdInstProcs[29] := RANQ;
    FStdInstProcs[30] := RI;
    FStdInstProcs[31] := RD;
    FStdInstProcs[32] := LLP;
    FStdInstProcs[33] := ALP;
    FStdInstProcs[34] := ANLP;
    FStdInstProcs[35] := TLP;
    FStdInstProcs[36] := RLP;
    FStdInstProcs[37] := RALP;
    FStdInstProcs[38] := RANLP;
    FStdInstProcs[39] := SAND;
    FStdInstProcs[40] := ORR;
    FStdInstProcs[41] := XORR;
    FStdInstProcs[42] := NOTT;
    FStdInstProcs[43] := SSU;
    FStdInstProcs[44] := ROR;
    FStdInstProcs[45] := RXOR;
    FStdInstProcs[46] := RNOT;
    FStdInstProcs[47] := RSSU;
    FStdInstProcs[48] := JT;
    FStdInstProcs[49] := J;
    FStdInstProcs[52] := SLJT;
    FStdInstProcs[53] := SLJ;
    FStdInstProcs[56] := R;
    FStdInstProcs[57] := TBI;
    FStdInstProcs[58] := JBD;
    case gConfig.Mode of
      m494:
      begin
        FStdInstProcs[11] := EXF;
        FStdInstProcs[15] := SC;
        FStdInstProcs[50] := JACTI;
        FStdInstProcs[51] := JACTO;
        FStdInstProcs[54] := TERMIN;
        FStdInstProcs[55] := TERMOT;
        FStdInstProcs[59] := INN;
        FStdInstProcs[60] := OUT;
        FStdInstProcs[61] := INMON;
        FStdInstProcs[62] := OUTMON;
      end;
      m490:
      begin
        FStdInstProcs[11] := EXF490;
        FStdInstProcs[15] := SC;
        FStdInstProcs[50] := JACTI490;
        FStdInstProcs[51] := JACTO490;
        FStdInstProcs[54] := TERMIN490;
        FStdInstProcs[55] := TERMOT490;
        FStdInstProcs[59] := INN490;
        FStdInstProcs[60] := OUT490;
        FStdInstProcs[61] := INMON490;
        FStdInstProcs[62] := OUTMON490;
      end;
      m1230:
      begin
        FStdInstProcs[11] := EXF1230;
        FStdInstProcs[15] := SC1230;
        FStdInstProcs[50] := JACTI490;
        FStdInstProcs[51] := JACTO490;
        FStdInstProcs[54] := TERMIN1230;
        FStdInstProcs[55] := TERMOT1230;
        FStdInstProcs[59] := INN490;
        FStdInstProcs[60] := OUT1230;
        FStdInstProcs[61] := INMON490;
        FStdInstProcs[62] := OUTMON1230;
      end;
    end;
    // Extended instructions
    case gConfig.Mode of
      m494:
      begin
        FExtInstProcs[1] := FA;
        FExtInstProcs[2] := FAN;
        FExtInstProcs[3] := FM;
        FExtInstProcs[5] := FD;
        FExtInstProcs[6] := FP;
        FExtInstProcs[7] := FU;
        FExtInstProcs[8] := DT;
        FExtInstProcs[9] := DA;
        FExtInstProcs[10] := DAN;
        FExtInstProcs[11] := DTE;
        FExtInstProcs[12] := DN;
        FExtInstProcs[13] := DAC;
        FExtInstProcs[14] := DANB;
        FExtInstProcs[15] := DTL;
        FExtInstProcs[17] := DPL;
        FExtInstProcs[18] := DPA;
        FExtInstProcs[19] := DPTE;
        FExtInstProcs[20] := DPN;
        FExtInstProcs[21] := DPS;
        FExtInstProcs[22] := DPAN;
        FExtInstProcs[23] := DPTL;
        FExtInstProcs[24] := SFS;
        FExtInstProcs[25] := CPL;
        FExtInstProcs[26] := CPU;
        FExtInstProcs[27] := DCL;
        FExtInstProcs[28] := DCU;
        FExtInstProcs[29] := CUL;
        FExtInstProcs[30] := CUU;
        FExtInstProcs[31] := ER;
        FExtInstProcs[32] := LBPJB0;
        FExtInstProcs[33] := LBPJB1;
        FExtInstProcs[34] := LBPJB2;
        FExtInstProcs[35] := LBPJB3;
        FExtInstProcs[36] := LBPJB4;
        FExtInstProcs[37] := LBPJB5;
        FExtInstProcs[38] := LBPJB6;
        FExtInstProcs[39] := LBPJB7;
        FExtInstProcs[41] := LRSQ;
        FExtInstProcs[42] := TSET;
        FExtInstProcs[43] := MATE;
        FExtInstProcs[44] := EXRN;
        FExtInstProcs[45] := LRSA;
        FExtInstProcs[46] := LRSAQ;
        FExtInstProcs[47] := MATL;
        FExtInstProcs[49] := EIR;
        FExtInstProcs[50] := LPLR;
        FExtInstProcs[53] := SIFR;
        FExtInstProcs[54] := ERIR;
        FExtInstProcs[57] := LBW;
        FExtInstProcs[58] := SCN;
        FExtInstProcs[59] := ECSR;
        FExtInstProcs[61] := SBW;
        FExtInstProcs[63] := LOG;
      end;
      m1230:
      begin
        FExtInstProcs[7] := NORM;
        FExtInstProcs[32] := EEIO;
        FExtInstProcs[48] := ESR;
        FExtInstProcs[49] := EISR;
        FExtInstProcs[50] := EOSR;
        FExtInstProcs[51] := EESR;
        FExtInstProcs[52] := EICDM;
        FExtInstProcs[53] := EOCDM;
        FExtInstProcs[54] := DICDM;
        FExtInstProcs[55] := DOCDM;
        FExtInstProcs[56] := SSR;
        FExtInstProcs[57] := SISR;
        FExtInstProcs[58] := SOSR;
        FExtInstProcs[59] := SESR;
        FExtInstProcs[60] := D17;
        FExtInstProcs[61] := E17;
      end;
    end;
end;

procedure T494Cpu.CUL;
var
    addr: T494Address;
    word: T494Word;
    a: UInt32;
begin
    addr := FMemory.Operand;
    a := FMemory.A.Value;
    word := FMemory.Fetch(addr.Value);
    word.H2 := (a shr 24) and $3f;
    FMemory.Store(addr.Value, word);
    word := FMemory.Fetch(addr.Value + 1);
    word.H2 := (a shr 18) and $3f;
    FMemory.Store(addr.Value + 1, word);
    word := FMemory.Fetch(addr.Value + 2);
    word.H2 := (a shr 12) and $3f;
    FMemory.Store(addr.Value + 2, word);
    word := FMemory.Fetch(addr.Value + 3);
    word.H2 := (a shr 6) and $3f;
    FMemory.Store(addr.Value + 3, word);
    word := FMemory.Fetch(addr.Value + 4);
    word.H2 := a and $3f;
    FMemory.Store(addr.Value + 4, word);
end;

procedure T494Cpu.CUU;
var
    addr: T494Address;
    word: T494Word;
    a: UInt32;
begin
    addr := FMemory.Operand;
    a := FMemory.A.Value;
    word := FMemory.Fetch(addr.Value);
    word.H1 := (a shr 24) and $3f;
    FMemory.Store(addr.Value, word);
    word := FMemory.Fetch(addr.Value + 1);
    word.H1 := (a shr 18) and $3f;
    FMemory.Store(addr.Value + 1, word);
    word := FMemory.Fetch(addr.Value + 2);
    word.H1 := (a shr 12) and $3f;
    FMemory.Store(addr.Value + 2, word);
    word := FMemory.Fetch(addr.Value + 3);
    word.H1 := (a shr 6) and $3f;
    FMemory.Store(addr.Value + 3, word);
    word := FMemory.Fetch(addr.Value + 4);
    word.H1 := a and $3f;
    FMemory.Store(addr.Value + 4, word);
end;

procedure T494Cpu.D;
var
    aq, quotient: Int64;
    ovfl, divByZero: Boolean;
    operand: T494Word;
begin
    operand := StdFetch;
    aq := (Int64(FMemory.A.Value) shl 30) or FMemory.Q.Value;
    // Extend the sign
    if ((aq and $800000000000000) <> 0) then
        aq := aq or $f000000000000000;
    // Make 2s complement
    if (aq < 0) then
        aq := aq + 1;
    if ((gConfig.Mode = m1230) and (FMemory.Inst.k = 7)) then
    begin
        // 1230 square root function
        if (FMemory.Q.Value >= 0) then
        begin
            aq := FMemory.Q.Value;
            FMemory.Q.Value := Trunc(Sqrt(aq));
            FMemory.A.Value := aq - (FMemory.Q.Value * FMemory.Q.Value);
        end else
        begin
            FMemory.Q.Value := 0;
            FMemory.A.Value := 0;
        end;
        case FMemory.Inst.j of
          1:
          begin
            FMemory.P := FMemory.P + 1;
          end;
          2:
          begin
              if (FMemory.A.Value <> 0) then
                FMemory.P := FMemory.P + 1;
          end;
          3:
          begin
              if (FMemory.A.Value = 0) then
                FMemory.P := FMemory.P + 1;
          end;
        end;
    end else
    begin
        // normal division
        if (Integer(operand) = 0) then
        begin
            divByZero := True;
            ovfl := False;
            if (aq >= 0) then
            begin
                FMemory.Q.Value := BITS30;
                FMemory.A := FMemory.Q;
            end else
            begin
                FMemory.Q := 0;
                FMemory.A := FMemory.Q;
            end;
        end else
        begin
            divByZero := False;
            quotient := aq div Integer(operand);
            FMemory.Q := quotient;
            FMemory.A := Integer(aq mod Integer(operand));
            if (quotient <> 0) then
                ovfl := Abs(aq div quotient) > $1fffffff
            else
                ovfl := False;
        end;
        // Divide skip conditions
        case FMemory.Inst.j of
          1:
          begin
            FMemory.P := FMemory.P + 1;
          end;
          2:
          begin
            if (not ovfl) then
                FMemory.P := FMemory.P + 1;
          end;
          3:
          begin
            if (ovfl) then
                FMemory.P := FMemory.P + 1;
          end;
          4:
          begin
            if (divByZero and (aq < 0)) then
            begin
                if (((not FMemory.A.Value) and BITS30) = 0) then
                    FMemory.P := FMemory.P + 1;
            end else
            begin
                if (FMemory.A.Value = 0) then
                    FMemory.P := FMemory.P + 1;
            end;
          end;
          5:
          begin
            if (divByZero and (aq < 0)) then
            begin
                if (((not FMemory.A.Value) and BITS30) <> 0) then
                    FMemory.P := FMemory.P + 1;
            end else
            begin
                if (FMemory.A.Value <> 0) then
                    FMemory.P := FMemory.P + 1;
            end;
          end;
          { TODO :
    I'm not sure if my interpretation of the j values 6 and 7. Check
    with assembler manual when I get it. }
          6:
          begin
            if (FMemory.A.Value = 0) then
                FMemory.P := FMemory.P + 1;
          end;
          7:
          begin
            if ((FMemory.A.Value and BIT29) <> 0) then
                FMemory.P := FMemory.P + 1;
          end;
        end;
    end;
end;

procedure T494Cpu.D17;
// We use the IFR f7 field to hold the15 / 17 bit mode for the 1230
// because this is essentially the same purpose as the 494 uses it for.
begin
    FMemory.IFR.Value := FMemory.IFR.Value and (not $4000000);
end;

procedure T494Cpu.DA;
var
    addr: UInt32;
    op1, op2: TBcd;
    sameSigns: Boolean;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (((op1 >= 0) and (op2 >= 0)) or ((op1 < 0) and (op2 < 0))) then
        sameSigns := True
    else
        sameSigns := False;
    op1 := op1 + op2;
    FMemory.StoreBcdAQ(op1);
    if (sameSigns and (op1.Precision > 10)) then
        FMemory.IFR.f5 := 1
    else
        FMemory.IFR.f5 := 0;
end;

procedure T494Cpu.DAC;
var
    addr: UInt32;
    op1, op2: TBcd;
    sameSigns: Boolean;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (((op1 >= 0) and (op2 >= 0)) or ((op1 < 0) and (op2 < 0))) then
        sameSigns := True
    else
        sameSigns := False;
    op1 := op1 + op2 + FMemory.IFR.f5;
    FMemory.StoreBcdAQ(op1);
    if (sameSigns and (op1.Precision > 10)) then
        FMemory.IFR.f5 := 1
    else
        FMemory.IFR.f5 := 0;
end;

procedure T494Cpu.DAN;
var
    addr: UInt32;
    op1, op2: TBcd;
    sameSigns: Boolean;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (((op1 >= 0) and (op2 >= 0)) or ((op1 < 0) and (op2 < 0))) then
        sameSigns := True
    else
        sameSigns := False;
    op1 := op1 - op2;
    FMemory.StoreBcdAQ(op1);
    if ((not sameSigns) and (op1.Precision > 10)) then
        FMemory.IFR.f4 := 1
    else
        FMemory.IFR.f4 := 0;
end;

procedure T494Cpu.DANB;
var
    addr: UInt32;
    op1, op2: TBcd;
    sameSigns: Boolean;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (((op1 >= 0) and (op2 >= 0)) or ((op1 < 0) and (op2 < 0))) then
        sameSigns := True
    else
        sameSigns := False;
    op1 := op1 - op2;
    if (sameSigns) then
        op1 := op1 + FMemory.IFR.f4;
    FMemory.StoreBcdAQ(op1);
    if ((not sameSigns) and (op1.Precision > 10)) then
        FMemory.IFR.f4 := 1
    else
        FMemory.IFR.f4 := 0;
end;

procedure T494Cpu.DCL;
var
    addr: UInt32;
    aq: UInt64;
    i: Integer;
begin
    addr := FMemory.Operand.Value;
    aq := FMemory.FetchAQ;
    for i := 1 to 5 do
    begin
        aq := (aq * 10) + (FMemory.Fetch(addr).Value and $f);
        Inc(addr);
    end;
    FMemory.StoreAQ(aq);
end;

procedure T494Cpu.DCU;
var
    addr: UInt32;
    aq: UInt64;
    i: Integer;
begin
    addr := FMemory.Operand.Value;
    aq := FMemory.FetchAQ;
    for i := 1 to 5 do
    begin
        aq := (aq * 10) + ((FMemory.Fetch(addr).Value shr 15) and $f);
        Inc(addr);
    end;
    FMemory.StoreAQ(aq);
end;

procedure T494Cpu.DICDM;
begin
    NotImplemented;
end;

procedure T494Cpu.DN;
var
    op1, op2: TBcd;
begin
    op1 := FMemory.FetchBcdAQ;
    if (op1 < 0) then
        op1 := -op1;
    op2 := 9999999999;
    op1 := op2 - op1;                       // nines complement
    if ((FMemory.Operand.Value and $1) = 0) then
        op1 := op1 + 1;                     // tens complement
    FMemory.StoreBcdAQ(op1);
end;

procedure T494Cpu.DOCDM;
begin
    NotImplemented;
end;

procedure T494Cpu.E17;
// We use the IFR f7 field to hold the15 / 17 bit mode for the 1230
// because this is essentially the same purpose as the 494 uses it for.
begin
    FMemory.IFR.Value := FMemory.IFR.Value or $4000000;
end;

procedure T494Cpu.ECSR;
begin
    if (FInterruptActive) then
        FMemory.IASR.Value := FMemory.Fetch(FMemory.Operand.Value).Value
    else
        FMemory.CSR.Value := FMemory.Fetch(FMemory.Operand.Value).Value;
end;

procedure T494Cpu.EEIO;
// Enabled / disable external I/O (whatever that means).
// khat = 0 disable, khat = 1 enable
begin
    NotImplemented;
end;

procedure T494Cpu.EESR;
begin
    NotImplemented;
end;

procedure T494Cpu.EICDM;
begin
    NotImplemented;
end;

procedure T494Cpu.EIR;
var
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    FMemory.RIR.Value := FMemory.Fetch(addr + 1).Value;
    FMemory.IFR.Value := FMemory.Fetch(addr).Value;
    FMemory.IFR.SetDelay(1);
end;

procedure T494Cpu.EISR;
begin
    NotImplemented;
end;

procedure T494Cpu.EOCDM;
begin
    NotImplemented;
end;

procedure T494Cpu.EOSR;
begin
    NotImplemented;
end;

procedure T494Cpu.ER;
begin
    FExecRemotePending := True;
    FExecRemoteAddr := FMemory.Operand.Value;
end;

procedure T494Cpu.ERIR;
begin
    FMemory.RIR.Value := FMemory.Fetch(FMemory.Operand.Value).Value;
end;

procedure T494Cpu.ESR;
var
    r: Byte;
begin
    r := FMemory.Inst.j77;
    if (r > 2) then
        raise Exception.CreateFmt('Illegal SR (%d)', [r]);
    FMemory.SR[r].Value := FMemory.Inst.y.Value15 and $f;
end;

procedure T494Cpu.LA;
begin
    FMemory.A := StdFetch;
end;

procedure T494Cpu.DPA;
var
    addr: UInt32;
    op1, op2: T494DWord;
begin
    addr := FMemory.Operand.Value;
    op1.Value := (Int64(FMemory.A.Value) shl 30) or (FMemory.Q.Value);
    op2 := FMemory.FetchDWord(addr);
    op1 := op1 + op2;
    FMemory.A.Value := op1.Value shr 30;
    FMemory.Q.Value := op1.Value;
end;

procedure T494Cpu.DPAN;
var
    addr: UInt32;
    op1, op2: T494DWord;
begin
    addr := FMemory.Operand.Value;
    op1.Value := (Int64(FMemory.A.Value) shl 30) or (FMemory.Q.Value);
    op2 := FMemory.FetchDWord(addr);
    op1 := op1 - op2;
    FMemory.A.Value := op1.Value shr 30;
    FMemory.Q.Value := op1.Value;
end;

procedure T494Cpu.DPL;
begin
    FMemory.A := FMemory.Fetch(FMemory.Operand.Value);
    FMemory.Q := FMemory.Fetch(FMemory.Operand.Value + 1);
end;

procedure T494Cpu.DPN;
var
    op1: T494DWord;
begin
    op1.Value := (Int64(FMemory.A.Value) shl 30) or (FMemory.Q.Value);
    op1.Value := not op1.Value;
    FMemory.A.Value := op1.Value shr 30;
    FMemory.Q.Value := op1.Value;
end;

procedure T494Cpu.LB;
var
    b1, b2: Byte;

    procedure ExtendSign;
    begin
        if ((FMemory.B[b1, b2].Value and $4fff) = 0) then
            FMemory.B[b1, b2].Value := FMemory.B[b1, b2].Value and (not $18000)
        else
            FMemory.B[b1, b2].Value := FMemory.B[b1, b2].Value or $18000;
    end;

    procedure LB494;
    begin
        if ((b2 >= 1) and (b2 <= 3)) then
        begin
            case FMemory.Inst.k of
              0,
              4:
              begin
                FMemory.B[b1, b2].Value := FMemory.Operand.Value15;
              end;
              1,
              3,
              5:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H2.Value and BITS15;
              end;
              2,
              6:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H1.Value and BITS15;
              end;
              7:
              begin
                FMemory.B[b1, b2].Value := FMemory.A.H2.Value and BITS15;
              end;
            end;
        end else
        begin
            case FMemory.Inst.k of
              0:
              begin
                FMemory.B[b1, b2].Value := FMemory.Operand.Value;
              end;
              1:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H2.Value;
              end;
              2:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H1.Value;
              end;
              3:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).Value and BITS17;
              end;
              4:
              begin
                FMemory.B[b1, b2].Value := FMemory.Operand.Value15;
                ExtendSign;
              end;
              5:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H2.Value;
                ExtendSign;
              end;
              6:
              begin
                FMemory.B[b1, b2].Value := FMemory.Fetch(FMemory.Operand.Value).H1.Value;
                ExtendSign;
              end;
              7:
              begin
                FMemory.B[b1, b2].Value := FMemory.A.Value and BITS17;
              end;
            end;
        end;
    end;

begin
    b1 := FMemory.IFR.f6;
    b2 := FMemory.Inst.j;
    if (b2 = 0) then
        Exit;

    case gConfig.Mode of
      m494:     LB494;
      m490:     FMemory.B[b1, b2].Value := StdFetch;
      m1230:    FMemory.B[b1, b2].Value := StdFetch;
    end;
end;

procedure T494Cpu.LBPJB0;
begin
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB1;
begin
    FMemory.B[FMemory.IFR.f6, 1].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB2;
begin
    FMemory.B[FMemory.IFR.f6, 2].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB3;
begin
    FMemory.B[FMemory.IFR.f6, 3].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB4;
begin
    FMemory.B[FMemory.IFR.f6, 4].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB5;
begin
    FMemory.B[FMemory.IFR.f6, 5].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB6;
begin
    FMemory.B[FMemory.IFR.f6, 6].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBPJB7;
begin
    FMemory.B[FMemory.IFR.f6, 7].Value := FMemory.P.Value - FMemory.RIR.Value;
    FMemory.P := FMemory.Operand.Value;
end;

procedure T494Cpu.LBW;
var
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    FMemory.B[1, 1].Value := FMemory.Fetch(addr).H2.Value;
    FMemory.B[1, 2].Value := FMemory.Fetch(addr + 1).H2.Value;
    FMemory.B[1, 3].Value := FMemory.Fetch(addr + 2).H2.Value;
    FMemory.B[1, 4].Value := FMemory.Fetch(addr + 3).Value;
    FMemory.B[1, 5].Value := FMemory.Fetch(addr + 4).Value;
    FMemory.B[1, 6].Value := FMemory.Fetch(addr + 5).Value;
    FMemory.B[1, 7].Value := FMemory.Fetch(addr + 6).Value;
end;

procedure T494Cpu.LLP;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A.Value := FMemory.Q.Value and operand.Value;
end;

procedure T494Cpu.LOG;
// This is an instruction that exists only in the emulator. It allows
// a program to write a message to the virtual maintenance panel.
// The first word of the operand is the length to the text in words.
begin
    if (Assigned(FOnLog)) then
        FOnLog(Self, FMemory.Operand.Value);
end;

procedure T494Cpu.LQ;
begin
    FMemory.Q := StdFetch;
end;

procedure T494Cpu.LANQ;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := operand - FMemory.Q;
end;

procedure T494Cpu.LAQ;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := FMemory.Q + operand;
end;

procedure T494Cpu.Execute;
var
    b: Byte;
    bval, b7: UInt32;
    holdp: T494Address;
begin
    // If an interrupt is pending, do not execute the current instruction.
    if (FInterruptPending) then
        Exit;
    //
    if (Assigned(FOnDebug)) then
        FOnDebug(Self, nil);
    try
        // If not a repeated instruction, increment P now.
        if ((not FPLockedOut) and (FMemory.IFR.f9 = 0)) then
            FMemory.P := FMemory.P + 1;
        FPLockedOut := False;
        holdp := FMemory.P;

        // Cache b7 for later use in the loop control code for repeated instructions.
        // This is necessary because 642 and 1230 code can modify the value of the b
        // registers by storing values into memory. So, it is possible for a repeated
        // instruction to modify b7 causing mayhem.
        b7 := FMemory.B[FMemory.IFR.f6, 7].Value;
        FCurInstProc;
        case FCurOpcode.JInterpret of
            jiNormal:   NormalSkip;
            jiPlus1:    Plus1Skip;
            jiLP:       LogicalProductSkip;
            jiAddQ:     AddQSkip;
        end;
        FMemory.IFR.DecDelay;
        // If the most recent instruction was an ExecuteRemote, then we
        // need to decrement P here because the executed instruction will
        // cause it to be incremented again.
        if (FExecRemotePending) then
            FMemory.P := FMemory.P - 1;
        if ((not FRepeatDelay) and (FMemory.IFR.f9 = 1)) then
        begin
            if (FCurOpcode.Opcode = 56) then
            begin
                // First pass for repeated instruction. Look up initial value
                // of effective operand.
                PreFetch(FMemory.P);
                b := FMemory.Inst.b;
                // Add index register (b) to address (y)
                FMemory.Inst.ybar := FMemory.Inst.y;
                if (b <> 0) then
                begin
                    bval := FMemory.B[FMemory.IFR.f6, b].Value;
                    case gConfig.Mode of
                      m494:
                      begin
                        FMemory.Inst.ybar := FMemory.Inst.y + bval;
                        if (FMemory.IFR.f7 = 0) then
                            FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
                      end;
                      m490:
                      begin
                        FMemory.Inst.ybar := FMemory.Inst.y + bval;
                        FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
                      end;
                      m1230:
                      begin
                        if (FMemory.IFR.f7 = 0) then
                        begin
                            FMemory.Inst.ybar := FMemory.Inst.y + bval;
                            FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
                        end else
                        begin
                            if (FMemory.Inst.s = 3) then
                                FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                                     (FMemory.P.Value and (not BITS13))
                            else
                                FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                                     (FMemory.SR[FMemory.Inst.s].Value shl 13);
                        end;
                      end;
                    end;
                end;
                FMemory.IFR.f1 := FMemory.Inst.ybar;
            end else
            begin
                // Second and sebsequent passes of repeated instructions.
                // Adjust effective operand as required.
                FMemory.B[FMemory.IFR.f6, 7].Value := b7 - 1;
                case FMemory.IFR.f2 of
                  1:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 + 1;
                  end;
                  2:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 - 1;
                  end;
                  3:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 + FMemory.B[FMemory.IFR.f6, FMemory.Inst.b]^;
                  end;
                  4:
                  begin
                    FMemory.Store(FMemory.Operand.Value + FMemory.B[Fmemory.IFR.f6, 6].Value, FMemory.IFR.f1.Value);
                  end;
                  5:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 + 1;
                    FMemory.Store(FMemory.Operand.Value + FMemory.B[Fmemory.IFR.f6, 6].Value, FMemory.IFR.f1.Value);
                  end;
                  6:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 - 1;
                    FMemory.Store(FMemory.Operand.Value + FMemory.B[Fmemory.IFR.f6, 6].Value, FMemory.IFR.f1.Value);
                  end;
                  7:
                  begin
                    FMemory.IFR.f1 := FMemory.IFR.f1 + FMemory.B[FMemory.IFR.f6, FMemory.Inst.b]^;
                    FMemory.Store(FMemory.Operand.Value + FMemory.B[Fmemory.IFR.f6, 6].Value, FMemory.IFR.f1.Value);
                  end;
                end;
                //
                if ((holdp + 1) = FMemory.P) then
                begin
                    // If a skip occurred, bump past instruction to be skipped
                    // and terminate the repeat sequence
                    FMemory.P := FMemory.P + 1;
                    FMemory.IFR.f9 := 0;
                end else
                begin
                    // If the loop count is exhausted, terminate the repeat sequence
                    // and skip to instruction following repeated instruction.
                    if (FMemory.B[FMemory.IFR.f6, 7].Value = 0) then
                    begin
                        FMemory.P := FMemory.P + 1;
                        FMemory.IFR.f9 := 0;
                    end;
                end;
            end;
        end;
        if (not FInterruptActive) then
            FRepeatDelay := False;
    except
      on E: EIllegalInstruction do
      begin
          FInterruptVector := IIllegalInstruction;
          FInterruptPending := True;
      end;
      on E: EProgramProtection do
      begin
          FInterruptVector := IProgramProtection;
          FInterruptPending := True;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
end;

procedure T494Cpu.EXF;
var
    chan: Byte;
begin
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
   case FMemory.Inst.khat of
      0,
      1,
      2:    Exit;
      3:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(FMemory.Fetch(FMemory.Operand.Value));
      end;
    end;
end;

procedure T494Cpu.EXF1230;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    case FMemory.Inst.khat of
      0:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunctionWithMonitor(FMemory.Fetch(FMemory.Operand.Value));
      end;
      1:
      begin
        // This is supposed to be the "with force" option. But I don't think I care about
        // forcing the external command to proceed.
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunctionWithMonitor(FMemory.Fetch(FMemory.Operand.Value));
      end;
      2:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(FMemory.Fetch(FMemory.Operand.Value));
      end;
      3:
      begin
        // This is supposed to be the "with force" option. But I don't think I care about
        // forcing the external command to proceed.
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(FMemory.Fetch(FMemory.Operand.Value));
      end;
    end;
end;

procedure T494Cpu.EXF490;
var
    chan: Byte;
begin
    chan := FMemory.Inst.jhat;
    case FMemory.Inst.khat of
      0,
      1,
      2:    Exit;
      3:
      begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(FMemory.Fetch(FMemory.Operand.Value));
      end;
    end;
end;

procedure T494Cpu.EXRN;
begin
    FInterruptPending := True;
    FInterruptVector := IExecutiveReturn;
end;

procedure T494Cpu.FA;
var
    addr: UInt32;
    w1, w2: UInt32;
    op1, op2: Double;
begin
    addr := FMemory.Operand.Value;
    w1 := FMemory.A.Value;
    w2 := FMemory.Q.Value;
    op1 := NativeToFloat(w1, w2);
    w1 := FMemory.Fetch(addr);
    w2 := FMemory.Fetch(addr + 1);
    op2 := NativeToFloat(w1, w2);
    try
        op1 := op1 + op2;
    except
      on E: EOverflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFOverflow;
      end;
      on E: EUnderflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFUnderflow;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
    FloatToNative(op1, w1, w2);
    FMemory.A.Value := w1;
    FMemory.Q.Value := w2;
end;

procedure T494Cpu.FAN;
var
    addr: UInt32;
    w1, w2: UInt32;
    op1, op2: Double;
begin
    addr := FMemory.Operand.Value;
    w1 := FMemory.A.Value;
    w2 := FMemory.Q.Value;
    op1 := NativeToFloat(w1, w2);
    w1 := FMemory.Fetch(addr);
    w2 := FMemory.Fetch(addr + 1);
    op2 := NativeToFloat(w1, w2);
    try
        op1 := op1 - op2;
    except
      on E: EOverflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFOverflow;
      end;
      on E: EUnderflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFUnderflow;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
    FloatToNative(op1, w1, w2);
    FMemory.A.Value := w1;
    FMemory.Q.Value := w2;
end;

procedure T494Cpu.FD;
var
    addr: UInt32;
    w1, w2: UInt32;
    op1, op2: Double;
begin
    addr := FMemory.Operand.Value;
    w1 := FMemory.A.Value;
    w2 := FMemory.Q.Value;
    op1 := NativeToFloat(w1, w2);
    w1 := FMemory.Fetch(addr);
    w2 := FMemory.Fetch(addr + 1);
    op2 := NativeToFloat(w1, w2);
    try
        op1 := op1 / op2;
    except
      on E: EZeroDivide do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFOverflow;
      end;
      on E: EOverflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFOverflow;
      end;
      on E: EUnderflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFUnderflow;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
    FloatToNative(op1, w1, w2);
    FMemory.A.Value := w1;
    FMemory.Q.Value := w2;
end;

procedure T494Cpu.Fetch;
var
    op, g, b, k: Byte;
    bval: Integer;
    ri, interrupt: T494Address;
    int: T494Interrupt;
begin
    { TODO :
Need something here to suppress memory limit checks if the P
register was incremented normally and not changed via a jump
instruction. }
    try
        if (FExecRemotePending) then
        begin
            PreFetch(FExecRemoteAddr);
            FExecRemotePending := False;
        end else if (FInterruptPending) then
        begin
            // We need to fire an unconditional interrupt
            InterruptLockout := True;
            FPLockedOut := True;
            InterruptActive := True;
            PreFetch(FInterruptVector);
        end else if ((not FInterruptLockout) and (FInterrupts.Count <> 0)) then
        begin
            // We need to fire a conditional interrupt.
            int := FInterrupts.Dequeue;
            InterruptLockout := True;
            FPLockedOut := True;
            InterruptActive := True;
            if (int.IType = intIO) then
            begin
                FMemory.IASR.Value := int.Channel;
                FMemory.IoStatus.Value := int.Status;
            end;
            interrupt.Value := int.Vector;
            PreFetch(interrupt);
        end else
        begin
            PreFetch(FMemory.P);
        end;
        b := FMemory.Inst.b;
        k := FMemory.Inst.k;
        if (FMemory.IFR.f9 = 1) then
        begin
            // Repeated instruction. Fetch ybar from the IFR
            FMemory.Inst.ybar := FMemory.IFR.f1;
        end else
        begin
            // Non-repeated instructions
            // Add index register (b) to address (y)
            FMemory.Inst.ybar := FMemory.Inst.y;
            if (b = 0) then
                bval := 0
            else
                bval := FMemory.B[FMemory.IFR.f6, b].Value;
            case gConfig.Mode of
              m494:
              begin
                FMemory.Inst.ybar := FMemory.Inst.y + bval;
                if (FMemory.IFR.f7 = 0) then
                    FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
              end;
              m490:
              begin
                FMemory.Inst.ybar := FMemory.Inst.y + bval;
                FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                begin
                    FMemory.Inst.ybar := FMemory.Inst.y + bval;
                    FMemory.Inst.ybar := FMemory.Inst.ybar.Value15;
                end else
                begin
                    case FCurOpcode.InstType of
                      itRead:
                      begin
                        if ((k <> 0) and (k <> 4) and (k <> 7)) then
                            FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                                 (FMemory.SR[FMemory.Inst.s].Value shl 13)
                        else
                            FMemory.Inst.ybar := FMemory.Inst.y.Value15 + bval;
                      end;
                      itStore:
                      begin
                        if ((k <> 0) and (k <> 4)) then
                            FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                                 (FMemory.SR[FMemory.Inst.s].Value shl 13)
                        else
                            FMemory.Inst.ybar := FMemory.Inst.y.Value15 + bval;
                      end;
                      itReplace:
                      begin
                        FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                             (FMemory.SR[FMemory.Inst.s].Value shl 13)
                      end;
                      it77:
                      begin
                        if (FMemory.Inst.g = 7)  then
                            FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                                 (FMemory.SR[FMemory.Inst.s].Value shl 13)
                        else
                            FMemory.Inst.ybar := (FMemory.Inst.y.Value and $ff) + bval;
                      end;
                      itIO:
                      begin
                        FMemory.Inst.ybar := FMemory.Inst.y.Value13 + bval +
                                             (FMemory.SR[FMemory.Inst.s].Value shl 13)
                      end;
                    end;
                end;
              end;
            end;
        end;
        // Calculate absolute address, if applicable. Do not relocate "reserved" addresses.
        k := FMemory.Inst.k;
        FMemory.Operand := FMemory.Inst.ybar;
        if (FMemory.Inst.ybar >= $60) then
        begin
            if (FMemory.IFR.f8 = 0) then
                ri.Value := FMemory.RIR.Value
            else if ((b >= 4) and (b <= 7)) then
                ri := FMemory.PLR.LL
            else
                ri.Value := FMemory.RIR.Value;
            case FCurOpcode.InstType of
              itRead:
              begin
                if ((k <> 0) and (k <> 4) and (k <> 7)) then
                    FMemory.Operand := FMemory.Operand + ri;
              end;
              itStore:
              begin
                if ((k <> 0) and (k <> 4)) then
                    FMemory.Operand := FMemory.Operand + ri;
              end;
              itReplace:
              begin
                FMemory.Operand := FMemory.Operand + ri;
              end;
              it77:
              begin
                if ((FMemory.Inst.g <> 8) and               // Some of decimal insts. are oddballs
                    (FMemory.Inst.g <> 12)) then
                    FMemory.Operand := FMemory.Operand + ri;
              end;
              itIO:
              begin
                FMemory.Operand := FMemory.Operand + ri;
              end;
            end;
        end;
        if (FMemory.IFR.f9 = 0) then
        begin
            op := FMemory.Inst.f;
            g := FMemory.Inst.g;
            if (((op >= 48) and (op <= 53)) or
                ((op = 63) and (g >= 31) and (g <= 39))) then
                // Jump instructions
                FMemory.IFR.f1 := FMemory.P.Value + 1
            else
                FMemory.IFR.f1 := FMemory.Operand;
        end;
        if ((FMemory.IFR.f3 = 1) or (FMemory.IFR.f3 = 3)) then
        begin
            if (FCurOpcode.Priviledged) then
                raise EProgramProtection.Create('Attempt to execute priviledged instruction');
        end;
        FInterruptPending := False;
    except
      on E: EIllegalInstruction do
      begin
        if (FInterruptPending) then
        begin
            raise;
        end else
        begin
            FInterruptVector := IIllegalInstruction;
            FInterruptPending := True;
        end;
      end;
      on E: EProgramProtection do
      begin
        if (FInterruptPending) then
        begin
            raise;
        end else
        begin
            FInterruptVector := IProgramProtection;
            FInterruptPending := True;
        end;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
end;

procedure T494Cpu.FloatToNative(r: Double; var w1, w2: UInt32);
var
    pi: PUint64;
    sign, exp, mantissa, rslt: UInt64;
begin
    pi := PUint64(@r);
    // The following bit of cruft converts from IEEE floating point format
    // to 494 format.
    //
    // 60-bit instead of 64-bit
    // exponent biased by 1024 rather than 1023
    // mantissa has zero to left of decimal instead of 1
    sign := pi^ shr 63;
    exp := (pi^ shr 52) and $7ff;
    mantissa := (pi^ and $fffffffffffff) shr 4;
    // If value is not zero then adjust exponent and mantissa.
    // Otherwise, leave them as zero.
    if ((exp <> 0) or (mantissa <> 0)) then
    begin
        exp := exp + 2;
        mantissa := (mantissa shr 1) or $800000000000;
    end;

    rslt := (exp shl 48) or mantissa;
    if (sign <> 0) then
        rslt := not rslt;
    // Break result into 2 30-bit words
    w1 := (rslt shr 30) and BITS30;
    w2 := rslt and BITS30;
end;

procedure T494Cpu.FM;
var
    addr: UInt32;
    w1, w2: UInt32;
    op1, op2: Double;
begin
    addr := FMemory.Operand.Value;
    w1 := FMemory.A.Value;
    w2 := FMemory.Q.Value;
    op1 := NativeToFloat(w1, w2);
    w1 := FMemory.Fetch(addr);
    w2 := FMemory.Fetch(addr + 1);
    op2 := NativeToFloat(w1, w2);
    try
        op1 := op1 * op2;
    except
      on E: EOverflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFOverflow;
      end;
      on E: EUnderflow do
      begin
        FInterruptPending := True;
        FInterruptVector := IFFUnderflow;
      end;
      on E: Exception do
      begin
        raise;
      end;
    end;
    FloatToNative(op1, w1, w2);
    FMemory.A.Value := w1;
    FMemory.Q.Value := w2;
end;

procedure T494Cpu.FP;
var
    addr: UInt32;
    count: UInt32;
    op1, sign, exp, mantissa: UInt64;
begin
    addr := FMemory.Operand.Value;
    exp := FMemory.Fetch(addr).Value;
    // Get AQ, save the sign and make it positive.
    op1 := (UInt64(FMemory.A.Value) shl 30) or FMemory.Q.Value;
    sign := op1 and $800000000000000;
    if (sign <> 0) then
        op1 := not op1;
    // Truncate to 48 bits
    mantissa := op1 and $ffffffffffff;
    // If result = 0 then we are done.
    if ((exp = 0) and (mantissa = 0)) then
    begin
        FMemory.A.Value := 0;
        FMemory.Q.Value := 0;
        Exit;
    end;
    // Normalize
    count := 0;
    if (mantissa <> 0) then
    begin
        while ((mantissa and $800000000000) = 0) do
        begin
            mantissa := mantissa shl 1;
            Inc(count);
        end;
    end;
    // Adjust exponent to allow for normalization
    Dec(exp, count);
    // Combine results
    op1 := (exp shl 48) or mantissa;
    if (sign <> 0) then
        op1 := not op1;
    FMemory.A.Value := (op1 shr 30) and BITS30;
    FMemory.Q.Value := op1 and BITS30;
end;

procedure T494Cpu.FU;
var
    addr: UInt32;
    w1, w2: UInt32;
    op1, sign, exp, mantissa: UInt64;
    word: T494Word;
    hw: T494HalfWord;
begin
    addr := FMemory.Operand.Value;
    w1 := FMemory.A.Value;
    w2 := FMemory.Q.Value;
    op1 := UInt64(w1) shl 30 or w2;
    sign := op1 and $800000000000000;
    exp := (op1 and $7ff000000000000) shr 48;
    mantissa := op1 and $ffffffffffff;
    if (sign <> 0) then
    begin
        exp := not exp;
        mantissa := mantissa or $fff000000000000;
    end;
    FMemory.A.Value := (mantissa shr 30) and BITS30;
    FMemory.Q.Value := mantissa and BITS30;
    word := FMemory.Fetch(addr);
    hw.Value := exp;
    word.H2 := hw;
    FMemory.Store(addr, word);
end;

procedure T494Cpu.IllegalInst;
begin
    if (FMemory.Inst.f = $3f) then
        raise EIllegalInstruction.Createfmt('Illegal instruction (77%s)', [Copy(FormatOctal(Fmemory.Inst.g), 9)])
    else
        raise EIllegalInstruction.CreateFmt('Illegal instruction (%s)', [Copy(FormatOctal(Fmemory.Inst.f), 9)]);
end;

procedure T494Cpu.INMON;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    addr := BcrIn(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    Exit;
      1:    bcr.H2 := operand.H2;
      2:    bcr.H1 := operand.H1;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (not Assigned(FChannels[chan])) then
        Exit;
    FChannels[chan].ActivateInput(True);
end;

procedure T494Cpu.INMON490;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrIn(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    ;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (not Assigned(FChannels[chan])) then
        Exit;
    FChannels[chan].ActivateInput(True);
end;

procedure T494Cpu.INN;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    addr := BcrIn(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    Exit;
      1:    bcr.H2 := operand.H2;
      2:    bcr.H1 := operand.H1;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (Assigned(FChannels[chan])) then
        FChannels[chan].ActivateInput(False);
end;

procedure T494Cpu.INN490;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrIn(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    ;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (not Assigned(FChannels[chan])) then
        Exit;
    FChannels[chan].ActivateInput(False);
end;

function T494Cpu.IOFetch: T494Word;
// Fetch the operand for I/O type instructions
var
    b: Byte;
    hw: T494HalfWord;
begin
    case FMemory.Inst.khat of
      0:
      begin
        b := FMemory.Inst.b;
        Result := 0;
        if (b = 0) then
        begin
            hw.Value := FMemory.Operand.Value15;
            Result.H2 := hw;
        end else
        begin
            case gConfig.Mode of
              m494:
              begin
                if ((FMemory.IFR.f7 = 0) or ((b >= 1) and (b <= 3))) then
                begin
                    hw.Value := FMemory.Operand.Value15;
                    Result.H2 := hw;
                end else
                    Result.Value := FMemory.Operand.Value;
              end;
              m490:
              begin
                hw.Value := FMemory.Operand.Value15;
                Result.H2 := hw;
              end;
              m1230:
              begin
                 { TODO : Needs to allow for SR registers }
                if (FMemory.IFR.f7 = 0) then
                begin
                    hw.Value := FMemory.Operand.Value15;
                    Result.H2 := hw;
                end else
                    Result.Value := FMemory.Operand.Value;
              end;
            end;
        end;
      end;
      1:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H2;
        Result.H1 := 0;
      end;
      2:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H1;
        Result.H1 := 0;
      end;
      3:
      begin
        Result := FMemory.Fetch(FMemory.Operand.Value);
      end;
    end;
end;

procedure T494Cpu.JT;
var
    operand: T494Word;
    addr: UInt32;
begin
    operand := StdFetch;
    addr := operand.Value and BITS17;
    case FMemory.Inst.j of
      0:
      begin
        InterruptLockout := False;
      end;
      1:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        InterruptLockout := False;
      end;
      2:
      begin
        if (not FMemory.Q.IsNegative) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      3:
      begin
        if (FMemory.Q.IsNegative) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      4:
      begin
        if (FMemory.A.Value = 0) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      5:
      begin
        if (FMemory.A.Value <> 0) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      6:
      begin
        if (not FMemory.A.IsNegative) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      7:
      begin
        if (FMemory.A.IsNegative) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
    end;
end;

procedure T494Cpu.J;
var
    operand: T494Word;
    addr: UInt32;
begin
    operand := StdFetch;
    addr := operand.Value and BITS17;
    case FMemory.Inst.j of
      0:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
      end;
      1:
      begin
        if (ps1 in FPanelSwitches) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      2:
      begin
        if (ps2 in FPanelSwitches) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      3:
      begin
        if (ps3 in FPanelSwitches) then
            case gConfig.Mode of
              m494:
              begin
                FMemory.P := addr + FMemory.RIR.Value;
              end;
              m490:
              begin
                FMemory.P := addr;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                    FMemory.P := addr
                else
                    FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
              end;
            end;
      end;
      4:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        Include(FState, csHalted);
      end;
      5:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        if (ps5 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
      6:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        if (ps6 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
      7:
      begin
        case gConfig.Mode of
          m494:
          begin
            FMemory.P := addr + FMemory.RIR.Value;
          end;
          m490:
          begin
            FMemory.P := addr;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 = 0) then
                FMemory.P := addr
            else
                FMemory.P := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        if (ps7 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
    end;
end;

procedure T494Cpu.JACTI;
var
    chan: Byte;
    operand: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    if (not Assigned(FChannels[chan])) then
        Exit;
    if (FChannels[chan].InputActive) then
    begin
        case FMemory.Inst.khat of
          0:    FMemory.P := operand.Value;
          1,
          3:    FMemory.P := operand.H2.Value;
          2:    FMemory.P := operand.H1.Value;
        end;
    end;
end;

procedure T494Cpu.JACTI490;
var
    chan: Byte;
    operand: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    if (not Assigned(FChannels[chan])) then
        Exit;
    if (FChannels[chan].InputActive) then
    begin
        case FMemory.Inst.khat of
          0:    FMemory.P := operand.Value;
          1,
          3:    FMemory.P := operand.H2.Value;
          2:    FMemory.P := operand.H1.Value;
        end;
    end;
end;

procedure T494Cpu.JACTO;
var
    chan: Byte;
    operand: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    if (not Assigned(FChannels[chan])) then
        Exit;
    if (FChannels[chan].OutputActive) then
    begin
        case FMemory.Inst.khat of
          0:    FMemory.P := operand.Value;
          1,
          3:    FMemory.P := operand.H2.Value;
          2:    FMemory.P := operand.H1.Value;
        end;
    end;
end;

procedure T494Cpu.JACTO490;
var
    chan: Byte;
    operand: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    if (not Assigned(FChannels[chan])) then
        Exit;
    if (FChannels[chan].OutputActive) then
    begin
        case FMemory.Inst.khat of
          0:    FMemory.P := operand.Value;
          1,
          3:    FMemory.P := operand.H2.Value;
          2:    FMemory.P := operand.H1.Value;
        end;
    end;
end;

function T494Cpu.LeftShift(value: UInt32; count: Integer): UInt32;
begin
    if (count > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [count]);
    while (count > 0) do
    begin
        value := value shl 1;
        value := value or ((value and $40000000) shr 30);
        Dec(count);
    end;
    Result := value;
end;

function T494Cpu.LeftShift(value: UInt64; count: Integer): UInt64;
begin
    if (count > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [count]);
    while (count > 0) do
    begin
        value := value shl 1;
        value := value or ((value and $4000000000000000) shr 60);
        Dec(count);
    end;
    Result := value;
end;

procedure T494Cpu.LogicalProductSkip;
// Skip processing for logical product instructions
var
    parity: Byte;
    a: UInt32;
    j: Byte;
begin
    parity := 0;
    j := FMemory.Inst.j;
    // Calculate the parity if needed.
    // 0 = even parity, 1 = odd parity.
    if ((j = 2) or (j = 3)) then
    begin
        a := FMemory.A.Value;
        while (a <> 0) do
        begin
            parity := parity xor (a and $1);
            a := a shr 1;
        end;
    end;
    //
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (parity = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (parity <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if (FMemory.A.Value = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if (FMemory.A.Value <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (not FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

function T494Cpu.LogicalRightShift(value: UInt32; count: Integer): UInt32;
begin
    if (count > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [count]);
    while (count > 0) do
    begin
        value := (value shr 1);
        Dec(count);
    end;
    Result := value;
end;

procedure T494Cpu.LPLR;
begin
    FMemory.PLR.Value := FMemory.Fetch(FMemory.Operand.Value).Value;
end;

procedure T494Cpu.LRSA;
var
    shiftCount, value: UInt32;
begin
    shiftCount := FMemory.Inst.ybar.Value and $3f;
    value := LogicalRightShift(FMemory.A.Value, shiftCount);
    FMemory.A.Value := value;
end;

procedure T494Cpu.LRSAQ;
var
    shiftCount: UInt32;
    value: UInt64;
begin
    shiftCount := FMemory.Inst.ybar.Value and $3f;
    if (shiftCount > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [shiftCount]);
    value := (UInt64(FMemory.A.Value) shl 30) or FMemory.Q.Value;
    while (shiftCount > 0) do
    begin
        value := (value shr 1);
        Dec(shiftCount);
    end;
    FMemory.A.Value := value shr 30;
    FMemory.Q.Value := value;
end;

procedure T494Cpu.LRSQ;
var
    shiftCount, value: UInt32;
begin
    shiftCount := FMemory.Inst.ybar.Value and $3f;
    value := LogicalRightShift(FMemory.Q.Value, shiftCount);
    FMemory.Q.Value := value;
end;

procedure T494Cpu.LSA;
var
    operand: T494Word;
    shiftCount, value: UInt32;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    value := LeftShift(FMemory.A.Value, shiftCount);
    FMemory.A.Value := value;
end;

procedure T494Cpu.LSAQ;
var
    operand: T494Word;
    shiftCount: UInt32;
    value: UInt64;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    if (shiftCount > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [shiftCount]);
    value := (UInt64(FMemory.A.Value) shl 30) or FMemory.Q.Value;
    while (shiftCount > 0) do
    begin
        value := value shl 1;
        value := value or ((value and $1000000000000000) shr 60);
        Dec(shiftCount);
    end;
    FMemory.A.Value := value shr 30;
    FMemory.Q.Value := value;
end;

procedure T494Cpu.LSQ;
var
    operand: T494Word;
    shiftCount, value: UInt32;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    value := LeftShift(FMemory.Q.Value, shiftCount);
    FMemory.Q.Value := value;
end;

procedure T494Cpu.M;
var
    operand: T494Word;
    rslt, q, a: Int64;
    ovfl: Boolean;
begin
    operand := StdFetch;
    q := Int64(FMemory.Q);
    rslt := Int64(operand) * q;
    // Some quirk of the hardware requires this. See manual
    // page 4-12.
    if ((FMemory.Inst.k = 7) and (q < 0))  then
        rslt := -rslt;
    //
    if (rslt < 0) then
        rslt := rslt - 1;
    FMemory.Q.Value := rslt;
    a := (rslt shr 30) and BITS30;
    FMemory.A.Value := a;
    ovfl := (a <> 0) and (a <> BITS30);
    // Multiply specific skip conditions
    case FMemory.Inst.j of
      1,
      6:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (not ovfl) then
            FMemory.P := FMemory.P + 1;
      end;
      3,
      5:
      begin
        if (ovfl) then
            FMemory.P := FMemory.P + 1;
      end;
      4:                                                // Product <= 31 bits
      begin
        if  ((FMemory.A.Value and $1fffffff) = 0) or ((FMemory.A.Value and $1fffffff) = $1fffffff) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.MATE;
var
    test1, test2: UInt32;
begin
    test1 := FMemory.A.Value and FMemory.Q.Value;
    test2 := FMemory.Fetch(FMemory.Operand.Value).Value and FMemory.Q.Value;
    if (test1 = test2) then
        FMemory.P := FMemory.P + 1;
end;

procedure T494Cpu.MATL;
var
    test1, test2: UInt32;
begin
    test1 := FMemory.A.Value and FMemory.Q.Value;
    test2 := FMemory.Fetch(FMemory.Operand.Value).Value and FMemory.Q.Value;
    if (test1 < test2) then
        FMemory.P := FMemory.P + 1;
end;

function T494Cpu.NativeToFloat(w1, w2: UInt32): Double;
var
    r, sign, exp, mantissa: UInt64;
    pi: PUInt64;
begin
    if ((w1 = 0) and (w2 = 0)) then
    begin
        Result := 0;
        Exit;
    end;
    // Get the sign. If negative, take the ones complement of
    // everything to make it IEEE compatible.
    sign := w1 and BIT29;
    if (sign <> 0) then
    begin
        w1 := (not w1) and BITS30;
        w2 := (not w2) and BITS30;
    end;
    // Isolate the exponent, make 1023 biased and shift to
    // high order 12 bits of 64-bit word
    exp := (w1 shr 18);
    exp := ((exp and $7ff) - 2) shl 52;
    // Get the 48-bit mantissa. Shift out the first 1 bit
    // since the 1 preceeding the decimal is assumed in IEEE format.
    // Shift left 4 bits to align properly in 64-bit word
    mantissa := (UInt64(w1 and $3ffff) shl 30) or w2;
    mantissa := (mantissa shl 1) and $ffffffffffff;
    r := exp or (mantissa shl 4);
    // If negative, set the sign bit.
    if (sign <> 0) then
        r := r or $8000000000000000;
    // Convert to double
    pi := PUInt64(@Result);
    pi^ := r;
end;

procedure T494Cpu.NORM;
// Normalize AQ. Shift AQ until high order 2 bits are different
// and return shift count in Y.
var
    operand: T494Word;
    shiftCount, highBits: UInt32;
    value: UInt64;
begin
    operand := StdFetch;
    shiftCount := 0;
    value := (FMemory.A.Value shr 30) or FMemory.Q.Value;
    highBits := (value and $c00000000000000) shr 58;
    while ((shiftCount < 58) and (highBits <> 1) and (highBits <> 2)) do
    begin
        value := LeftShift(value, 1);
        highBits := (value and $c00000000000000) shr 58;
        Inc(shiftCount);
    end;
    FMemory.A.Value := value shr 30;
    FMemory.Q.Value := value;
    FMemory.Store(operand.Value, shiftCount, False);
end;

procedure T494Cpu.NormalSkip;
// Skip processing for "normal" instructions
begin
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (not FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if (FMemory.A.Value = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if (FMemory.A.Value <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (not FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.NotImplemented;
begin
    if (FMemory.Inst.f = $3f) then
        raise Exception.Createfmt('Instruction not implemented (77%s)', [Copy(FormatOctal(FCurOpcode.Opcode), 9)])
    else
        raise Exception.CreateFmt('Instruction not implemented (%s)', [Copy(FormatOctal(FCurOpcode.Opcode), 9)]);
end;

procedure T494Cpu.Plus1Skip;
// Skips for REPYPlus1 and REPYMinus1.
begin
    case FMemory.Inst.j of
      1:
      begin
        FMemory.P := FMemory.P + 1;
      end;
      2:
      begin
        if (not FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      3:
      begin
        if (FMemory.A.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      4:
      begin
        if (FMemory.Q.Value = 0) then
            FMemory.P := FMemory.P + 1;
      end;
      5:
      begin
        if (FMemory.Q.Value <> 0) then
            FMemory.P := FMemory.P + 1;
      end;
      6:
      begin
        if (not FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
      7:
      begin
        if (FMemory.Q.IsNegative) then
            FMemory.P := FMemory.P + 1;
      end;
    end;
end;

procedure T494Cpu.PreFetch(addr: T494Address);
var
    op, g: Byte;
begin
    FMemory.Inst := FMemory.Fetch(addr.Value).Value;           // fetch instruction word
    // Lookup opcode info
    op := FMemory.Inst.f;
    if (op = $3f) then
    begin
        g := FMemory.Inst.g;
        case gConfig.Mode of
          m494:     FCurOpcode := U494ExtOpcodes[g];
          m490:     IllegalInst;
          m1230:    FCurOpcode := U1230ExtOpcodes[g];
        end;
        FCurInstProc := FExtInstProcs[g];
    end else
    begin
        FCurOpcode := U494StdOpcodes[op];
        FCurInstProc := FStdInstProcs[op];
    end;
    if (FCurOpcode.SpurtMnemonic = 'UNK') then
        IllegalInst;
end;

procedure T494Cpu.RANLP;
begin
    ANLP;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RAN;
begin
    AN;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RALP;
begin
    ALP;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RA;
begin
    A;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RLP;
begin
    LLP;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RD;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := operand - 1;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RANQ;
begin
    LANQ;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RI;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := operand + 1;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RAQ;
begin
    LAQ;
    StdStore(FMemory.A);
end;

function T494Cpu.RightShift(value: UInt32; count: Integer): UInt32;
var
    fillBit: UInt32;
begin
    if (count > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [count]);
    fillBit := value and BIT29;
    while (count > 0) do
    begin
        value := (value shr 1) or fillBit;
        Dec(count);
    end;
    Result := value;
end;

procedure T494Cpu.SLJT;
var
    operand, mem: T494Word;
    hw: T494HalfWord;
    addr: UInt32;

    procedure StoreAndJump;
    begin
        case gConfig.Mode of
          m494:
          begin
            addr := addr + FMemory.RIR.Value;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 <> 0) then
                addr := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        mem := FMemory.Fetch(addr);
        hw.Value := FMemory.P.Value - FMemory.RIR.Value;
        mem.H2 := hw;
        FMemory.Store(addr, mem);
        FMemory.P := addr + 1;
    end;

begin
    operand := StdFetch;
    addr := operand.Value and BITS15;
    case FMemory.Inst.j of
      0:
      begin
        InterruptLockout := True;
      end;
      1:
      begin
        InterruptLockout := True;
        StoreAndJump;
      end;
      2:
      begin
        if (not FMemory.Q.IsNegative) then
            StoreAndJump;
      end;
      3:
      begin
        if (FMemory.Q.IsNegative) then
            StoreAndJump;
      end;
      4:
      begin
        if (FMemory.A.Value = 0) then
            StoreAndJump;
      end;
      5:
      begin
        if (FMemory.A.Value <> 0) then
            StoreAndJump;
      end;
      6:
      begin
        if (not FMemory.A.IsNegative) then
            StoreAndJump;
      end;
      7:
      begin
        if (FMemory.A.IsNegative) then
            StoreAndJump;
      end;
    end;
end;

procedure T494Cpu.SOSR;
begin
    NotImplemented;
end;

procedure T494Cpu.SLJ;
var
    operand, mem: T494Word;
    hw: T494Halfword;
    addr: UInt32;

    procedure StoreAndJump;
    begin
        case gConfig.Mode of
          m494:
          begin
            addr := addr + FMemory.RIR.Value;
          end;
          m1230:
          begin
            if (FMemory.IFR.f7 <> 0) then
                addr := (addr and BITS13) or UInt32(FMemory.SR[FMemory.Inst.s].Value) shl 13;
          end;
        end;
        mem := FMemory.Fetch(addr);
        hw.Value := FMemory.P.Value - FMemory.RIR.Value;
        mem.H2 := hw;
        FMemory.Store(addr, mem);
        FMemory.P := addr + 1;
    end;

begin
    operand := StdFetch;
    addr := operand.Value and BITS15;
    case FMemory.Inst.j of
      0:
      begin
        StoreAndJump;
      end;
      1:
      begin
        if (ps1 in FPanelSwitches) then
            StoreAndJump;
      end;
      2:
      begin
        if (ps2 in FPanelSwitches) then
            StoreAndJump;
      end;
      3:
      begin
        if (ps3 in FPanelSwitches) then
            StoreAndJump;
      end;
      4:
      begin
        StoreAndJump;
        Include(FState, csHalted);
      end;
      5:
      begin
        StoreAndJump;
        if (ps5 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
      6:
      begin
        StoreAndJump;
        if (ps6 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
      7:
      begin
        StoreAndJump;
        if (ps7 in FPanelSwitches) then
            Include(FState, csHalted);
      end;
    end;
end;

procedure T494Cpu.R;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.B[FMemory.IFR.f6, 7].Value := operand.Value;
    if (operand.Value = 0) then
    begin
        FMemory.P := FMemory.P + 1;
        Exit;
    end;
    FMemory.IFR.f1 := FMemory.Inst.ybar;
    FMemory.IFR.f2 := FMemory.Inst.j;
    FMemory.IFR.f9 := 1;
end;

procedure T494Cpu.RNOT;
begin
    NOTT;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RXOR;
begin
    XORR;
    StdStore(FMemory.A);
end;

procedure T494Cpu.ROR;
begin
    ORR;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RSSU;
begin
    SSU;
    StdStore(FMemory.A);
end;

procedure T494Cpu.RSA;
var
    operand: T494Word;
    shiftCount, value: UInt32;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    value := RightShift(FMemory.A.Value, shiftCount);
    FMemory.A.Value := value;
end;

procedure T494Cpu.RSAQ;
var
    operand: T494Word;
    shiftCount: UInt32;
    fillBit, value: UInt64;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    if (shiftCount > 59) then
        raise Exception.CreateFmt('Illegal shift count (%d)', [shiftCount]);
    value := (UInt64(FMemory.A.Value) shl 30) or FMemory.Q.Value;
    fillBit := value and $800000000000000;
    while (shiftCount > 0) do
    begin
        value := (value shr 1) or fillBit;
        Dec(shiftCount);
    end;
    FMemory.A.Value := value shr 30;
    FMemory.Q.Value := value;
end;

procedure T494Cpu.RSQ;
var
    operand: T494Word;
    shiftCount, value: UInt32;
begin
    operand := StdFetch;
    shiftCount := operand;
    shiftCount := shiftCount and $3f;
    value := RightShift(FMemory.Q.Value, shiftCount);
    FMemory.Q.Value := value;
end;

procedure T494Cpu.NOTT;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A.Value := FMemory.A.Value and (not operand.Value);
end;

procedure T494Cpu.XORR;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A.Value := FMemory.A.Value xor operand.Value;
end;

procedure T494Cpu.ORR;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A.Value := FMemory.A.Value or operand.Value;
end;

procedure T494Cpu.OUTMON;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    addr := BcrOut(chan);
    if ((FMemory.Inst.jhat mod 2) = 1) then
    begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(operand);
    end else
    begin
        bcr := FMemory.Fetch(addr.Value);
        case FMemory.Inst.khat of
          0:    Exit;
          1:    bcr.H2 := operand.H2;
          2:    bcr.H1 := operand.H1;
          3:    bcr := operand;
        end;
        FMemory.Store(addr.Value, bcr);
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ActivateOutput(True);
    end;
end;

procedure T494Cpu.OUTMON1230;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrOut(chan);
    if (FMemory.Inst.khat = 2) then
        addr := BcrExt(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    bcr := operand;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (Assigned(FChannels[chan])) then
    begin
        if (FMemory.Inst.khat = 2) then
            FChannels[chan].ActivateExternal(True)
        else
            FChannels[chan].ActivateOutput(True);
    end;
end;

procedure T494Cpu.OUTMON490;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrOut(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    ;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (Assigned(FChannels[chan])) then
        FChannels[chan].ActivateOutput(True);
end;

procedure T494Cpu.OUT;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    if (FInterruptActive) then
        chan := FMemory.IASR.Value
    else
        chan := FMemory.CSR.Value;
    addr := BcrOut(chan);
    if ((FMemory.Inst.jhat mod 2) = 1) then
    begin
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ExternalFunction(operand);
    end else
    begin
        bcr := FMemory.Fetch(addr.Value);
        case FMemory.Inst.khat of
          0:    Exit;
          1:    bcr.H2 := operand.H2;
          2:    bcr.H1 := operand.H1;
          3:    bcr := operand;
        end;
        FMemory.Store(addr.Value, bcr);
        if (Assigned(FChannels[chan])) then
            FChannels[chan].ActivateOutput(False);
    end;
end;

procedure T494Cpu.OUT1230;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrOut(chan);
    if (FMemory.Inst.khat = 2) then
        addr := BcrExt(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    bcr := operand;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (Assigned(FChannels[chan])) then
    begin
        if (FMemory.Inst.khat = 2) then
            FChannels[chan].ActivateExternal(False)
        else
            FChannels[chan].ActivateOutput(False);
    end;
end;

procedure T494Cpu.OUT490;
var
    operand: T494Word;
    chan: Byte;
    addr: T494Address;
    bcr: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    addr := BcrOut(chan);
    bcr := FMemory.Fetch(addr.Value);
    case FMemory.Inst.khat of
      0:    bcr.H2 := operand.H2;
      1:    bcr.H2 := operand.H2;
      2:    ;
      3:    bcr := operand;
    end;
    FMemory.Store(addr.Value, bcr);
    if (Assigned(FChannels[chan])) then
        FChannels[chan].ActivateOutput(False);
end;

procedure T494Cpu.SSR;
// Not sure how this instruction is supposed to work. All I know is
// 7770000000 means store SR0 in Q.
var
    r: Byte;
begin
    r := FMemory.Inst.j77;
    if (r > 2) then
        raise Exception.CreateFmt('Illegal SR (%d)', [r]);
    FMemory.Q.Value := FMemory.SR[r].Value;
end;

procedure T494Cpu.SSU;
var
    operand: T494Word;
    valq, vala, valop, bit: UInt32;
begin
    operand := StdFetch;
    valq := FMemory.Q.Value;
    vala := FMemory.A.Value;
    valop := operand.Value;
    bit := BIT29;
    while (bit <> 0) do
    begin
        if ((valq and bit) <> 0) then
        begin
            if ((valop and bit) = 0) then
                vala := vala and (not bit)
            else
                vala := vala or bit;
        end;
        bit := bit shr 1;
    end;
    FMemory.A.Value := vala;
end;

procedure T494Cpu.SFS;
// Scale Factor Shift. Shift A until high order 2 bits are different
// and return shift count in Q.
var
    shiftCount, highBits, value: UInt32;
begin
    shiftCount := 0;
    value := FMemory.A.Value;
    highBits := (value and $30000000) shr 28;
    while ((shiftCount < 28) and (highBits <> 1) and (highBits <> 2)) do
    begin
        value := LeftShift(value, 1);
        highBits := (value and $30000000) shr 28;
        Inc(shiftCount);
    end;
    FMemory.A.Value := value;
    FMemory.Q := shiftCount;
end;

procedure T494Cpu.SIFR;
var
    word: T494Word;
begin
    word.Value := FMemory.IFR.Value;
    FMemory.Store(FMemory.Operand.Value, word);
end;

procedure T494Cpu.SISR;
begin
    NotImplemented;
end;

procedure T494Cpu.Start;
var
    count: Integer;
begin
    count := 0;
    Exclude(FState, csHalted);
    try
        try
            while (not (csHalted in FState)) do
            begin
                Fetch;
                Execute;
                Inc(count);
                if (count >= 100) then
                begin
                    Application.ProcessMessages;
                    count := 0;
                end;
            end;
        except
          on E: Exception do
          begin
            if (Assigned(FOnDebug)) then
                FOnDebug(Self, E)
            else
                raise;
          end;
        end;
    finally
        Include(FState, csHalted);
    end;
end;

function T494Cpu.StdFetch: T494Word;
// Fetch the operand for read type instructions
var
    b: Byte;
    hw: T494HalfWord;
begin
    case FMemory.Inst.k of
      0:
      begin
        b := FMemory.Inst.b;
        Result := 0;
        if (b = 0) then
        begin
            hw.Value := FMemory.Operand.Value15;
            Result.H2 := hw;
        end else
        begin
            case gConfig.Mode of
              m494:
              begin
                if ((FMemory.IFR.f7 = 0) or ((b >= 1) and (b <= 3))) then
                begin
                    hw.Value := FMemory.Operand.Value15;
                    Result.H2 := hw;
                end else
                    Result.Value := FMemory.Operand.Value;
              end;
              m490:
              begin
                hw.Value := FMemory.Operand.Value15;
                Result.H2 := hw;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                begin
                    hw.Value := FMemory.Operand.Value15;
                    Result.H2 := hw;
                end else
                    Result.Value := FMemory.Operand.Value;
              end;
            end;
        end;
      end;
      1:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H2;
        Result.H1 := 0;
      end;
      2:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H1;
        Result.H1 := 0;
      end;
      3:
      begin
        Result := FMemory.Fetch(FMemory.Operand.Value);
      end;
      4:
      begin
        hw.Value := FMemory.Operand.Value15;
        Result.H2 := hw;
        if ((Result.H2.Value and BIT14) <> 0) then
        begin
            hw.Value := BITS15;
            Result.H1 := hw;
        end else
        begin
            hw.Value := 0;
            Result.H1 := hw;
        end;
      end;
      5:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H2;
        if ((Result.H2.Value and BIT14) <> 0) then
        begin
            hw.Value := BITS15;
            Result.H1 := hw;
        end else
        begin
            hw.Value := 0;
            Result.H1 := hw;
        end;
      end;
      6:
      begin
        Result.H2 := FMemory.Fetch(FMemory.Operand.Value).H1;
        if ((Result.H2.Value and BIT14) <> 0) then
        begin
            hw.Value := BITS15;
            Result.H1 := hw;
        end else
        begin
            hw.Value := 0;
            Result.H1 := hw;
        end;
      end;
      7:
      begin
        Result := FMemory.A;
      end;
    end;
end;

procedure T494Cpu.StdStore(value: T494Word);
var
    mem: T494Word;
    hw: T494HalfWord;
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    case FMemory.Inst.k of
      0:
      begin
        FMemory.Q := value;
      end;
      1:
      begin
        mem := FMemory.Fetch(addr);
        mem.H2 := value.H2;
        FMemory.Store(addr, mem);
      end;
      2:
      begin
        mem := FMemory.Fetch(addr);
        mem.H1 := value.H2;
        FMemory.Store(addr, mem);
      end;
      3:
      begin
        FMemory.Store(addr, value);
      end;
      4:
      begin
        FMemory.A := value;
      end;
      5:
      begin
        mem := FMemory.Fetch(addr);
        hw.Value := not value.H2.Value;
        mem.H2 := hw;
        FMemory.Store(addr, mem);
      end;
      6:
      begin
        mem := FMemory.Fetch(addr);
        hw.Value := not value.H2.Value;
        mem.H1 := hw;
        FMemory.Store(addr, mem);
      end;
      7:
      begin
        value.Value := not value.Value;
        FMemory.Store(addr, value);
      end;
    end;
end;

procedure T494Cpu.Stop;
begin
    Include(FState, csHalted);
end;

procedure T494Cpu.SA;
var
    operand: T494Word;
    half: T494HalfWord;
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    case FMemory.Inst.k of
      0:
      begin
        FMemory.Q.Value := FMemory.A.Value;
      end;
      1:
      begin
        operand := FMemory.Fetch(addr);
        operand.H2 := FMemory.A.H2;
        FMemory.Store(addr, operand);
      end;
      2:
      begin
        operand := FMemory.Fetch(addr);
        operand.H1 := FMemory.A.H2;
        FMemory.Store(addr, operand);
      end;
      3:
      begin
        FMemory.Store(addr, FMemory.A);
      end;
      4:
      begin
        FMemory.A := not FMemory.A;
      end;
      5:
      begin
        operand := FMemory.Fetch(addr);
        half := operand.H2;
        half := not FMemory.A.H2;
        operand.H2 := half;
        FMemory.Store(addr, operand);
      end;
      6:
      begin
        operand := FMemory.Fetch(addr);
        half := operand.H1;
        half := not FMemory.A.H2;
        operand.H1 := half;
        FMemory.Store(addr, operand);
      end;
      7:
      begin
        operand := FMemory.Fetch(addr);
        operand := not FMemory.A;
        FMemory.Store(addr, operand);
      end;
    end;
end;

procedure T494Cpu.SANQ;
begin
    FMemory.A := FMemory.A - FMemory.Q;
    AQStore(FMemory.A);
end;

procedure T494Cpu.SAQ;
begin
    FMemory.A := FMemory.A + FMemory.Q;
    AQStore(FMemory.A);
end;

procedure T494Cpu.DPS;
begin
    FMemory.Store(FMemory.Operand.Value, FMemory.A);
    FMemory.Store(FMemory.Operand.Value + 1, FMemory.Q);
end;

procedure T494Cpu.DPTE;
var
    addr: UInt32;
    op1, op2: T494DWord;
begin
    addr := FMemory.Operand.Value;
    op1.Value := (Int64(FMemory.A.Value) shl 30) or (FMemory.Q.Value);
    op2 := FMemory.FetchDWord(addr);
    if (op1.Value = op2.Value) then
        FMemory.P := FMemory.P + 1;
end;

procedure T494Cpu.DPTL;
var
    addr: UInt32;
    op1, op2: T494DWord;
begin
    addr := FMemory.Operand.Value;
    op1.Value := (Int64(FMemory.A.Value) shl 30) or (FMemory.Q.Value);
    op2 := FMemory.FetchDWord(addr);
    if (Int64(op1) < Int64(op2)) then
        FMemory.P := FMemory.P + 1;
end;

procedure T494Cpu.DT;
var
    test: UInt32;
    aq: TBcd;
    skip: Boolean;
begin
    skip := False;
    test := FMemory.Operand.Value;
    aq := FMemory.FetchBcdAQ;
    if (((test and $1) <> 0) and ((FMemory.IFR.f4 = 1) or (FMemory.IFR.f5 = 1))) then
        skip := True;
    if (((test and $2) <> 0) and (FMemory.IFR.f4 = 0) and (FMemory.IFR.f5 = 0)) then
        skip := True;
    if (((test and $4) <> 0) and (aq > 0)) then
        skip := True;
    if (((test and $8) <> 0) and (aq = 0)) then
        skip := True;
    if (((test and $10) <> 0) and (aq < 0)) then
        skip := True;
    if (((test and $20) <> 0) and (aq.Nibble[6] <> 0)) then
        skip := True;
    if (((test and $40) <> 0) and (aq.Nibble[7] <> 0)) then
        skip := True;
    if (((test and $80) <> 0) and (aq.Nibble[8] <> 0)) then
        skip := True;
    if (((test and $100) <> 0) and (aq.Nibble[9] <> 0)) then
        skip := True;
    if (((test and $200) <> 0) and (aq.Nibble[10] <> 0)) then
        skip := True;
    if (((test and $400) <> 0) and (aq <> 0)) then
        skip := True;
    if (skip) then
        FMemory.P.Value := FMemory.P.Value + 1;
end;

procedure T494Cpu.DTE;
var
    addr: UInt32;
    op1, op2: TBcd;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (op1 = op2) then
        FMemory.P.Value := FMemory.P.Value + 1;
end;

procedure T494Cpu.DTL;
var
    addr: UInt32;
    op1, op2: TBcd;
begin
    addr := FMemory.Operand.Value;
    op1 := FMemory.FetchBcdAQ;
    op2 := FMemory.FetchBcd(addr);
    if (op1 < op2) then
        FMemory.P.Value := FMemory.P.Value + 1;
end;

procedure T494Cpu.SB;
var
    addr, bval: UInt32;
    b1, b2: Byte;
    value: T494Word;
    hvalue: T494HalfWord;
begin
    b1 := FMemory.IFR.f6;
    b2 := FMemory.Inst.j;
    addr := FMemory.Operand.Value;
    if (b2 = 0) then
    begin
        value := 0;
        StdStore(value);
    end else
    begin
        case FMemory.Inst.k of
          0:
          begin
            FMemory.Q := FMemory.B[b1, b2].Value;
          end;
          1:
          begin
            value := FMemory.Fetch(addr);
            value.Value := (value.Value and $3ffe0000) or FMemory.B[b1, b2].Value;
            FMemory.Store(addr, value);
          end;
          2:
          begin
            value := FMemory.Fetch(addr);
            hvalue.Value := FMemory.B[b1, b2].Value;
            value.H1 := hvalue;
            FMemory.Store(addr, value);
          end;
          3:
          begin
            value.Value := FMemory.B[b1, b2].Value;
            FMemory.Store(addr, value);
          end;
          4:
          begin
            FMemory.A := FMemory.B[b1, b2].Value;
          end;
          5:
          begin
            value := FMemory.Fetch(addr);
            hvalue := value.H2;
            hvalue.Value := (FMemory.B[b1, b2].Value and BITS15) xor BITS15;
            value.H2 := hvalue;
            FMemory.Store(addr, value);
          end;
          6:
          begin
            value := FMemory.Fetch(addr);
            hvalue := value.H1;
            hvalue.Value := (FMemory.B[b1, b2].Value and BITS15) xor BITS15;
            value.H1 := hvalue;
            FMemory.Store(addr, value);
          end;
          7:
          begin
            value := FMemory.Fetch(addr);
            case gConfig.Mode of
              m494:
              begin
                if ((FMemory.IFR.f7 = 0) or ((b2 >= 1) and (b2 <= 3))) then
                begin
                    bval := (not FMemory.B[b1, b2].Value) and BITS15;
                    if ((bval and BIT14) = 0) then
                        value.Value := 0
                    else
                        value.Value := BITS30;
                    value.H2 := bval;
                end else
                begin
                    bval := (not FMemory.B[b1, b2].Value) and BITS17;
                    if ((bval and $10000) = 0) then
                        value.Value := 0
                    else
                        value.Value := $3ffe0000;
                    value.Value := value.Value or bval;
                end;
              end;
              m490:
              begin
                bval := (not FMemory.B[b1, b2].Value) and BITS15;
                if ((bval and BIT14) = 0) then
                    value.Value := 0
                else
                    value.Value := BITS30;
                value.H2 := bval;
              end;
              m1230:
              begin
                if (FMemory.IFR.f7 = 0) then
                begin
                    bval := (not FMemory.B[b1, b2].Value) and BITS15;
                    if ((bval and BIT14) = 0) then
                        value.Value := 0
                    else
                        value.Value := BITS30;
                    value.H2 := bval;
                end else
                begin
                    bval := (not FMemory.B[b1, b2].Value) and BITS17;
                    if ((bval and $10000) = 0) then
                        value.Value := 0
                    else
                        value.Value := $3ffe0000;
                    value.Value := value.Value or bval;
                end;
              end;
            end;
            FMemory.Store(addr, value);
          end;
        end;
    end;
end;

procedure T494Cpu.SBW;
var
    addr: UInt32;
    word: T494Word;
begin
    addr := FMemory.Operand.Value;
    word.Value := FMemory.B[1, 1].Value;
    FMemory.Store(addr, word);
    word.Value := FMemory.B[1, 2].Value;
    FMemory.Store(addr + 1, word);
    word.Value := FMemory.B[1, 3].Value;
    FMemory.Store(addr + 2, word);
    word.Value := FMemory.B[1, 4].Value;
    FMemory.Store(addr + 3, word);
    word.Value := FMemory.B[1, 5].Value;
    FMemory.Store(addr + 4, word);
    word.Value := FMemory.B[1, 6].Value;
    FMemory.Store(addr + 5, word);
    word.Value := FMemory.B[1, 7].Value;
    FMemory.Store(addr + 6, word);
end;

procedure T494Cpu.SC;
begin
    if (FMemory.Inst.khat = 3) then
        FMemory.Store(FMemory.Operand.Value, FMemory.IoStatus);
end;

procedure T494Cpu.SC1230;
var
    chan: Byte;
    operand: T494Word;
begin
    operand := IOFetch;
    chan := FMemory.Inst.jhat;
    if (not Assigned(FChannels[chan])) then
        Exit;
    case FMemory.Inst.khat of
      0:
      begin
        if (FChannels[chan].ExternalActive) then
            FMemory.P := operand.Value;
      end;
      1:
      begin
        if (FChannels[chan].ExternalActive) then
            FMemory.P := operand.H2.Value;
      end;
      2:
      begin
        FMemory.Store(FMemory.Operand.Value, FMemory.IoStatus);
      end;
      3:
      begin    // Octal 520 + channel # is external function status
        FMemory.Store(FMemory.Operand.Value, FMemory.Fetch(336 + chan));
      end;
    end;
end;

procedure T494Cpu.SCN;
var
    value: Byte;
begin
    if (FInterruptActive) then
        value := FMemory.IASR.Value
    else
        value := $1f;
    FMemory.Store(FMemory.Operand.Value, value);
end;

procedure T494Cpu.SESR;
begin
    NotImplemented;
end;

procedure T494Cpu.SetInterruptActive(const Value: Boolean);
begin
    FInterruptActive := Value;
    FMemory.IFR.LockedOut := Value;
    FMemory.RIR.LockedOut := Value;
    if (Value) then
        FRepeatDelay := True;
end;

procedure T494Cpu.SetInterruptLockout(const Value: Boolean);
begin
    FInterruptLockout := Value;
    if (not Value) then
        InterruptActive := False;
end;

procedure T494Cpu.SAND;
var
    value, mem: T494Word;
    hw: T494HalfWord;
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    value.Value := FMemory.A.Value and FMemory.Q.Value;
    case FMemory.Inst.k of
      0:
      begin
        FMemory.Q := value;
      end;
      1:
      begin
        mem := FMemory.Fetch(addr);
        mem.H2 := value.H2;
        FMemory.Store(addr, mem);
      end;
      2:
      begin
        mem := FMemory.Fetch(addr);
        mem.H1 := value.H2;
        FMemory.Store(addr, mem);
      end;
      3:
      begin
        FMemory.Store(addr, value);
      end;
      4:
      begin
        FMemory.A := value;
      end;
      5:
      begin
        mem := FMemory.Fetch(addr);
        mem.H2 := not value.H2;
        FMemory.Store(addr, mem);
      end;
      6:
      begin
        mem := FMemory.Fetch(addr);
        hw.Value := not value.H2.Value;
        mem.H1 := hw;
        FMemory.Store(addr, mem);
      end;
    end;
end;

procedure T494Cpu.SQ;
var
    operand: T494Word;
    half: T494HalfWord;
    addr: UInt32;
begin
    addr := FMemory.Operand.Value;
    case FMemory.Inst.k of
      0:
      begin
        FMemory.Q.Value := not FMemory.Q.Value;
      end;
      1:
      begin
        operand := FMemory.Fetch(addr);
        operand.H2 := FMemory.Q.H2;
        FMemory.Store(addr, operand);
      end;
      2:
      begin
        operand := FMemory.Fetch(addr);
        operand.H1 := FMemory.Q.H2;
        FMemory.Store(addr, operand);
      end;
      3:
      begin
        FMemory.Store(addr, FMemory.Q);
      end;
      4:
      begin
        FMemory.A := FMemory.Q;
      end;
      5:
      begin
        operand := FMemory.Fetch(addr);
        half := operand.H2;
        half := not FMemory.Q.H2;
        operand.H2 := half;
        FMemory.Store(addr, operand);
      end;
      6:
      begin
        operand := FMemory.Fetch(addr);
        half := operand.H1;
        half := not FMemory.Q.H2;
        operand.H1 := half;
        FMemory.Store(addr, operand);
      end;
      7:
      begin
        operand := FMemory.Fetch(addr);
        operand := not FMemory.Q;
        FMemory.Store(addr, operand);
      end;
    end;
end;

procedure T494Cpu.AN;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.A := FMemory.A - operand;
end;

procedure T494Cpu.ANLP;
var
    op1, op2: T494Word;
begin
    op1 := StdFetch;
    op2.Value := (FMemory.Q.Value and op1.Value);
    FMemory.A := FMemory.A - op2;
end;

procedure T494Cpu.ANQ;
var
    operand: T494Word;
begin
    operand := StdFetch;
    FMemory.Q := FMemory.Q - operand;
end;

{ T494ChannelList }

constructor T494ChannelList.Create;
var
    i: Integer;
begin
    inherited Create;
    for i := 0 to 23 do
        Add(nil);
end;

{ T494Device }

procedure T494Device.ActivateExternal(withMon: Boolean);
begin
    Lock;
    try
        FExternalMonitor := withMon;
        FExternalActive := True;
    finally
        Unlock;
    end;
    FEvent.SetEvent;
end;

procedure T494Device.ActivateInput(withMon: Boolean);
begin
    Lock;
    try
        FInputMonitor := withMon;
        FInputActive := True;
    finally
        Unlock;
    end;
    FEvent.SetEvent;
end;

procedure T494Device.ActivateOutput(withMon: Boolean);
begin
    Lock;
    try
        FOutputMonitor := withMon;
        FOutputActive := True;
    finally
        Unlock;
    end;
    FEvent.SetEvent;
end;

constructor T494Device.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited Create(True);
    FCpu := cpu;
    FMemory := mem;
    FChannel := chan;
    FEvent := TEvent.Create(nil, False, False, '');
    FCrit := TCriticalSection.Create;
end;

destructor T494Device.Destroy;
begin
    if (not Terminated) then
    begin
        FEvent.SetEvent;
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FEvent);
    FreeAndNil(FCrit);
    inherited Destroy;
end;

function T494Device.ExternalActive: Boolean;
begin
    Result := FExternalActive;
end;

procedure T494Device.ExternalFunctionWithMonitor(func: T494Word);
begin
    FExternalMonitor := True;
    ExternalFunction(func);
end;

function T494Device.FetchInputBcr: T494Word;
begin
    Result := FMemory.Fetch(BcrIn(FChannel), True);
end;

function T494Device.FetchOutputBcr: T494Word;
begin
    Result := FMemory.Fetch(BcrOut(FChannel), True);
end;

function T494Device.InputActive: Boolean;
begin
    Result := FInputActive;
end;

procedure T494Device.Lock;
begin
    FCrit.Acquire;
end;

function T494Device.OutputActive: Boolean;
begin
    Result := FOutputActive;
end;

procedure T494Device.QueueInterrupt(itype: T494InterruptType; vector: Smallint; status: UInt32);
var
    int: T494Interrupt;
begin
    // Do not queue interrupts if interrups are disabled for this device
    case gConfig.Mode of
      m490:
      begin
        if (FInpIntLockout and (vector >= IIsiInput490) and (vector < (IIsiInput490 + 16))) then
            Exit;
        if (FOutIntLockout and (vector >= IIsiOutput490) and (vector < (IIsiOutput490 + 16))) then
            Exit;
      end;
      m1230:
      begin
        if (FInpIntLockout and (vector >= IIsiInput1230) and (vector < (IIsiInput1230 + 16))) then
            Exit;
        if (FOutIntLockout and (vector >= IIsiOutput1230) and (vector < (IIsiOutput1230 + 16))) then
            Exit;
        if (FExtIntLockout and (vector >= IIsiExternal1230) and (vector < (IIsiExternal1230 + 16))) then
            Exit;
      end;
    end;
    int.IType := itype;
    int.Vector := vector;
    int.Channel := FChannel;
    int.Status := status;
    FCpu.Interrupts.Enqueue(int);
end;

procedure T494Device.StoreInputBcr(Value: T494Word);
begin
    Fmemory.Store(BcrIn(FChannel), Value, True);
end;

procedure T494Device.StoreOutputBcr(Value: T494Word);
begin
    Fmemory.Store(BcrOut(FChannel), Value, True);
end;

procedure T494Device.Terminate;
begin
    FEvent.SetEvent;
    inherited Terminate;
end;

procedure T494Device.TerminateExternal;
begin
    Lock;
    try
        FExternalMonitor := False;
        FExternalActive := False;
    finally
        Unlock;
    end;
end;

procedure T494Device.TerminateInput;
begin
    Lock;
    try
        FInputMonitor := False;
        FInputActive := False;
    finally
        Unlock;
    end;
end;

procedure T494Device.TerminateOutput;
begin
    Lock;
    try
        FOutputMonitor := False;
        FOutputActive := False;
    finally
        Unlock;
    end;
end;

procedure T494Device.Unlock;
begin
    FCrit.Release;
end;

{ T494CardDevice }

procedure T494CardDevice.AddBlankCards(count: Integer);
var
    cfr: TCardFileRec;
begin
    Lock;
    try
        Inc(FInputCount, count);
        FHopperEmpty := False;
        cfr.FileName := '';
        cfr.BlankCards := count;
        FFiles.Add(cfr);
    finally
        Unlock;
    end;
end;

procedure T494CardDevice.AddFile(fname, rpgType: String);
var
    fin: TCardFileStream;
    cfr: TCardFileRec;
    cclIn: TCCLStream;
    cclr: TCCLRec;
    extn: String;
    rootDir: String;
    itemp: Integer;
begin
    extn := LowerCase(ExtractFileExt(fname));
    if (extn = '.ccl') then
    begin
        rootDir := '.';
        cclIn := TCCLStream.Create(fname, fmOpenRead);
        try
            while (cclIn.Read(cclr)) do
            begin
                case cclr.FileType of
                  ctRootDir:
                  begin
                    rootDir := cclr.Name;
                  end;
                  ctData:
                  begin
                    if ((Pos(':', cclr.Name) <> 2) and (Pos('\', cclr.Name) <> 1)) then
                        cclr.Name := rootDir + '\' + cclr.Name;
                    AddFile(cclr.Name, cclr.RPGType);
                  end;
                  ctBlanks:
                  begin
                    if (TryStrToInt(cclr.Name, itemp)) then
                        AddBlankCards(itemp)
                    else
                        raise Exception.CreateFmt('Invalid # of blank cards in /BLANKS command (%s)',
                                                  [cclr.Name]);
                  end;
                end;
            end;
        finally
            cclIn.Free;
        end;
        Exit;
    end;
    Lock;
    try
        extn := LowerCase(ExtractFileExt(fname));
        if (extn = '.rpg') then
            fin := TRPGCardStream.Create(fname, fmOpenRead, rpgType)
        else
            fin := TCardFileStream.Create(fname, fmOpenRead);
        try
            FInputCount := FInputCount + fin.RecordCount;
            FHopperEmpty := False;
        finally
            fin.Free;
        end;
        cfr.FileName := fname;
        cfr.RPGType := rpgType;
        cfr.BlankCards := 0;
        FFiles.Add(cfr);
    finally
        Unlock;
    end;
end;

constructor T494CardDevice.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited Create(cpu, mem, chan);
    FFiles := TCardFileList.Create;
end;

destructor T494CardDevice.Destroy;
begin
    FreeAndNil(FFiles);
    inherited Destroy;
end;

function T494CardDevice.OpenNextFile: Boolean;
var
    cfr: TCardFileRec;
    ext: String;
begin
    Result := False;
    if (Assigned(FCurrentFile)) then
    begin
        FFiles.Delete(0);
        FreeAndNil(FCurrentFile);
    end;
    if (FFiles.Count > 0) then
    begin
        cfr := FFiles[0];
        if (cfr.FileName <> '') then
        begin
            ext := LowerCase(ExtractFileExt(cfr.FileName));
            if (ext = '.rpg') then
                FCurrentFile := TRPGCardStream.Create(cfr.FileName, fmOpenRead, cfr.RPGType)
            else
                FCurrentFile := TCardFileStream.Create(cfr.FileName, fmOpenRead)
        end else
            FCurrentFile := TBlankCardStream.Create(cfr.BlankCards);
        Result := True;
    end;
end;

end.
