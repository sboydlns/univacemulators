unit Cpu;

interface

uses Windows, SysUtils, Classes, Forms, Dialogs, SyncObjs, Data.FmtBcd,
     Bcd, U9030Types;

type
  // I/O Status Tabler
  TIOST = class
  private
    FPendingCount: Integer;
    FIntPending: Boolean;
    FSuspended: Boolean;                    // offset to first status to be copied after suspend
    FSuspendedOffset: Integer;              // offset into BCSW at which to resume
    FStatLength: Integer;                   // Length of most recent status in words
    FStatChannel: Byte;                     // Channel number of most recent status
    FLock: TCriticalSection;
    procedure TableInterrupt(resume: Boolean);
  public
    constructor Create;
    destructor Destroy; override;
    procedure DecPendingCount;
    procedure IntRequest;
    procedure Resume;
    procedure ProcessInterrupts;
    property IntPending: Boolean read FIntPending write FIntPending;
  end;

  // Program Status Word
  TProcessorMode = ( pmSupervisor = 0, pmProgram );
  TRegisterSet = ( rsSupervisor = 0, rsProgram );
  TProcessorEmulation = ( emNative = 0, em9300, em360 );

  TPSW = class
  private
    FTimerIntEnabled: Boolean;
    FIOSTIntEnabled: Boolean;
    FKey: Byte;
    FAscii: Boolean;
    FRegisterSet: TRegisterSet;
    FMode: TProcessorMode;
    FEmulation: TProcessorEmulation;
    FMonitorMode: Boolean;
    FIntCode: Byte;
    FInstLength: Byte;
    FCondCode: Byte;
    FFixedOvflExcp: Boolean;
    FDecOvflExcp: Boolean;
    FCharacteristicOvflExcp: Boolean;
    FSignificantExcp: Boolean;
    FInstAddr: TMemoryAddress;
    function GetAsDblWord: TDblWord;
    procedure SetAsDblWord(const Value: TDblWord);
    procedure SetTimerIntEnabled(const Value: Boolean);
    procedure SetAscii(const Value: Boolean);
    procedure SetCharacteristicOvflExcp(const Value: Boolean);
    procedure SetCondCode(const Value: Byte);
    procedure SetDecOvflExcp(const Value: Boolean);
    procedure SetEmulation(const Value: TProcessorEmulation);
    procedure SetFixedOvflExcp(const Value: Boolean);
    procedure SetInstAddr(const Value: TMemoryAddress);
    procedure SetInstLength(const Value: Byte);
    procedure SetIntCode(const Value: Byte);
    procedure SetIOSTIntEnabled(const Value: Boolean);
    procedure SetKey(const Value: Byte);
    procedure SetMode(const Value: TProcessorMode);
    procedure SetMonitorMode(const Value: Boolean);
    procedure SetRegisterSet(const Value: TRegisterSet);
    procedure SetSignificantExcp(const Value: Boolean);
  public
    property AsDblWord: TDblWord read GetAsDblWord write SetAsDblWord;
    property TimerIntEnabled: Boolean read FTimerIntEnabled write SetTimerIntEnabled;
    property IOSTIntEnabled: Boolean read FIOSTIntEnabled write SetIOSTIntEnabled;
    property Key: Byte read FKey write SetKey;
    property Ascii: Boolean read FAscii write SetAscii;
    property RegisterSet: TRegisterSet read FRegisterSet write SetRegisterSet;
    property Mode: TProcessorMode read FMode write SetMode;
    property Emulation: TProcessorEmulation read FEmulation write SetEmulation;
    property MonitorMode: Boolean read FMonitorMode write SetMonitorMode;
    property IntCode: Byte read FIntCode write SetIntCode;
    property InstLength: Byte read FInstLength write SetInstLength;
    property CondCode: Byte read FCondCode write SetCondCode;
    property FixedOvflExcp: Boolean read FFixedOvflExcp write SetFixedOvflExcp;
    property DecOvflExcp: Boolean read FDecOvflExcp write SetDecOvflExcp;
    property CharacteristicOvflExcp: Boolean read FCharacteristicOvflExcp write SetCharacteristicOvflExcp;
    property SignificantExcp: Boolean read FSignificantExcp write SetSignificantExcp;
    property InstAddr: TMemoryAddress read FInstAddr write SetInstAddr;
  end;

  // Instruction register
  TInstruction = class
  private
    FInst: array [0..5] of Byte;
    function GetAsDblWord: TDblWord;
    function GetByte(idx: Integer): Byte;
    procedure SetByte(idx: Integer; const Value: Byte);
  public
    function B1: Byte; inline;
    function B2: Byte; inline;
    function BranchMask: Byte; inline;
    procedure Clear;
    procedure Fetch;
    function ImmedOperand: Byte; inline;
    function Length: Byte; inline;
    function Length1: Byte; inline;
    function Length2: Byte; inline;
    function Off1: UInt16; inline;
    function Off2: UInt16; inline;
    function R1: Byte; inline;
    function R2: Byte; inline;
    function X1: Byte; inline;
    property AsDblWord: TDblWord read GetAsDblWord;
    property AsBytes[idx: Integer]: Byte read GetByte write SetByte;
    property Opcode: Byte read FInst[0];
  end;

  TDebugEvent = procedure(Sender: TObject; E: Exception) of object;

  TCpu = class
  private
    FIOST: TIOST;
    FExecuteInProgress: Boolean;
    FMachineCheckMasked: Boolean;
    FMachineCheckPending: Boolean;
    FMonitorPending: Boolean;
    FOpcode: TOpcode;
    FProgramExceptionMasked: Boolean;
    FProgramExceptionPending: Boolean;
    FRegisters: array [TRegisterSet, 0..15] of TWord;
    FRelocateReg: TMemoryAddress;
    FTimerReg: TWord;
    FTimerEnabled: Boolean;
    FState: TProcessorState;
    FTimerPending: Boolean;
    FInhibitTimer: Boolean;
    FMsgTimer: Int64;
    FMSTimer: Int64;
    FTimerFreq: Int64;
    FMsgInterval: Int64;
    FMSInterval: Int64;
    FInstCount: Int64;
    FOnDebug: TDebugEvent;
    function Compare(op1, op2: TWord): Byte;
    function CompareLogical(op1, op2: UInt32): Byte; overload;
    function CompareLogical(op1, op2: Byte): Byte; overload;
    procedure DivideTestOverflow(dividend: TDblWord; divisor: TWord;
                             var quotient, remainder: TWord);
    procedure Execute;
    procedure Fetch;
    function GetAbsAddress(b: Byte; off: UInt16): TMemoryAddress; overload; inline;
    function GetAbsAddress(b, x: Byte; off: UInt16): TMemoryAddress; overload; inline;
    function GetRegisters(i: TRegisterSet; j: Integer): TWord;
    function GetRelAddress(b: Byte; off: UInt16): TMemoryAddress; overload; inline;
    function GetRelAddress(b, x: Byte; off: UInt16): TMemoryAddress; overload; inline;
    procedure InitIOST;
    procedure InitMachineCheck;
    procedure InitMonitor;
    procedure InitProgramException;
    procedure InitTimer;
    procedure GetPackedOperands(var bcd1, bcd2: TBcd);
    function PackedCC(value: TBcd; len: Integer): Byte;
    procedure SumSetCondCode(op1, op2: THalfWord; var sum: THalfWord) overload;
    procedure SumSetCondCode(op1, op2: TWord; var sum: TWord) overload;
    procedure SumSetCondCode(op1, op2: TDblWord; var sum: TDblWord); overload;
    // Instruction implementations
    procedure A;
    procedure AH;
    procedure AI;
    procedure AP;
    procedure AR;
    procedure BAL;
    procedure BALR;
    procedure BC;
    procedure BCT;
    procedure BCTR;
    procedure BCR;
    procedure C;
    procedure CLC;
    procedure CLR;
    procedure CLI;
    procedure CH;
    procedure CR;
    procedure CVB;
    procedure D;
    procedure DIAG;
    procedure EX;
    procedure HPR;
    procedure IC;
    procedure ISK;
    procedure L;
    procedure LA;
    procedure LCS;
    procedure LH;
    procedure LM;
    procedure LPSW;
    procedure LR;
    procedure LTR;
    procedure M;
    procedure MP;
    procedure MVC;
    procedure MVI;
    procedure MVN;
    procedure MVZ;
    procedure N;
    procedure NC;
    procedure NI;
    procedure O;
    procedure OC;
    procedure OI;
    procedure ORR;
    procedure PACK;
    procedure S;
    procedure SH;
    procedure SIO;
    procedure SLL;
    procedure SLM;
    procedure SPM;
    procedure SR;
    procedure SRL;
    procedure SSK;
    procedure SSM;
    procedure SSTM;
    procedure ST;
    procedure STC;
    procedure STH;
    procedure STM;
    procedure STR;
    procedure SVC;
    procedure TM;
    procedure XC;
    procedure XI;
    procedure XR;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Run;
    procedure Step(enable: Boolean);
    procedure Stop;
    property InhibitTimer: Boolean read FInhibitTimer write FInhibitTimer;
    property IOST: TIOST read FIOST;
    property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
    property Registers[i: TRegisterSet; j: Integer]: TWord read GetRegisters;
    property RelocateReg: TMemoryAddress read FRelocateReg;
    property State: TProcessorState read FState;
  end;

implementation

uses Globals, Channels, Memory;

{ TCpu }

procedure TCpu.A;
var
    r: Integer;
    op1, op2, sum: TWord;
begin
    r := CurInst.R1;
    op1 := FRegisters[PSW.RegisterSet, r];
    op2 := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    SumSetCondCode(op1, op2, sum);
    FRegisters[PSW.RegisterSet, r] := sum;
end;

procedure TCpu.AH;
var
    r: Integer;
    op1, op2, sum: TWord;
begin
    r := CurInst.R1;
    op1 := FRegisters[PSW.RegisterSet, r];
    op2 := Core.FetchHalfWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    SumSetCondCode(op1, op2, sum);
    FRegisters[PSW.RegisterSet, r] := sum;
end;

procedure TCpu.AI;
var
    op1, op2, sum: THalfWord;
    addr: TMemoryAddress;
begin
    op1 := CurInst.ImmedOperand;
    // Extend sign
    if ((op1 and $80) <> 0) then
        op1 := op1 or THalfWord($FF00);
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := Core.FetchHalfWord(PSW.Key, addr);
    SumSetCondCode(op1, op2, sum);
    Core.StoreHalfWord(PSW.Key, addr, sum);
end;

procedure TCpu.AP;
var
    bcd1, bcd2, rslt: TBcd;
begin
    GetPackedOperands(bcd1, bcd2);
    rslt := bcd1 + bcd2;
    Core.StorePacked(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.Off1), CurInst.Length1, rslt);
    PSW.CondCode := PackedCC(rslt, CurInst.Length1);
    if (PSW.DecOvflExcp and (PSW.CondCode = 3)) then
        raise EDecimalOverflow.Create('AP caused overflow');
end;

procedure TCpu.AR;
var
    sum: TWord;
begin
    SumSetCondCode(FRegisters[PSW.RegisterSet, CurInst.R1], FRegisters[PSW.RegisterSet, CurInst.R2], sum);
    FRegisters[PSW.RegisterSet, CurInst.R1] := sum;
end;

procedure TCpu.BAL;
begin
    FRegisters[PSW.RegisterSet, CurInst.R1] := PSW.InstAddr - FRelocateReg;
    PSW.InstAddr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
end;

procedure TCpu.BALR;
begin
    FRegisters[PSW.RegisterSet, CurInst.R1] := PSW.InstAddr - FRelocateReg;
    if (CurInst.R2 <> 0) then
        PSW.InstAddr := TMemoryAddress(FRegisters[PSW.RegisterSet, CurInst.R2]) + FRelocateReg;
end;

procedure TCpu.BC;
var
    test: Byte;
    jump: Boolean;
begin
    jump := False;
    test := CurInst.BranchMask;
    case PSW.CondCode of
      0:
        jump := ((test and $80) <> 0);
      1:
        jump := ((test and $40) <> 0);
      2:
        jump := ((test and $20) <> 0);
      3:
        jump := ((test and $10) <> 0);
    end;
    if (jump) then
        PSW.InstAddr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
end;

procedure TCpu.BCR;
var
    test: Byte;
    jump: Boolean;
begin
    jump := False;
    test := CurInst.BranchMask;
    case PSW.CondCode of
      0:
        jump := ((test and $80) <> 0);
      1:
        jump := ((test and $40) <> 0);
      2:
        jump := ((test and $20) <> 0);
      3:
        jump := ((test and $10) <> 0);
    end;
    if (jump) then
        PSW.InstAddr := TMemoryAddress(FRegisters[PSW.RegisterSet, CurInst.R2]) + FRelocateReg;
end;

procedure TCpu.BCT;
var
    r: Integer;
    count: TWord;
begin
    r := CurInst.R1;
    count := FRegisters[PSW.RegisterSet, r];
    Dec(count);
    FRegisters[PSW.RegisterSet, r] := count;
    if (count <> 0) then
        PSW.InstAddr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
end;

procedure TCpu.BCTR;
var
    r1, r2: Integer;
    count: TWord;
begin
    r1 := CurInst.R1;
    r2 := CurInst.R2;
    count := FRegisters[PSW.RegisterSet, r1];
    Dec(count);
    FRegisters[PSW.RegisterSet, r1] := count;
    if ((count <> 0) and (r2 <> 0)) then
        PSW.InstAddr := TMemoryAddress(FRegisters[PSW.RegisterSet, r2]) + FRelocateReg;
end;

procedure TCpu.C;
var
    op1, op2: TWord;
begin
    op1 := FRegisters[PSW.RegisterSet, CurInst.R1];
    op2 := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    PSW.CondCode := Compare(op1, op2);
end;

procedure TCpu.CH;
var
    op1: TWord;
    op2: THalfWord;
begin
    op1 := FRegisters[PSW.RegisterSet, CurInst.R1];
    op2 := Core.FetchHalfWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    PSW.CondCode := Compare(op1, op2);
end;

procedure TCpu.CLC;
var
    len: Integer;
    op1, op2: TMemoryAddress;
    b1, b2: Byte;
    CC: Byte;
begin
    CC := 0;
    len := CurInst.Length + 1;
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2);
    while (len > 0) do
    begin
        b1 := Core.FetchByte(PSW.Key, op1);
        b2 := Core.FetchByte(PSW.Key, op2);
        if (b1 < b2) then
        begin
            CC := 1;
            Break;
        end
        else if (b1 > b2) then
        begin
            CC := 2;
            Break;
        end;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
    PSW.CondCode := CC;
end;

procedure TCpu.CLI;
var
    b1, b2: Byte;
begin
    b2 := CurInst.ImmedOperand;
    b1 := Core.FetchByte(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.Off1));
    PSW.CondCode := CompareLogical(b1, b2);
end;

procedure TCpu.CLR;
var
    op1, op2: UInt32;
begin
    op1 := FRegisters[PSW.RegisterSet, CurInst.R1];
    op2 := FRegisters[PSW.RegisterSet, CurInst.R2];
    PSW.CondCode := CompareLogical(op1, op2);
end;

function TCpu.Compare(op1, op2: TWord): Byte;
begin
    if (op1 = op2) then
        Result := 0
    else if (op1 < op2) then
        Result := 1
    else
        Result := 2;
end;

function TCpu.CompareLogical(op1, op2: Byte): Byte;
begin
    if (op1 = op2) then
        Result := 0
    else if (op1 < op2) then
        Result := 1
    else
        Result := 2;
end;

function TCpu.CompareLogical(op1, op2: UInt32): Byte;
begin
    if (op1 = op2) then
        Result := 0
    else if (op1 < op2) then
        Result := 1
    else
        Result := 2;
end;

procedure TCpu.CR;
var
    r1, r2: TWord;
begin
    r1 := FRegisters[PSW.RegisterSet, CurInst.R1];
    r2 := FRegisters[PSW.RegisterSet, CurInst.R2];
    PSW.CondCode := Compare(r1, r2);
end;

constructor TCpu.Create;
begin
    FIOST := TIOST.Create;
    Opcodes.FindOpcode('A').Proc := A;
    Opcodes.FindOpcode('AH').Proc := AH;
    Opcodes.FindOpcode('AI').Proc := AI;
    Opcodes.FindOpcode('AP').Proc := AP;
    Opcodes.FindOpcode('AR').Proc := AR;
    Opcodes.FindOpcode('BAL').Proc := BAL;
    Opcodes.FindOpcode('BALR').Proc := BALR;
    Opcodes.FindOpcode('BC').Proc := BC;
    Opcodes.FindOpcode('BCR').Proc := BCR;
    Opcodes.FindOpcode('BCT').Proc := BCT;
    Opcodes.FindOpcode('BCTR').Proc := BCTR;
    Opcodes.FindOpcode('C').Proc := C;
    Opcodes.FindOpcode('CLC').Proc := CLC;
    Opcodes.FindOpcode('CLR').Proc := CLR;
    Opcodes.FindOpcode('CLI').Proc := CLI;
    Opcodes.FindOpcode('CH').Proc := CH;
    Opcodes.FindOpcode('CR').Proc := CR;
    Opcodes.FindOpcode('CVB').Proc := CVB;
    Opcodes.FindOpcode('D').Proc := D;
    Opcodes.FindOpcode('DIAG').Proc := DIAG;
    Opcodes.FindOpcode('EX').Proc := EX;
    Opcodes.FindOpcode('HPR').Proc := HPR;
    Opcodes.FindOpcode('IC').Proc := IC;
    Opcodes.FindOpcode('ISK').Proc := ISK;
    Opcodes.FindOpcode('L').Proc := L;
    Opcodes.FindOpcode('LA').Proc := LA;
    Opcodes.FindOpcode('LCS').Proc := LCS;
    Opcodes.FindOpcode('LH').Proc := LH;
    Opcodes.FindOpcode('LM').Proc := LM;
    Opcodes.FindOpcode('LPSW').Proc := LPSW;
    Opcodes.FindOpcode('LR').Proc := LR;
    Opcodes.FindOpcode('LTR').Proc := LTR;
    Opcodes.FindOpcode('M').Proc := M;
    Opcodes.FindOpcode('MP').Proc := MP;
    Opcodes.FindOpcode('MVC').Proc := MVC;
    Opcodes.FindOpcode('MVI').Proc := MVI;
    Opcodes.FindOpcode('MVN').Proc := MVN;
    Opcodes.FindOpcode('MVZ').Proc := MVZ;
    Opcodes.FindOpcode('N').Proc := N;
    Opcodes.FindOpcode('NC').Proc := NC;
    Opcodes.FindOpcode('NI').Proc := NI;
    Opcodes.FindOpcode('O').Proc := O;
    Opcodes.FindOpcode('OC').Proc := OC;
    Opcodes.FindOpcode('OI').Proc := OI;
    Opcodes.FindOpcode('OR').Proc := ORR;
    Opcodes.FindOpcode('PACK').Proc := PACK;
    Opcodes.FindOpcode('S').Proc := S;
    Opcodes.FindOpcode('SH').Proc := SH;
    Opcodes.FindOpcode('SIO').Proc := SIO;
    Opcodes.FindOpcode('SLL').Proc := SLL;
    Opcodes.FindOpcode('SLM').Proc := SLM;
    Opcodes.FindOpcode('SPM').Proc := SPM;
    Opcodes.FindOpcode('SR').Proc := SR;
    Opcodes.FindOpcode('SRL').Proc := SRL;
    Opcodes.FindOpcode('SSK').Proc := SSK;
    Opcodes.FindOpcode('SSM').Proc := SSM;
    Opcodes.FindOpcode('SSTM').Proc := SSTM;
    Opcodes.FindOpcode('ST').Proc := ST;
    Opcodes.FindOpcode('STC').Proc := STC;
    Opcodes.FindOpcode('STH').Proc := STH;
    Opcodes.FindOpcode('STM').Proc := STM;
    Opcodes.FindOpcode('STR').Proc := STR;
    Opcodes.FindOpcode('SVC').Proc := SVC;
    Opcodes.FindOpcode('TM').Proc := TM;
    Opcodes.FindOpcode('XC').Proc := XC;
    Opcodes.FindOpcode('XI').Proc := XI;
    Opcodes.FindOpcode('XR').Proc := XR;
end;

procedure TCpu.CVB;
var
    bcd: TBcd;
begin
    bcd := Core.FetchPacked(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1), 7);
    FRegisters[PSW.RegisterSet, CurInst.R1] := Integer(bcd);
    if (Int64(bcd) > MaxInt) then
        raise EDecimalDivideException.Create('CVB overflow');
end;

procedure TCpu.D;
var
    r: Integer;
    dividend: TDblWord;
    divisor: TWord;
begin
    r := CurInst.R1;
    if ((r mod 2) <> 0) then
        raise ESpecificationException.Create('Register not even for divide');
    dividend := (TDblWord(FRegisters[PSW.RegisterSet, r]) shl 32) or
                FRegisters[PSW.RegisterSet, r + 1];
    divisor := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    DivideTestOverflow(dividend, divisor, FRegisters[PSW.RegisterSet, r + 1], FRegisters[PSW.RegisterSet, r]);
end;

destructor TCpu.Destroy;
begin
    FreeAndNil(FIOST);
    inherited;
end;

procedure TCpu.DIAG;
// The undocumented diagnose instruction. I can only make guesses based on
// a dis-assembly of the OS.
//
// It looks like the immediate operand gives the function code.
//
// Function code guesses:
//   15 - Scan for task control block that matches certain criteria

var
    code: Byte;

    function TCBMatches(tcb: TMemoryAddress; offset, match: THalfWord): Boolean;
    // Check the content of the TCB to see if it is a match.
    // If offset = 0 then check JT$WAIT and $JT$WAIT + 1 equal to zero.
    // Otherwise check to see if any of the bits given in match are set in
    // the byte at offset.
    begin
        if (offset = 0) then
        begin
            Result := Core.FetchWord(PSW.Key, tcb + 4) = 0;
        end else
        begin
            Result := (Core.FetchByte(PSW.Key, tcb + offset) and match) <> 0;
        end;
    end;

    procedure ScanSwitchList;
    // Scan the switch list given by the address in the least significant
    // half word of B1. Keep scanning until we a pointer with the most
    // significant half word = $ffff.
    //
    // If the contents of B1 + 1 <> 0 then do not start matching until after
    // the TCB given by the contents of B1 + 1 is reached.
    //
    // If the most significant half word of b1 is zero then a TCB matches if JT$WAIT and
    // JT$WAIT + 1 = 0;
    var
        r: Integer;
        offset, match: THalfWord;
        switchList, firstTcb, tcb, skipUntil: TMemoryAddress;
    begin
        psw.CondCode := 2;
        // Start scanning the switch list
        r := CurInst.B1;
        switchList := (FRegisters[PSW.RegisterSet, r] and $ffff) + FRelocateReg;
//        if (switchList <> $408) then
//            raise Exception.Create('DIAG 15 for other than address $408');
        offset := FRegisters[PSW.RegisterSet, r] shr 16;
        match := CurInst.Off1;
        if (match = $104) then
            raise Exception.Create('Match = $104');
        firstTcb := TMemoryAddress(Core.FetchWord(PSW.Key, switchList));
        skipUntil := FRegisters[PSW.RegisterSet, r + 1];
        if (skipUntil <> 0) then
            Inc(skipUntil, FRelocateReg);
        while ((firstTcb shr 16) <> $ffff) do       // until end of list
        begin
            if (firstTcb <> 0) then
            begin
                firstTcb := (firstTcb and $ffffff) + FRelocateReg;
                tcb := firstTcb;
                repeat
                    if (skipUntil = 0) then
                    begin
                        if (TCBMatches(tcb, offset, match)) then
                        begin
                            psw.CondCode := 0;
                            FRegisters[PSW.RegisterSet, r] := switchList - FRelocateReg;
                            FRegisters[PSW.RegisterSet, r + 1] := TWord(tcb - FRelocateReg);
                            Break;
                        end;
                    end else if (skipUntil = tcb) then
                        skipUntil := 0;
                    tcb := TMemoryAddress(Core.FetchWord(PSW.Key, tcb) and $ffffff) +
                           FRelocateReg;
                until (tcb = firstTcb);
            end;
            Inc(switchList, 4);
            firstTcb := TMemoryAddress(Core.FetchWord(PSW.Key, switchList));
        end;
    end;

begin
    code := CurInst.ImmedOperand;
    case code of
      14:
      begin
        FRegisters[PSW.RegisterSet, CurInst.B1] := 0;
        PSW.CondCode := 0;
//        PSW.CondCode := 1;
        { TODO : Figure out what this is suppsed to do. }
        // No idea what this is supposed to do. Implement as no-op for now.
        ;
      end;
      15:
      begin
        ScanSwitchList;
      end;
      else     raise ESpecificationException.Create('Unimplemented DIAG function');
    end;
end;

procedure TCpu.DivideTestOverflow(dividend: TDblWord; divisor: TWord; var quotient, remainder: TWord);
var
    q: TDblWord;
begin
    if (divisor = 0) then
        raise EFixedDivideException.Create('Divide by zero');
    q := dividend div divisor;
    if (Abs(q) > MaxInt) then
        raise EFixedDivideException.Create('Divide overflow');
    quotient := q;
    remainder := dividend mod divisor;
end;

procedure TCpu.EX;
// Execute the instruction given by the op1. This is going to get bizarre.
var
    r: Integer;
    oldPsw: TDblWord;
begin
    if (FExecuteInProgress) then
        raise EExecuteException.Create('Nested execute instruction');

    FExecuteInProgress := True;
    try
        // Save the current PSW
        oldPsw := PSW.AsDblWord;
        // Save stuff we need from the current instruction
        r := CurInst.R1;
        // Point to instruction to be executed, fetch it and check for errors
        PSW.InstAddr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
        CurInst.Fetch;
        FOpcode := Opcodes.FindOpcode(CurInst.Opcode);
        if (FOpcode.Privileged and (PSW.FMode <> pmSupervisor)) then
            raise EExecuteException.Create(Format('%s is privileged', [FOpcode.Code]));
        if (not Assigned(FOpcode.Proc)) then
            raise EIllegalOpcode.Create(Format('%s is not implemented @ %6.6x', [FOpcode.Code, PSW.InstAddr]));
        // Modify inst to be executed with contents of R1
        if (r <> 0) then
            CurInst.AsBytes[1] := CurInst.AsBytes[1] or FRegisters[PSW.RegisterSet, r];
        // Restore the old PSW and execute the instruction
        PSW.AsDblWord := oldPsw;
        Execute;
    finally
        FExecuteInProgress := False;
    end;
end;

procedure TCpu.Execute;
begin
    if (Assigned(FOnDebug)) then
        FOnDebug(Self, nil);
    FOpcode.Proc;
end;

procedure TCpu.Fetch;
var
    curTimer: Int64;
begin
    QueryPerformanceCounter(curTimer);
    if ((curTimer - FMsgTimer) > FMsgInterval) then
    begin
        Application.ProcessMessages;
        FMsgTimer := curTimer;
    end;
    if ((not FInhibitTimer) and FTimerEnabled and ((curTimer - FMSTimer) > FMSInterval)) then
    begin
        FTimerReg := (FTimerReg - 1) and $ffffff;
        if (FTimerReg = 0) then
            FTimerPending := True;
        FMSTimer := curTimer;
    end;

    if (FIOST.FPendingCount <> 0) then
        FIOST.ProcessInterrupts;
    // Invoke interrupt handlers in priority sequence
    if (FMachineCheckPending) then
        InitMachineCheck
    else if (FProgramExceptionPending) then
        InitProgramException
    else if (FTimerPending and PSW.TimerIntEnabled) then
        InitTimer
    else if (FIOST.IntPending and PSW.IOSTIntEnabled) then
        InitIOST
    else if (FMonitorPending) then
        InitMonitor;
    // Fetch next instruction and make sure it is valid
    CurInst.Fetch;
    FOpcode := Opcodes.FindOpcode(CurInst.Opcode);
    if (FOpcode.Privileged and (PSW.FMode <> pmSupervisor)) then
        raise EPrivilegedInst.Create(Format('%s is privileged', [FOpcode.Code]));
    if (not Assigned(FOpcode.Proc)) then
        raise EIllegalOpcode.Create(Format('%s is not implemented @ %6.6x', [FOpcode.Code, PSW.InstAddr]));
end;

function TCpu.GetAbsAddress(b: Byte; off: UInt16): TMemoryAddress;
begin
    Result := off + FRelocateReg;
    if (b <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, b]) + Result;
end;

function TCpu.GetAbsAddress(b, x: Byte; off: UInt16): TMemoryAddress;
begin
    Result := off + FRelocateReg;
    if (b <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, b]) + Result;
    if (x <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, x]) + Result;
end;

procedure TCpu.GetPackedOperands(var bcd1, bcd2: TBcd);
var
    op1, op2: TMemoryAddress;
begin
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2);
    bcd1 := Core.FetchPacked(PSW.Key, op1, CurInst.Length1);
    bcd2 := Core.FetchPacked(PSW.Key, op2, CurInst.Length2);
end;

function TCpu.GetRegisters(i: TRegisterSet; j: Integer): TWord;
begin
    Result := FRegisters[i, j];
end;

function TCpu.GetRelAddress(b: Byte; off: UInt16): TMemoryAddress;
begin
    Result := off;
    if (b <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, b]) + Result;
end;

function TCpu.GetRelAddress(b, x: Byte; off: UInt16): TMemoryAddress;
begin
    Result := off;
    if (b <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, b]) + Result;
    if (x <> 0) then
        Result := UInt32(FRegisters[PSW.RegisterSet, x]) + Result;
end;

procedure TCpu.HPR;
begin
    FState := FState + [psHalted];
end;

procedure TCpu.IC;
var
    w: TWord;
begin
    w := FRegisters[PSW.RegisterSet, CurInst.R1] and $ffffff00;
    w := w or Core.FetchByte(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    FRegisters[PSW.RegisterSet, CurInst.R1] := w;
end;

procedure TCpu.InitIOST;
begin
    PSW.IntCode := 0;
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, IOST_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, IOST_NEW);
    PSW.InstLength := 0;
//    PSW.TimerIntEnabled := False;
//    PSW.IOSTIntEnabled := False;
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
    FIOST.IntPending := False;
end;

procedure TCpu.InitMachineCheck;
begin
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, MACH_CHECK_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, MACH_CHECK_NEW);
    PSW.InstLength := 0;
//    PSW.TimerIntEnabled := False;
//    PSW.IOSTIntEnabled := False;
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
    FMachineCheckPending := False;
end;

procedure TCpu.InitMonitor;
begin
    PSW.IntCode := 0;
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, MONITOR_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, MONITOR_NEW);
    PSW.InstLength := 0;
//    PSW.TimerIntEnabled := False;
//    PSW.IOSTIntEnabled := False;
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
    FMonitorPending := False;
end;

procedure TCpu.InitProgramException;
begin
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, PGM_EXCP_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, PGM_EXCP_NEW);
    PSW.InstLength := 0;
//    PSW.TimerIntEnabled := False;
//    PSW.IOSTIntEnabled := False;
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
    FProgramExceptionPending := False;
end;

procedure TCpu.InitTimer;
begin
    PSW.IntCode := 0;
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, TIMER_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, TIMER_NEW);
    PSW.InstLength := 0;
//    PSW.TimerIntEnabled := False;
//    PSW.IOSTIntEnabled := False;
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
    FTimerPending := False;
end;

procedure TCpu.ISK;
var
    k: Byte;
begin
    k := Core.FetchStorageKey(FRegisters[PSW.RegisterSet, CurInst.R2]);
    FRegisters[PSW.RegisterSet, CurInst.R1] :=
        (Registers[PSW.RegisterSet, CurInst.R1] and $ffffff00) or k;
end;

procedure TCpu.L;
begin
    FRegisters[PSW.RegisterSet, CurInst.R1] :=
        Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
end;

procedure TCpu.LA;
begin
    FRegisters[PSW.RegisterSet, CurInst.R1] := GetRelAddress(CurInst.B1, CurInst.X1, CurInst.Off1) and $ffffff;
end;

procedure TCpu.LCS;
// Load control storage. I have no real idea of what this instruction
// is supposed to do. I just know that I need to set the condition
// code to 1 for the boot process to succeed.
begin
    PSW.CondCode := 1;
end;

procedure TCpu.LH;
var
    addr: TMemoryAddress;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
    FRegisters[PSW.RegisterSet, CurInst.R1] := Core.FetchHalfWord(PSW.Key, addr);
end;

procedure TCpu.LM;
var
    addr: TMemoryAddress;
    r: Integer;
    done: Boolean;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    r := CurInst.R1;
    done := False;
    while (not done) do
    begin
        FRegisters[PSW.RegisterSet, r] := Core.FetchWord(PSW.Key, addr);
        done := (r = CurInst.R2);
        Inc(addr, 4);
        Inc(r);
        if (r > 15) then
            r := 0;
    end;
end;

procedure TCpu.LPSW;
begin
    if (CurInst.ImmedOperand <> 0) then
        raise ESpecificationException.CreateFmt('LPSW with non-zero immediate @ %6.6x', [PSW.InstAddr]);
    PSW.AsDblWord := Core.FetchDblWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.Off1));
    PSW.IntCode := 0;
    if (FMachineCheckMasked) then
        FMachineCheckMasked := False
    else
        FProgramExceptionMasked := False;
end;

procedure TCpu.LR;
begin
    FRegisters[PSW.RegisterSet, CurInst.R1] := FRegisters[PSW.RegisterSet, CurInst.R2];
end;

procedure TCpu.LTR;
var
    r2: TWord;
begin
    r2 := FRegisters[PSW.RegisterSet, CurInst.R2];
    FRegisters[PSW.RegisterSet, CurInst.R1] := r2;
    if (r2 = 0) then
        PSW.CondCode := 0
    else if (r2 < 0) then
        PSW.CondCode := 1
    else
        PSW.CondCode := 2;
end;

procedure TCpu.M;
var
    r: Integer;
    multiplicand, multiplier, rslt: TDblWord;
begin
    r := CurInst.R1;
    if ((r mod 2) <> 0) then
        raise ESpecificationException.Create('Register not even for multiply');
    multiplicand := FRegisters[PSW.RegisterSet, r + 1];
    multiplier := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    rslt := multiplicand * multiplier;
    FRegisters[PSW.RegisterSet, r] := rslt shr 32;
    FRegisters[PSW.RegisterSet, r + 1] := rslt and $ffffffff;
end;

procedure TCpu.MP;
var
    l1, l2: Integer;
    d2, i: Integer;
    bcd1, bcd2, rslt: TBcd;
begin
    l1 := CurInst.Length1;
    l2 := CurInst.Length2;
    if ((l2 > 7) or (l2 >= l1)) then
        raise ESpecificationException.Create('Multiply packed L2 error');
    GetPackedOperands(bcd1, bcd2);
    // Check multiplicand for enough leading zero digits to hold the multiplier
    d2 := ((l2 + 1) * 2) - 1;
    for i := 0 to d2 - 1 do
    begin
        if (bcd1.Nibble[i] <> 0) then
            raise EDataException.Create('Multiply packed insufficient op1 leading zeros');
    end;
    rslt := bcd1 * bcd2;
    Core.StorePacked(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.Off1), l1, rslt);
end;

procedure TCpu.MVC;
var
    len: Integer;
    src, dest: TMemoryAddress;
begin
    len := CurInst.Length;
    src := GetAbsAddress(CurInst.B2, CurInst.Off2);
    dest := GetAbsAddress(CurInst.B1, CurInst.Off1);
    while (len >= 0) do
    begin
        Core.StoreByte(PSW.Key, dest, Core.FetchByte(PSW.Key, src));
        Inc(src);
        Inc(dest);
        Dec(len);
    end;
end;

procedure TCpu.MVI;
var
    addr: TMemoryAddress;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    Core.StoreByte(PSW.Key, addr, CurInst.ImmedOperand);
end;

procedure TCpu.MVN;
var
    len: Integer;
    src, dest: TMemoryAddress;
    rslt: Byte;
begin
    len := CurInst.Length;
    src := GetAbsAddress(CurInst.B2, CurInst.Off2);
    dest := GetAbsAddress(CurInst.B1, CurInst.Off1);
    while (len >= 0) do
    begin
        rslt := (Core.FetchByte(PSW.Key, dest) and $F0) or (Core.FetchByte(PSW.Key, src) and $0F);
        Core.StoreByte(PSW.Key, dest, rslt);
        Inc(src);
        Inc(dest);
        Dec(len);
    end;
end;

procedure TCpu.MVZ;
var
    len: Integer;
    src, dest: TMemoryAddress;
    rslt: Byte;
begin
    len := CurInst.Length;
    src := GetAbsAddress(CurInst.B2, CurInst.Off2);
    dest := GetAbsAddress(CurInst.B1, CurInst.Off1);
    while (len >= 0) do
    begin
        rslt := (Core.FetchByte(PSW.Key, dest) and $0F) or (Core.FetchByte(PSW.Key, src) and $F0);
        Core.StoreByte(PSW.Key, dest, rslt);
        Inc(src);
        Inc(dest);
        Dec(len);
    end;
end;

procedure TCpu.N;
var
    w: TWord;
begin
    w := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1)) and
         FRegisters[PSW.RegisterSet, CurInst.R1];
    FRegisters[PSW.RegisterSet, CurInst.R1] := w;
    if (w = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.NC;
var
    len: Integer;
    op1, op2: TMemoryAddress;
    rslt: Byte;
begin
    len := CurInst.Length;
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2);
    PSW.CondCode := 0;
    while (len >= 0) do
    begin
        rslt := Core.FetchByte(PSW.Key, op1) and Core.FetchByte(PSW.Key, op2);
        Core.StoreByte(PSW.Key, op1, rslt);
        if (rslt <> 0) then
            PSW.CondCode := 1;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
end;

procedure TCpu.NI;
var
    addr: TMemoryAddress;
    b: Byte;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    b := Core.FetchByte(PSW.Key, addr) and CurInst.ImmedOperand;
    Core.StoreByte(PSW.Key, addr, b);
    if (b = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.O;
var
    w: TWord;
begin
    w := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1)) or
         FRegisters[PSW.RegisterSet, CurInst.R1];
    FRegisters[PSW.RegisterSet, CurInst.R1] := w;
    if (w = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.OC;
var
    len: Integer;
    op1, op2: TMemoryAddress;
    rslt: Byte;
begin
    len := CurInst.Length;
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2);
    PSW.CondCode := 0;
    while (len >= 0) do
    begin
        rslt := Core.FetchByte(PSW.Key, op1) or Core.FetchByte(PSW.Key, op2);
        Core.StoreByte(PSW.Key, op1, rslt);
        if (rslt <> 0) then
            PSW.CondCode := 1;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
end;

procedure TCpu.OI;
var
    addr: TMemoryAddress;
    b: Byte;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    b := Core.FetchByte(PSW.Key, addr) or CurInst.ImmedOperand;
    Core.StoreByte(PSW.Key, addr, b);
    if (b = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.ORR;
var
    w: TWord;
begin
    w := FRegisters[PSW.RegisterSet, CurInst.R1] or FRegisters[PSW.RegisterSet, CurInst.R2];
    FRegisters[PSW.RegisterSet, CurInst.R1] := w;
    if (w = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.PACK;
var
    l1, l2: Integer;
    op1, op2: TMemoryAddress;
    b: Byte;
begin
    l1 := CurInst.Length1;
    l2 := CurInst.Length2;
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1) + TMemoryAddress(l1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2) + TMemoryAddress(l2);
    b := Core.FetchByte(PSW.Key, op2);
    Core.StoreByte(PSW.Key, op1, ((b and $0F) shl 4) or ((b and $F0) shr 4));
    Dec(op1);
    Dec(op2);
    Dec(l1);
    Dec(l2);
    while ((l1 >= 0) or (l2 >= 0)) do
    begin
        if (l1 < 0) then
            Break;
        if (l2 >= 0) then
        begin
            b := Core.FetchByte(PSW.Key, op2) and $0F;
            Dec(op2);
            Dec(l2);
            if (l2 >= 0) then
            begin
                b := b or ((Core.FetchByte(PSW.Key, op2) and $0F) shl 4);
                Dec(op2);
                Dec(l2);
            end;
        end else
            b := 0;
        Core.StoreByte(PSW.Key, op1, b);
        Dec(op1);
        Dec(l1);
    end;
end;

function TCpu.PackedCC(value: TBcd; len: Integer): Byte;
var
    test: Integer;
begin
    test := BCDCompare(value, NullBcd);
    if (BcdPrecision(value) > (((len + 1) * 2) - 1)) then
        Result := 3
    else if (test < 0) then
        Result := 1
    else if (test > 0) then
        Result := 2
    else
        Result := 0;
end;

procedure TCpu.Reset;
var
    i: Integer;
begin
    for i := 0 to 7 do
    begin
        if (Assigned(Adapters.Channel[i])) then
            Adapters.Channel[i].Reset;
    end;
    FMachineCheckMasked := False;
    FMachineCheckPending := False;
    FMonitorPending := False;
    FProgramExceptionMasked := False;
    FProgramExceptionPending := False;
    FRelocateReg := 0;
    FState := [ psHalted ];
    FTimerEnabled := False;
    FTimerPending := False;
    FIOST.FPendingCount := 0;
    PSW.AsDblWord := 0;
    CurInst.Clear;
end;

procedure TCpu.Run;
//var
//    runStart, runEnd: Int64;
begin
    FInstCount := 0;
    QueryPerformanceFrequency(FTimerFreq);
    QueryPerformanceCounter(FMsgTimer);
    FMSTimer := FMsgTimer;
//    runStart := FMsgTimer;
    FMsgInterval := Trunc(FTimerFreq * 0.1);            // Call ProcessMessages every 100ms
    FMSInterval := Trunc(FTimerFreq * 0.001);           // # tick in a millisecond
    FState := FState - [psHalted, psError];
    while ((FState * [psHalted, psError]) = []) do
    begin
        try
            Fetch;
            Execute;
            Inc(FInstCount);
            if (psSingleStep in FState) then
                FState := FState + [psHalted];
        except
          on E: EProgramException do
          begin
            if ((not FProgramExceptionMasked) and (not FMachineCheckMasked)) then
            begin
                PSW.IntCode := E.IntCode;
                FProgramExceptionPending := True;
                FProgramExceptionMasked := True;
            end else if (FMachineCheckMasked) then
            begin
                FState := [ psError ];
                raise Exception.Create('Program exception while MC set. Unrecoverable error.');
            end else
            begin
                FMachineCheckPending := True;
                FMachineCheckMasked := True;
                PSW.IntCode := $ec;
            end;
          end;

          on E: EMachineCheck do
          begin
            if (not FMachineCheckMasked) then
            begin
                PSW.IntCode := E.IntCode;
                FMachineCheckPending := True;
                FMachineCheckMasked := True;
            end else
            begin
                FState := [ psError ];
                raise Exception.Create('Machine check while MC set. Unrecoverable error.');
            end;
          end;

          else
          begin
            FState := [ psError ];
            raise;
          end;
        end;
    end;
//    QueryPerformanceCounter(runEnd);
//    ShowMessageFmt('# inst = %d  run time = %d ms', [FInstCount, (runEnd - runStart) div (FTimerFreq div 1000)]);
end;

procedure TCpu.S;
var
    r: Integer;
    op1, op2, sum: TWord;
begin
    r := CurInst.R1;
    op1 := FRegisters[PSW.RegisterSet, r];
    op2 := Core.FetchWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    SumSetCondCode(op1, -op2, sum);
    FRegisters[PSW.RegisterSet, r] := sum;
end;

procedure TCpu.SH;
var
    r: Integer;
    op1, op2, sum: TWord;
begin
    r := CurInst.R1;
    op1 := FRegisters[PSW.RegisterSet, r];
    op2 := Core.FetchHalfWord(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1));
    SumSetCondCode(op1, -op2, sum);
    FRegisters[PSW.RegisterSet, r] := sum;
end;

procedure TCpu.SIO;
// Start I/O. This instruction is largely undocumented and the hits that are found
// in the programmers reference seem to be misleading at best. The documention seems to
// imply that if an I/O is issued to a non-existent device then the SIO instruction
// should set the condition code to 3 and do nothing else. This is contradicted by OS/3
// itself which expected a UNIT_CHECK status followed by a sense command returning
// CMD_REJECT.
var
    addr: TWord;
    chan: Byte;
begin
    addr := GetRelAddress(CurInst.B1, CurInst.Off1);
    chan := (addr shr 8) and $7;
    if (not Assigned(Adapters.Channel[chan])) then
        raise EOperationException.Create('Invalid channel #')
    else
        PSW.CondCode := Adapters.Channel[chan].SIO(addr);
end;

procedure TCpu.SLL;
var
    count: UInt32;
begin
    count := GetRelAddress(CurInst.B1, CurInst.Off1) and $3f;
    FRegisters[PSW.RegisterSet, CurInst.R1] :=
        FRegisters[PSW.RegisterSet, CurInst.R1] shl count;
end;

procedure TCpu.SLM;
// Supervisor load multiple. No docs for this. I'm guessing that
// I am supposed to load the program register set.
var
    addr: TMemoryAddress;
    r: Integer;
    done: Boolean;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    r := CurInst.R1;
    done := False;
    while (not done) do
    begin
        FRegisters[rsProgram, r] := Core.FetchWord(PSW.Key, addr);
        done := (r = CurInst.R2);
        Inc(addr, 4);
        Inc(r);
        if (r > 15) then
            r := 0;
    end;
end;

procedure TCpu.SPM;
var
    val: TWord;
begin
    val := FRegisters[PSW.RegisterSet, CurInst.R1];
    PSW.CondCode := (val shr 4) and $3;
    PSW.FixedOvflExcp := (val and $08) <> 0;
    PSW.DecOvflExcp := (val and $04) <> 0;
    PSW.CharacteristicOvflExcp := (val and $02) <> 0;
    PSW.SignificantExcp := (val and $01) <> 0;
end;

procedure TCpu.SR;
var
    op1, op2: TWord;
begin
    op1 := FRegisters[PSW.RegisterSet, CurInst.R1];
    op2 := FRegisters[PSW.RegisterSet, CurInst.R2];
    SumSetCondCode(op1, -op2, FRegisters[PSW.RegisterSet, CurInst.R1]);
    if ((PSW.CondCode = 3) and PSW.FixedOvflExcp) then
        raise EFixedOverflow.Create('SR caused overflow');
    
end;

procedure TCpu.SRL;
var
    count: UInt32;
begin
    count := GetRelAddress(CurInst.B1, CurInst.Off1) and $3f;
    FRegisters[PSW.RegisterSet, CurInst.R1] :=
        FRegisters[PSW.RegisterSet, CurInst.R1] shr count;
end;

procedure TCpu.SSK;
begin
    Core.StoreStorageKey(FRegisters[PSW.RegisterSet, CurInst.R2],
                         FRegisters[PSW.RegisterSet, CurInst.R1]);
end;

procedure TCpu.SSM;
var
    b: Byte;
begin
    b := Core.FetchByte(PSW.Key, GetAbsAddress(CurInst.B1, CurInst.Off1));
    PSW.TimerIntEnabled := ((b and $80) <> 0);
    PSW.IOSTIntEnabled := ((b and $40) <> 0);
end;

procedure TCpu.SSTM;
// Supervisor load multiple. No docs for this. I'm guessing that
// I am supposed to save the program register set.
var
    addr: TMemoryAddress;
    r: Integer;
    done: Boolean;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    r := CurInst.R1;
    done := False;
    while (not done) do
    begin
        Core.StoreWord(PSW.Key, addr, FRegisters[rsProgram, r]);
        done := (r = CurInst.R2);
        Inc(addr, 4);
        Inc(r);
        if (r > 15) then
            r := 0;
    end;
end;

procedure TCpu.ST;
begin
    Core.StoreWord(PSW.Key,
                   GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1),
                   FRegisters[PSW.RegisterSet, CurInst.R1]);
end;

procedure TCpu.STC;
var
    addr: TMemoryAddress;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1);
    Core.StoreByte(PSW.Key, addr, FRegisters[PSW.RegisterSet, CurInst.R1] and $ff);
end;

procedure TCpu.Step(enable: Boolean);
begin
    if (enable) then
        FState := FState + [psSingleStep]
    else
        FState := FState - [psSingleStep];
end;

procedure TCpu.STH;
begin
    Core.StoreHalfWord(PSW.Key,
                       GetAbsAddress(CurInst.B1, CurInst.X1, CurInst.Off1),
                       FRegisters[PSW.RegisterSet, CurInst.R1]);
end;

procedure TCpu.STM;
var
    addr: TMemoryAddress;
    r: Integer;
    done: Boolean;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    r := CurInst.R1;
    done := False;
    while (not done) do
    begin
        Core.StoreWord(PSW.Key, addr, FRegisters[PSW.RegisterSet, r]);
        done := (r = CurInst.R2);
        Inc(addr, 4);
        Inc(r);
        if (r > 15) then
            r := 0;
    end;
end;

procedure TCpu.Stop;
begin
    FState := [psHalted];
end;

procedure TCpu.STR;
// Service Timer Register. Not documented. Implementation based on analysis of
// OS/3 dis-assembly.
var
    value: TWord;
    func: Byte;
begin
    value := FRegisters[PSW.RegisterSet, CurInst.R1];
    func := CurInst.R2;
    case func of
      0:                                // retrieve current value of timer
      begin
        FRegisters[PSW.RegisterSet, CurInst.R1] := FTimerReg;
        // Extend sign if timer negative
        if ((FTimerReg and $800000) <> 0) then
            FRegisters[PSW.RegisterSet, CurInst.R1] := FRegisters[PSW.RegisterSet, CurInst.R1] or $ff000000;
      end;
      1:                                // no clue what this is supposed to do
      begin
        { TODO : Need to figure out what this does }
      end;
      4:                                // set new timer value and enable timer
      begin
        FTimerReg := value and $ffffff;
        FTimerEnabled := True;
      end;
      else
      begin
        raise EProgramException.Create(Format('STR function %d not implemented', [func]));
      end;
    end;
end;

procedure TCpu.SumSetCondCode(op1, op2: THalfWord; var sum: THalfWord);
// Return the sum of two half words, checking for overflow and
// setting the condition code appropriately.
var
    ovfl: Boolean;
begin
    sum := op1 + op2;
    ovfl := (((op1 > 0) and (op2 > 0) and (sum < 0)) or
             ((op1 < 0) and (op2 < 0) and (sum > 0)));
    if (ovfl) then
        PSW.CondCode := 3
    else if (sum = 0) then
        PSW.CondCode := 0
    else if (sum < 0) then
        PSW.CondCode := 1
    else
        PSW.CondCode := 2;
end;

procedure TCpu.SumSetCondCode(op1, op2: TWord; var sum: TWord);
// Return the sum of two words, checking for overflow and
// setting the condition code appropriately.
var
    ovfl: Boolean;
begin
    sum := op1 + op2;
    ovfl := (((op1 > 0) and (op2 > 0) and (sum < 0)) or
             ((op1 < 0) and (op2 < 0) and (sum > 0)));
    if (ovfl) then
        PSW.CondCode := 3
    else if (sum = 0) then
        PSW.CondCode := 0
    else if (sum < 0) then
        PSW.CondCode := 1
    else
        PSW.CondCode := 2;
end;

procedure TCpu.SumSetCondCode(op1, op2: TDblWord; var sum: TDblWord);
// Return the sum of two double words, checking for overflow and
// setting the condition code appropriately.
var
    ovfl: Boolean;
begin
    sum := op1 + op2;
    ovfl := (((op1 > 0) and (op2 > 0) and (sum < 0)) or
             ((op1 < 0) and (op2 < 0) and (sum > 0)));
    if (ovfl) then
        PSW.CondCode := 3
    else if (sum = 0) then
        PSW.CondCode := 0
    else if (sum < 0) then
        PSW.CondCode := 1
    else
        PSW.CondCode := 2;
end;

procedure TCpu.SVC;
begin
    PSW.IntCode := CurInst.ImmedOperand;
    PSW.InstAddr := PSW.InstAddr - FRelocateReg;
    Core.StoreDblWord(0, SVC_OLD, PSW.AsDblWord);
    PSW.AsDblWord := Core.FetchDblWord(0, SVC_NEW);
    FRelocateReg := Core.FetchRelReg(PSW.Key);
    PSW.InstAddr := PSW.InstAddr + FRelocateReg;
end;

procedure TCpu.TM;
var
    mask: Byte;
    addr: TMemoryAddress;
    rslt: Byte;
begin
    mask := CurInst.ImmedOperand;
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    rslt := mask and Core.FetchByte(PSW.Key, addr);
    if (rslt = 0) then
        PSW.CondCode := 0
    else if (rslt = mask) then
        PSW.CondCode := 3
    else
        PSW.CondCode := 1;
end;

procedure TCpu.XC;
var
    len: Integer;
    op1, op2: TMemoryAddress;
    rslt: Byte;
begin
    len := CurInst.Length;
    op1 := GetAbsAddress(CurInst.B1, CurInst.Off1);
    op2 := GetAbsAddress(CurInst.B2, CurInst.Off2);
    PSW.CondCode := 0;
    while (len >= 0) do
    begin
        rslt := Core.FetchByte(PSW.Key, op1) xor Core.FetchByte(PSW.Key, op2);
        Core.StoreByte(PSW.Key, op1, rslt);
        if (rslt <> 0) then
            PSW.CondCode := 1;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
end;

procedure TCpu.XI;
var
    addr: TMemoryAddress;
    b: Byte;
begin
    addr := GetAbsAddress(CurInst.B1, CurInst.Off1);
    b := Core.FetchByte(PSW.Key, addr) xor CurInst.ImmedOperand;
    Core.StoreByte(PSW.Key, addr, b);
    if (b = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

procedure TCpu.XR;
var
    b: Byte;
begin
    b := FRegisters[PSW.RegisterSet, CurInst.R1] xor
         FRegisters[PSW.RegisterSet, CurInst.R2];
    FRegisters[PSW.RegisterSet, CurInst.R1] := b;
    if (b = 0) then
        PSW.CondCode := 0
    else
        PSW.CondCode := 1;
end;

constructor TIOST.Create;
begin
    FLock := TCriticalSection.Create;
end;

procedure TIOST.DecPendingCount;
begin
    FLock.Acquire;
    try
        Dec(FPendingCount);
        if (FPendingCount < 0) then
            FPendingCount := 0;
    finally
        FLock.Release;
    end;
end;

destructor TIOST.Destroy;
begin
    FreeAndNil(FLock);
    inherited Destroy;
end;

procedure TIOST.IntRequest;
begin
    FLock.Acquire;
    try
        Inc(FPendingCount);
    finally
        FLock.Release;
    end;
end;

procedure TIOST.Resume;
begin
    if (FSuspendedOffset > 0) then
        // Status partially tabled and Cpu notified.
        // Just clear supsended flag.
        FSuspended := False
    else
        // Resume attempt to table last received status.
        TableInterrupt(True);
end;

procedure TIOST.TableInterrupt(resume: Boolean);
var
    j, last: Integer;
    iw, cont: TWord;
    key: Byte;
    addr: TMemoryAddress;
begin
    j := FSuspendedOffset;
    try
        // Copy status to I/O status table
        key := TIOSTCW.Key;
        cont := 0;                                  // set continued bit if more than 1 word
        if (FStatLength > 1) then
            cont := $40000000;
        last := (FStatLength - 1) * 4;              // start of last status word
        while (j < (FStatLength * 4)) do
        begin
            if (j >= last) then                     // clear c bit for last word
                cont := 0;
            addr := TIOSTCW.Address;
            iw := Core.FetchWord(key, addr);
            if (iw >= 0) then
            begin
                // Prior interrupt not processed. Suspend the IOST
                FSuspended := True;
                FSuspendedOffset := j;
                TIOSTCW.Channel := FStatChannel;
                TIOSTCW.Flags := IOSTSW_STF;        // set status table full
                Exit;
            end;
            iw := Core.FetchWord(0, BCSW0 + j);
            iw := (iw and $7fffffff) or cont;       // clear v bit and set c bit
            Core.StoreWord(key, addr, iw);
            TIOSTCW.NextWord;
            FIntPending := True;
            Inc(j, 4);
        end;
        TIOSTCW.Channel := 0;
        TIOSTCW.Flags := 0;
        DecPendingCount;
        FSuspended := False;
    except
      on E: EAddressException do
      begin
        FSuspended := True;
        FSuspendedOffset := j;
        TIOSTCW.Channel := FStatChannel;
        TIOSTCW.Flags := IOSTSW_IWE;               // set interrupt word error
      end;
    end;
end;

procedure TIOST.ProcessInterrupts;
var
    i: Integer;
    stat: TStatus;
begin
    if (FSuspended) then
        Exit;

    for i := 0 to MAX_CHANNEL do
    begin
        if (Assigned(Adapters[i])) then
        begin
            while ((not FSuspended) and Adapters.Channel[i].IntPending) do
            begin
                // Copy status to BCSW
                stat := Adapters.Channel[i].GetStatus;
                Core.Copy(@stat.Status, BCSW0, 16);
                FStatLength := stat.Length;
                FStatChannel := stat.Status[0] and $7;
                TableInterrupt(False);
            end;
        end;
    end;
end;

{ TPSW }

function TPSW.GetAsDblWord: TDblWord;
begin
    Result := 0;
    if (FTimerIntEnabled) then
        Result := Result or $8000000000000000;
    if (FIOSTIntEnabled) then
        Result := Result or $4000000000000000;
    Result := Result or (Int64(FKey) shl 52);
    if (FAscii) then
        Result := Result or $8000000000000;
    Result := Result or (Int64(FRegisterSet) shl 50);
    Result := Result or (Int64(FMode) shl 49);
    Result := Result or (Int64(FEmulation) shl 45);
    if (FMonitorMode) then
        Result := Result or $100000000000;
    Result := Result or (Int64(FIntCode) shl 32);
    Result := Result or (Int64(FInstLength) shl 30);
    Result := Result or (Int64(FCondCode) shl 28);
    if (FFixedOvflExcp) then
        Result := Result or $8000000;
    if (FDecOvflExcp) then
        Result := Result or $4000000;
    if (FCharacteristicOvflExcp) then
        Result := Result or $2000000;
    if (FSignificantExcp) then
        Result := Result or $1000000;
    Result := Result or (FInstAddr);
end;

procedure TPSW.SetAscii(const Value: Boolean);
begin
    FAscii := Value;
end;

procedure TPSW.SetAsDblWord(const Value: TDblWord);
begin
    FTimerIntEnabled := (Value and $8000000000000000) <> 0;
    FIOSTIntEnabled := (Value and $4000000000000000) <> 0;
    FKey := (Value shr 52) and $f;
    FAscii := (Value and $8000000000000) <> 0;
    FRegisterSet := TRegisterSet((Value shr 50) and $1);
    FMode := TProcessorMode((Value shr 49) and $1);
    FEmulation := TProcessorEmulation((Value shr 45) and $7);
    FMonitorMode := (Value and $100000000000) <> 0;
    FIntCode := (Value shr 32) and $ff;
    FInstLength := (Value shr 30) and $3;
    FCondCode := (Value shr 28) and $3;
    FFixedOvflExcp := (Value and $8000000) <> 0;
    FDecOvflExcp := (Value and $4000000) <> 0;
    FCharacteristicOvflExcp := (Value and $2000000) <> 0;
    FSignificantExcp := (Value and $1000000) <> 0;
    FInstAddr := Value and $ffffff;
end;

procedure TPSW.SetCharacteristicOvflExcp(const Value: Boolean);
begin
    FCharacteristicOvflExcp := Value;
end;

procedure TPSW.SetCondCode(const Value: Byte);
begin
    FCondCode := Value;
end;

procedure TPSW.SetDecOvflExcp(const Value: Boolean);
begin
    FDecOvflExcp := Value;
end;

procedure TPSW.SetEmulation(const Value: TProcessorEmulation);
begin
    FEmulation := Value;
end;

procedure TPSW.SetFixedOvflExcp(const Value: Boolean);
begin
    FFixedOvflExcp := Value;
end;

procedure TPSW.SetInstAddr(const Value: TMemoryAddress);
begin
    FInstAddr := Value and $ffffff;                 // wrap after 24 bits
end;

procedure TPSW.SetInstLength(const Value: Byte);
begin
    FInstLength := Value;
end;

procedure TPSW.SetIntCode(const Value: Byte);
begin
    FIntCode := Value;
end;

procedure TPSW.SetIOSTIntEnabled(const Value: Boolean);
begin
    FIOSTIntEnabled := Value;
end;

procedure TPSW.SetKey(const Value: Byte);
begin
    FKey := Value;
end;

procedure TPSW.SetMode(const Value: TProcessorMode);
begin
    FMode := Value;
end;

procedure TPSW.SetMonitorMode(const Value: Boolean);
begin
    FMonitorMode := Value;
end;

procedure TPSW.SetRegisterSet(const Value: TRegisterSet);
begin
    FRegisterSet := Value;
end;

procedure TPSW.SetSignificantExcp(const Value: Boolean);
begin
    FSignificantExcp := Value;
end;

procedure TPSW.SetTimerIntEnabled(const Value: Boolean);
begin
    FTimerIntEnabled := Value;
end;

{ TInstruction }

function TInstruction.B1: Byte;
begin
    Result := FInst[2] shr 4;
end;

function TInstruction.B2: Byte;
begin
    Result := FInst[4] shr 4;
end;

function TInstruction.BranchMask: Byte;
begin
    Result := FInst[1] and $f0;
end;

procedure TInstruction.Clear;
begin
    FillChar(FInst, 6, 0);
end;

procedure TInstruction.Fetch;
var
    ilc: Byte;
    len, i: Integer;
    hw: THalfWord;
begin
    FillChar(FInst, 6, 0);
    // Determine instruction length based of first 2 bits of opcode
    ilc := Core.FetchByte(PSW.Key, PSW.InstAddr) and $c0;
    case ilc of
      0:        len := 1;
      $80, $40: len := 2;
      else      len := 3;
    end;
    PSW.InstLength := len;
    i := 0;
    while (len > 0) do
    begin
        hw := Core.FetchHalfWord(PSW.Key, PSW.InstAddr);
        FInst[i] := hw shr 8;
        Inc(i);
        FInst[i] := hw and $ff;
        Inc(i);
        PSW.InstAddr := PSW.InstAddr + 2;
        Dec(len);
    end;
end;

function TInstruction.GetAsDblWord: TDblWord;
var
    i: Integer;
begin
    Result := 0;
    for i := 0 to 5 do
        Result := (Result shl 8) or FInst[i];
end;

function TInstruction.GetByte(idx: Integer): Byte;
begin
    Result := FInst[idx];
end;

function TInstruction.ImmedOperand: Byte;
begin
    Result := FInst[1];
end;

function TInstruction.Length: Byte;
begin
    Result := FInst[1];
end;

function TInstruction.Length1: Byte;
begin
    Result := (Length and $f0) shr 4;
end;

function TInstruction.Length2: Byte;
begin
    Result := Length and $0f;
end;

function TInstruction.Off1: UInt16;
begin
    Result := (UInt16(FInst[2] and $f) shl 8) or FInst[3];
end;

function TInstruction.Off2: UInt16;
begin
    Result := (UInt16(FInst[4] and $f) shl 8) or FInst[5];
end;

function TInstruction.R1: Byte;
begin
    Result := (FInst[1] and $f0) shr 4;
end;

function TInstruction.R2: Byte;
begin
    Result := FInst[1] and $0f;
end;

procedure TInstruction.SetByte(idx: Integer; const Value: Byte);
begin
    FInst[idx] := Value;
end;

function TInstruction.X1: Byte;
begin
    Result := FInst[1] and $0f;
end;

end.
