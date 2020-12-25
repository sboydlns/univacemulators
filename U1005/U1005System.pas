unit U1005System;

interface

uses Windows, SysUtils, ExtCtrls, Forms, U1005Types, U1005Memory, U1005Reader, U1005Printer, U1005Punch,
     U1005CPU;

type
  T1005System = class(TObject)
  private
    FSystemType: T1005SystemType;
    FMemory: T1005Memory;
    FState: T1005State;
    FReader: T1005Reader;
    FPrinter: T1005Printer;
    FPunch: T1005Punch;
    FCPU: T1005CPU;
    FAlt1: Boolean;
    FAlt2: Boolean;
    FAlt3: Boolean;
    FAlt4: Boolean;
    FDisplay4: Cardinal;
    FDisplay6: Cardinal;
    FDisplay8: Cardinal;
    FDisplay9: Cardinal;
    FRefreshTimer: TTimer;
    procedure DoTimer(Sender: TObject);
    procedure DoExecuteInstruction(Sender: TObject);
    procedure MemTest;
    procedure SetAlt1(const Value: Boolean);
    procedure SetAlt2(const Value: Boolean);
    procedure SetAlt3(const Value: Boolean);
    procedure SetAlt4(const Value: Boolean);
    procedure SetSystemType(const Value: T1005SystemType);
  public
    constructor Create(st: T1005SystemType);
    destructor Destroy; override;
    procedure Clear;
    procedure Load;
    procedure PowerOff;
    procedure PowerOn;
    procedure ResetD4(val: Cardinal);
    procedure SetD4(val: Cardinal);
    procedure ResetD9(val: Cardinal);
    procedure SetD9(val: Cardinal);
    procedure Start;
    property Alt1: Boolean read FAlt1 write SetAlt1;
    property Alt2: Boolean read FAlt2 write SetAlt2;
    property Alt3: Boolean read FAlt3 write SetAlt3;
    property Alt4: Boolean read FAlt4 write SetAlt4;
    property CPU: T1005CPU read FCPU;
    property Display4: Cardinal read FDisplay4;
    property Display6: Cardinal read FDisplay6;
    property Display8: Cardinal read FDisplay8;
    property Display9: Cardinal read FDisplay9;
    property Printer: T1005Printer read FPrinter;
    property Punch: T1005Punch read FPunch;
    property Reader: T1005Reader read FReader;
    property SystemType: T1005SystemType read FSystemType write SetSystemType;
  end;

const
  D4_HOPPER = $80000000;
  D4_FEED = $40000000;
  D4_READ_JAM = $20000000;
  D4_TRANSPORT_JAM = $10000000;
  D4_STACKER = $08000000;
  D4_FORM = $04000000;
  D4_ADVANCE = $02000000;
  D4_PUNCH = $01000000;
  D4_HALT = $00800000;
  D4_INDICATOR1 = $00400000;
  D4_INDICATOR2 = $00200000;
  D4_INDICATOR3 = $00100000;
  D4_INDICATOR4 = $00080000;
  //
  D9_IC1X = $00008000;
  D9_IC2X = $00004000;
  D9_IC3X = $00002000;
  D9_IC4X = $00001000;
  D9_IC5X = $00000800;

implementation

uses EmulatorTypes, CardFile;

{ T1005System }

procedure T1005System.Clear;
var
    holdState: T1005State;
begin
    holdState := FState;
    try
        FState := [usInitializing];
        FReader.Clear;
        FPunch.Clear;
        FPrinter.Clear;
        FCPU.Clear;
        FDisplay4 := 0;
        FDisplay6 := 0;
        FDisplay8 := 0;
        FDisplay9 := 0;
    finally
        FState := holdState;
    end;
end;

constructor T1005System.Create(st: T1005SystemType);
begin
    inherited Create;
    FSystemType := st;
    FState := [usPowerOff];
    FMemory := T1005Memory.Create(st);
    FReader := T1005Reader.Create(FMemory);
    FPrinter := T1005Printer.Create(FMemory);
    FPunch := T1005Punch.Create(FMemory);
    if (FSystemType = stFedSys) then
        FCPU := T1005FedSysCPU.Create(FMemory, FReader, FPrinter, FPunch)
    else
        FCPU := T1005CommCPU.Create(FMemory, FReader, FPrinter, FPunch);
    FCPU.OnExecuteInstruction := DoExecuteInstruction;
    FRefreshTimer := TTimer.Create(nil);
    FRefreshTimer.Interval := 50;
    FRefreshTimer.Enabled := True;
    FRefreshTimer.OnTimer := DoTimer;
end;

destructor T1005System.Destroy;
begin
    FreeAndNil(FMemory);
    FreeAndNil(FReader);
    FreeAndNil(FPrinter);
    FreeAndNil(FPunch);
    FreeAndNil(FCPU);
    FreeAndNil(FRefreshTimer);
    inherited;
end;

procedure T1005System.DoExecuteInstruction(Sender: TObject);
begin
    Application.ProcessMessages;
//    Sleep(1000);
end;

procedure T1005System.DoTimer(Sender: TObject);
const
    // Table equating opcodes to display mask 6 bits
    opcodeBits:array [0..63] of Cardinal = (
        $80000000,  // LA1
        $40000000,  // LD1
        $00100000,  // IC
        $20000000,  // LPR
        $00800000,  // CLR
        $00080000,  // J
        $00000010,  // LN1
        $10000000,  // SA1
        $00400000,  // CA1
        0,
        $00040000,  // JL
        $00010000,  // JE
        0,
        $00000200,  // MUL
        $08000000,  // SD1
        $00004000,  // JX
        $00000002,  // XFC
        $00020000,  // CN1
        $01000000,  // SHL
        $00000020,  // LWS
        $00000080,  // TRL
        $00020000,  // JG
        $00000400,  // SR1
        $00008000,  // JR
        $00000004,  // PTE
        $02000000,  // SHR
        $00000100,  // DIV
        $00000800,  // SM1
        $04000000,  // SPR
        $00001000,  // AR1
        $00002000,  // AM1
        $00000001,  // XF
        $80000000,  // LA2
        $40000000,  // LD2
        0,
        0,
        0,
        0,
        $00000010,  // LN2
        $10000000,  // SA2
        $00400000,  // CA2
        $00000040,  // SZS
        0,
        0,
        $00000008,  // SED
        0,
        $08000000,  // SD2
        0,
        0,
        $00200000,  //CN2
        0,
        0,
        0,
        0,
        $00000400,  // SR2
        0,
        $00000004,  // LS3
        0,
        0,
        $00000800,  // SM2
        0,
        $00001000,  // AR2
        $00002000,  // AM2
        0
    );
var
    opcode: Byte;
    op: T1005FedSysOperand;
begin
    if (not (usPowerOn in FState)) then
        Exit;

    if (FReader.HopperEmpty) then
        SetD4(D4_HOPPER)
    else
        ResetD4(D4_HOPPER);
    if (FReader.ReadStationLoaded) then
        ResetD4(D4_FEED)
    else
        SetD4(D4_FEED);
    if (FPunch.HopperEmpty) then
        SetD4(D4_PUNCH)
    else
        ResetD4(D4_PUNCH);
    if ((ucsHalted in FCPU.State) or (ucsSingleStep in FCPU.State)) then
        SetD4(D4_HALT)
    else
        ResetD4(D4_HALT);
    ResetD4(CC_INDICATOR1 or CC_INDICATOR2);
    if ((FCPU.ConditionCodes and CC_INDICATOR1) <> 0) then
    begin
        if ((FCPU.ConditionCodes and CC_INDICATOR2) <> 0) then
            SetD4(D4_INDICATOR1 or D4_INDICATOR2)
        else
            SetD4(D4_INDICATOR1);
    end else if ((FCPU.ConditionCodes and CC_INDICATOR2) <> 0) then
    begin
        SetD4(D4_INDICATOR2);
    end;
    //
    if (SystemType = stFedSys) then
    begin
        opcode := FMemory.FetchFedSysOpcode;
        if ((opcode >= Low(opcodeBits)) and (opcode <= High(opcodeBits))) then
            FDisplay6 := opcodeBits[opcode];
    end;
    //
    if (SystemType = stFedSys) then
    begin
        op := FMemory.FetchFedSysOperand;
        FDisplay8 := ((op.M.Row and $1f) shl 27) or
                     ((op.M.Col and $1f) shl 22) or
                     ((op.L.Row and $1f) shl 17) or
                     ((op.L.Col and $1f) shl 12);
    end;
    //
    if (SystemType = stFedSys) then
    begin
        if ((FMemory.FetchFedSysOpcode and $20) <> 0) then
            SetD9(D9_IC1X)
        else
            ResetD9(D9_IC1X);
        if ((op.M.Row and $20) <> 0) then
            SetD9(D9_IC2X)
        else
            ResetD9(D9_IC2X);
        if ((op.M.Col and $20) <> 0) then
            SetD9(D9_IC3X)
        else
            ResetD9(D9_IC3X);
        if ((op.L.Row and $20) <> 0) then
            SetD9(D9_IC4X)
        else
            ResetD9(D9_IC4X);
        if ((op.L.Col and $20) <> 0) then
            SetD9(D9_IC5X)
        else
            ResetD9(D9_IC5X);
    end;
end;

procedure T1005System.Load;
var
    bfr: TCardRec;
    done: Boolean;

    procedure LoadObject;
    var
        op: T1005FedSysOperand;
        col: Integer;
    begin
        op := FedSysOperand;
        op.M.SetAddr(bfr.Columns[77], bfr.Columns[78]);
        op.L.SetAddr(bfr.Columns[79], bfr.Columns[80]);
        col := 1;
        while ((op.M.GetSequentialAddr <= op.L.GetSequentialAddr) and (col <= 62)) do
        begin
            FMemory.StoreByte(op.M, bfr.Columns[col]);
            op.M.Increment;
            Inc(col);
        end;
    end;

    procedure SetXfer;
    var
        addr: I1005Addr;
    begin
        addr := T1005FedSysAddr.Create;
        addr.SetAddr(bfr.Columns[77], bfr.Columns[78]);
        FMemory.StorePAK(addr);
        done := True;
    end;

begin
    done := False;
    while (not done) do
    begin
        done := (not FReader.Read(bfr));
        if (not done) then
        begin
            case bfr.Columns[74] of
              X3_RIGHT_SQUARE:  LoadObject;
              X3_SLASH:         SetXfer;
            end;
        end;
    end;
end;

procedure T1005System.MemTest;
var
    bank, row, col: Byte;
    addr: I1005Addr;
begin
    addr := T1005FedSysAddr.Create;
    for bank := 1 to 4 do
        for row := 1 to 32 do
            for col := 1 to 32 do
            begin
                addr.SetAddr(bank, row, col);
                FMemory.StoreByte(addr, col);
            end;
end;

procedure T1005System.PowerOff;
begin
    if (usPowerOff in FState) then
        Exit;
    FState := [usPowerOff];
end;

procedure T1005System.PowerOn;
begin
    if (FState * [usPowerOn, usInitializing] <> []) then
        Exit;
    // Do a system reset
    Clear;
    // Let everyone know that we are up
    FState := [usPowerOn];
    // Check status of various periperals
    if (FReader.HopperEmpty) then
        SetD4(D4_HOPPER);
    if (not FReader.ReadStationLoaded) then
        SetD4(D4_FEED);
    //
//    MemTest;
end;

procedure T1005System.ResetD4(val: Cardinal);
begin
    FDisplay4 := FDisplay4 and (not val);
end;

procedure T1005System.ResetD9(val: Cardinal);
begin
    FDisplay9 := FDisplay9 and (not val);
end;

procedure T1005System.SetAlt1(const Value: Boolean);
begin
    FAlt1 := Value;
    FCPU.Alt1 := Value;
end;

procedure T1005System.SetAlt2(const Value: Boolean);
begin
    FAlt2 := Value;
    FCPU.Alt2 := Value;
end;

procedure T1005System.SetAlt3(const Value: Boolean);
begin
    FAlt3 := Value;
    FCPU.Alt3 := Value;
end;

procedure T1005System.SetAlt4(const Value: Boolean);
begin
    FAlt4 := Value;
    FCPU.Alt4 := Value;
end;

procedure T1005System.SetD4(val: Cardinal);
begin
    FDisplay4 := FDisplay4 or val;
end;

procedure T1005System.SetD9(val: Cardinal);
begin
    FDisplay9 := FDisplay9 or val;
end;

procedure T1005System.SetSystemType(const Value: T1005SystemType);
begin
    if (Value <> FSystemType) then
    begin
        FreeAndNil(FCPU);
        FreeAndNil(FMemory);
        FMemory := T1005Memory.Create(Value);
        if (Value = stFedSys) then
            FCPU := T1005FedSysCpu.Create(FMemory, FReader, FPrinter, FPunch)
        else
            FCPU := T1005CommCpu.Create(FMemory, FReader, FPrinter, FPunch);
        FSystemType := Value;
    end;
end;

procedure T1005System.Start;
begin
    FCPU.Start;
end;

end.
