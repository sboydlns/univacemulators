unit U1005CPU;

interface

uses SysUtils, U1005Types, U1005Memory, U1005Reader, U1005Printer, U1005Punch;

type
  T1005IOFlipFlop = ( ffRead, ffPunchRead, ffPrint, ffStackerSelect );
  T1005IOFlipFlops = set of T1005IOFlipFlop;

  T1005CPU = class(TObject)
  private
    function GetSingleStep: Boolean;
    procedure SetSingleStep(const Value: Boolean);
  protected
    FMemory: T1005Memory;
    FReader: T1005Reader;
    FPrinter: T1005Printer;
    FPunch: T1005Punch;
    FState: T1005CPUState;
    FOnDebug: TDebugEvent;
    FOnExecuteInstruction : TExecuteInstructionEvent;
    FOnError: TErrorEvent;
    FOnFetchInstruction: TFetchInstructionEvent;
    FOpcodes: T1005OpcodeList;
    FCrntOpcode: T1005Opcode;
    FConditionCodes: Cardinal;
    FAlt1: Boolean;
    FAlt2: Boolean;
    FAlt3: Boolean;
    FAlt4: Boolean;
    procedure SetAlt1(Value: Boolean); virtual;
    procedure SetAlt2(Value: Boolean); virtual;
  public
    constructor Create(mem: T1005Memory;
                       rdr: T1005Reader;
                       prt: T1005Printer;
                       pch: T1005Punch); virtual;
    procedure Clear; virtual; abstract;
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;
    property Alt1: Boolean read FAlt1 write SetAlt1;
    property Alt2: Boolean read FAlt2 write SetAlt2;
    property Alt3: Boolean read FAlt3 write FAlt3;
    property Alt4: Boolean read FAlt4 write FAlt4;
    property ConditionCodes: Cardinal read FConditionCodes;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnExecuteInstruction: TExecuteInstructionEvent read FOnExecuteInstruction write FOnExecuteInstruction;
    property OnFetchInstruction: TFetchInstructionEvent read FOnFetchInstruction write FOnFetchInstruction;
    property SingleStep: Boolean read GetSingleStep write SetSingleStep;
    property State: T1005CPUState read FState;
  end;

  T1005FedSysCPU = class(T1005CPU)
  private
    FCrntOperand: T1005FedSysOperand;
    procedure AM1;
    procedure AM2;
    procedure AR1;
    procedure AR2;
    procedure CA1;
    procedure CA2;
    procedure CLR;
    procedure CN1;
    procedure CN2;
    procedure DIVIDE;
    procedure ExecuteInstruction;
    procedure FetchInstruction;
    procedure IC;
    procedure J;
    procedure J0;
    procedure J1;
    procedure J2;
    procedure JR;
    procedure JX;
    procedure LA1;
    procedure LA2;
    procedure LD1;
    procedure LD2;
    procedure LN1;
    procedure LN2;
    procedure LPR;
    procedure LWS;
    procedure MemClear(const dest: I1005Addr; len: Integer);
    procedure MemCpy(const dest, src: I1005Addr; len: Integer);
    procedure MUL;
    procedure PrintSpace(count: Integer);
    procedure Punch(stacker: Cardinal; image: Cardinal);
    procedure PunchPaperTape(count: Integer);
    procedure Read(image: Cardinal);
    procedure ReadAux(stacker: Cardinal);
    procedure ReadPaperTape(count: Integer);
    procedure ReadPunch(stacker: Cardinal; image: Cardinal);
    procedure SA1;
    procedure SA2;
    procedure SD1;
    procedure SD2;
    procedure SED;
    procedure SHIFTL;
    procedure SHIFTR;
    procedure Skip(chan: Cardinal);
    procedure SPR;
    procedure SM1;
    procedure SM2;
    procedure SR1;
    procedure SR2;
    procedure SZS;
    procedure XFC;
  public
    constructor Create(mem: T1005Memory;
                       rdr: T1005Reader;
                       prt: T1005Printer;
                       pch: T1005Punch); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Start; override;
    procedure Stop; override;
  end;

  T1005CommCPU = class(T1005CPU)
  private
    FIOFlipFlops: T1005IOFlipFlops;
    procedure ExecuteInstruction;
    procedure FetchInstruction;
    procedure AD;
    procedure AK;
    procedure AM;
    procedure BT;
    procedure CA;
    procedure CC;
    procedure CK;
    procedure CM;
    procedure CN;
    procedure DV;
    procedure ED;
    procedure EL;
    procedure GC;
    procedure J;
    procedure JC;
    procedure JK;
    procedure JL;
    procedure JR;
    procedure JT;
    procedure ML;
    procedure MU;
    procedure SC;
    procedure SM;
    procedure SU;
    procedure TA;
    procedure TC;
    procedure TD;
    procedure TK;
    procedure TN;
    procedure TR;
    procedure TX;
  protected
    procedure SetAlt1(Value: Boolean); override;
    procedure SetAlt2(Value: Boolean); override;
  public
    constructor Create(mem: T1005Memory;
                       rdr: T1005Reader;
                       prt: T1005Printer;
                       pch: T1005Punch); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure InitBoot;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

uses Forms, Math, FmtBcd, Bcd, EmulatorTypes;

{ T1005CPU }

procedure T1005FedSysCPU.AM1;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR1Bcd(len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreBcd(op.M, len, rslt);
end;

procedure T1005FedSysCPU.AM2;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR2Bcd(len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreBcd(op.M, len, rslt);
end;

procedure T1005FedSysCPU.AR1;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR1Bcd(len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreAR1Bcd(rslt);
end;

procedure T1005FedSysCPU.AR2;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR2Bcd(len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreAR2Bcd(rslt);
end;

procedure T1005FedSysCPU.CA1;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 10);
    i := 10 - len + 1;
    while (len > 0) do
    begin
        if (FMemory.FetchByte(op.M) <> FMemory.FetchAR1(i)) then
        begin
            FMemory.StoreIndicator(IND_NOT_EQUAL);
            Exit;
        end;
        op.M.Increment;
        Dec(len);
        Inc(i);
    end;
    FMemory.StoreIndicator(IND_EQUAL);
end;

procedure T1005FedSysCPU.CA2;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 21);
    i := 21 - len + 1;
    while (len > 0) do
    begin
        if (FMemory.FetchByte(op.M) <> FMemory.FetchAR2(i)) then
        begin
            FMemory.StoreIndicator(IND_NOT_EQUAL);
            Exit;
        end;
        op.M.Increment;
        Dec(len);
        Inc(i);
    end;
    FMemory.StoreIndicator(IND_EQUAL);
end;

procedure T1005FedSysCPU.Clear;
begin
    FState := [ucsHalted];
end;

procedure T1005FedSysCPU.CLR;
var
    len: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    MemClear(op.M, len);
end;

procedure T1005FedSysCPU.CN1;
var
    len: Integer;
    test: Integer;
    op: T1005FedSysOperand;
    op1, op2: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - Op.M.SequentialAddr + 1;
    op1 := FMemory.FetchAR1Bcd(len);
    op2 := FMemory.FetchBcd(op.M, len);
    test := BcdCompare(op1, op2);
    if (test < 0) then
        FMemory.StoreIndicator(IND_LESS)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_GREATER)
    else
        FMemory.StoreIndicator(IND_EQUAL);
end;

procedure T1005FedSysCPU.CN2;
var
    len: Integer;
    test: Integer;
    op: T1005FedSysOperand;
    op1, op2: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - Op.M.SequentialAddr + 1;
    op1 := FMemory.FetchAR2Bcd(len);
    op2 := FMemory.FetchBcd(op.M, len);
    test := BcdCompare(op1, op2);
    if (test < 0) then
        FMemory.StoreIndicator(IND_LESS)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_GREATER)
    else
        FMemory.StoreIndicator(IND_EQUAL);
end;

constructor T1005FedSysCPU.Create(mem: T1005Memory; rdr: T1005Reader; prt: T1005Printer; pch: T1005Punch);
begin
    inherited;
    FCrntOperand := FedSysOperand;
    FOpcodes := T1005OpcodeList.Create;
    FOpcodes.AddOpcode(OpCode(X3_SPACE, 'LA1', itClass1, False, LA1));
    FOpcodes.AddOpcode(OpCode(X3_APOS, 'LA2', itClass1, False, LA2));
    FOpcodes.AddOpcode(OpCode(X3_RIGHT_SQUARE, 'LD1', itClass1, False, LD1));
    FOpcodes.AddOpcode(OpCode(X3_ASTERIX, 'LD2', itClass1, False, LD2));
    FOpcodes.AddOpcode(OpCode(X3_0, 'LPR', itClass1, False, LPR));
    FOpcodes.AddOpcode(OpCode(X3_4, 'SA1', itClass1, False, SA1));
    FOpcodes.AddOpcode(OpCode(X3_M, 'SA2', itClass1, False, SA2));
    FOpcodes.AddOpcode(OpCode(X3_SEMI, 'SD1', itClass1, False, SD1));
    FOpcodes.AddOpcode(OpCode(X3_AT, 'SD2', itClass1, False, SD2));
    FOpcodes.AddOpcode(OpCode(X3_I, 'SPR', itClass1, False, SPR));
    FOpcodes.AddOpcode(OpCode(X3_F, 'SHR', itShift, False, SHIFTR));
    FOpcodes.AddOpcode(OpCode(X3_PERIOD, 'SHL', itShift, False, SHIFTL));
    FOpcodes.AddOpcode(OpCode(X3_1, 'CLR', itClass1, False, CLR));
    FOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 1, 'SC', itCharacter));
    FOpcodes.AddOpcode(OpCode(X3_5, 'CA1', itClass1, False, CA1));
    FOpcodes.AddOpcode(OpCode(X3_N, 'CA2', itClass1, False, CA2));
    FOpcodes.AddOpcode(OpCode(X3_COLON, 'CN1', itClass1, False, CN1));
    FOpcodes.AddOpcode(OpCode(X3_PERCENT, 'CN2', itClass1, False, CN2));
    FOpcodes.AddOpcode(OpCode(X3_MINUS, 'IC', itClass2, False, IC));
    FOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 2, 'CCA', itCharacter));
    FOpcodes.AddOpcode(OpCode(X3_2, 'J', itClass2, False, J));
    FOpcodes.AddOpcode(OpCode(X3_B, 'JG', itClass2, False, J2));
    FOpcodes.AddOpcode(OpCode(X3_8, 'JE', itClass2, False, J0));
    FOpcodes.AddOpcode(OpCode(X3_7, 'JU', itClass2, False, J1));
    FOpcodes.AddOpcode(OpCode(X3_D, 'JR', itClass2, False, JR));
    FOpcodes.AddOpcode(OpCode(X3_LEFT_SQUARE, 'JX', itClass2, False, JX));
    FOpcodes.AddOpcode(OpCode(X3_V, 1, 'JS3', itClass2));
    FOpcodes.AddOpcode(OpCode(X3_V, 2, 'JOF', itClass2));
    FOpcodes.AddOpcode(OpCode(X3_LESS, 'AM1', itClass1, False, AM1));
    FOpcodes.AddOpcode(OpCode(X3_GREATER, 'AM2', itClass1, False, AM2));
    FOpcodes.AddOpcode(OpCode(X3_SHARP, 'AR1', itClass1, False, AR1));
    FOpcodes.AddOpcode(OpCode(X3_LOZENGE, 'AR2', itClass1, False, AR2));
    FOpcodes.AddOpcode(OpCode(X3_H, 'SM1', itClass1, False, SM1));
    FOpcodes.AddOpcode(OpCode(X3_Y, 'SM2', itClass1, False, SM2));
    FOpcodes.AddOpcode(OpCode(X3_C, 'SR1', itClass1, False, SR1));
    FOpcodes.AddOpcode(OpCode(X3_T, 'SR2', itClass1, False, SR2));
    FOpcodes.AddOpcode(OpCode(X3_BACK_SLASH, 'MUL', itClass1, False, MUL));
    FOpcodes.AddOpcode(OpCode(X3_G, 'DIV', itClass1, False, DIVIDE));
    FOpcodes.AddOpcode(OpCode(X3_A, 'TRL', itClass1, False));
    FOpcodes.AddOpcode(OpCode(X3_O, 'SZS', itClass1, False, SZS));
    FOpcodes.AddOpcode(OpCode(X3_QUESTION, 'LWS', itClass1, False, LWS));
    FOpcodes.AddOpcode(OpCode(X3_3, 'LN1', itClass1, False, LN1));
    FOpcodes.AddOpcode(OpCode(X3_L, 'LN2', itClass1, False, LN2));
    FOpcodes.AddOpcode(OpCode(X3_R, 'SED', itClass1, False, SED));
    FOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 3, 'LAN', itCharacter));
    FOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 4, 'LOR', itCharacter));
    FOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 5, 'BSH', itCharacter));
    FOpcodes.AddOpcode(OpCode(X3_E, 'PTE'));
    FOpcodes.AddOpcode(OpCode(X3_AMP, 'XFC', itClass3, False, XFC));
    FOpcodes.AddOpcode(OpCode(X3_V, 3, 'JPE', itClass2));
    FOpcodes.AddOpcode(OpCode(X3_V, 4, 'JET', itClass1));
    FOpcodes.AddOpcode(OpCode(X3_V, 5, 'JI1', itClass1));
    FOpcodes.AddOpcode(OpCode(X3_V, 6, 'JAL', itClass1));
end;

destructor T1005FedSysCPU.Destroy;
begin
    FreeAndNil(FOpcodes);
    inherited;
end;

procedure T1005FedSysCPU.DIVIDE;
var
    op: T1005FedSysOperand;
    len: Integer;
    op1, op2, rslt, quotient, btemp, zero: TBcd;
begin
    zero := 0;
    op := FMemory.FetchFedSysOperand;
    len := Min(7, op.L.SequentialAddr - op.M.SequentialAddr + 1);
    op1 := FMemory.FetchBcd(op.M, len);
    op1.SignSpecialPlaces := op1.SignSpecialPlaces and $7f;     // clear negative flag
    op2 := FMemory.FetchAR2Bcd(13);
    op2.SignSpecialPlaces := op2.SignSpecialPlaces and $7f;     // clear negative flag
    if (op2 = 0) then
    begin
        // space fill result if div by zero
        for len := 1 to 10 do
            FMemory.StoreAR1(len, X3_SPACE);
        for len := 1 to 21 do
            FMemory.StoreAR2(len, X3_SPACE);
    end else
    begin
        rslt := op2 / op1;
        FMemory.StoreAR1Bcd(rslt);                              // save to quotient
        quotient := FMemory.FetchAR1Bcd(10);                    // get it back
        rslt := rslt - quotient;                                // isolate fractional part of result
        FMemory.StoreAR2Bcd(rslt * 100000000);                  // save 8 digits of fraction
        btemp := FMemory.FetchAR2Bcd(8);                        // Get it back
        btemp := btemp * 1000000000;                            // shift 9 digits
        btemp := btemp + (op2 - (op1 * quotient));              // add in the decimal remainder
        FMemory.StoreAR2Bcd(btemp * 10000);                     // save remainders
    end;
end;

procedure T1005FedSysCPU.ExecuteInstruction;
var
    op,op2: Byte;
begin
    FState := FState - [ucsInstructionFetched];

    if (Assigned(FOnExecuteInstruction)) then
        FOnExecuteInstruction(Self);

    if (Assigned(FOnDebug)) then
        FOnDebug(Self, nil);

    op := FMemory.FetchFedSysOpcode;
    FCrntOperand := FMemory.FetchFedSysOperand;
    if (op = X3_V) then
        op2 := FCrntOperand.L.Col
    else if (op = X3_NOT_EQUAL) then
        op2 := FCrntOperand.M.Col
    else
        op2 := 0;
    FCrntOpcode := FOpcodes.FindOpcode(op, op2);
    if (Assigned(FCrntOpcode.Proc)) then
        FCrntOpcode.Proc;
end;

procedure T1005FedSysCPU.FetchInstruction;
var
    pc: I1005Addr;
    mrow, mcol, lrow, lcol: Byte;
    op: T1005FedSysOperand;
    bank, row, col: Byte;
begin
    if (ucsInstructionFetched in FState) then
        Exit;

    pc := FMemory.FetchPAK;
    FMemory.StoreOpcode(FMemory.FetchByte(pc));
    pc.Increment;
    mrow := FMemory.FetchByte(pc);
    pc.Increment;
    mcol := FMemory.FetchByte(pc);
    pc.Increment;
    lrow := FMemory.FetchByte(pc);
    pc.Increment;
    lcol := FMemory.FetchByte(pc);
    pc.Increment;
    op := FedSysOperand(mrow, mcol, lrow, lcol);
    FMemory.StoreOperand(op);
    // If we have just fetched the last instn in a row,
    // bump to the next row.
    pc.Decode(bank, row, col);
    if (col = 31) then
        pc.Increment;
    FMemory.StorePAK(pc);

    if (Assigned(FOnFetchInstruction)) then
        FOnFetchInstruction(Self);

    FState := FState + [ucsInstructionFetched];
end;

procedure T1005FedSysCPU.IC;
var
    addr: I1005Addr;
    limit, incr, count: TBcd;
    test: Integer;
begin
    addr := T1005FedSysAddr.Create;
    addr.Assign(FMemory.FetchFedSysOperand.M);
    limit := FMemory.FetchBcd(addr, 2);
    addr.Increment;
    addr.Increment;
    count := FMemory.FetchBcd(addr, 2);
    addr.Increment;
    addr.Increment;
    incr := FMemory.FetchBcd(addr, 1);
    count := count + incr;
    test := BcdCompare(count, limit);
    if (test < 0) then
        FMemory.StoreIndicator(IND_LESS)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_GREATER)
    else
        FMemory.StoreIndicator(IND_EQUAL);
    addr.Assign(FMemory.FetchFedSysOperand.M);
    addr.Increment;
    addr.Increment;
    FMemory.StoreBcd(addr, 2, count);
end;

procedure T1005FedSysCPU.J;
begin
    FMemory.StorePAK(FMemory.FetchFedSysOperand.M);
end;

procedure T1005FedSysCPU.J0;
// Jump if indicator zero. JE / JEA / JZ
begin
    if (FMemory.FetchIndicator = IND_EQUAL) then
        FMemory.StorePAK(FMemory.FetchFedSysOperand.M);
end;

procedure T1005FedSysCPU.J1;
// Jump if indicator 1. JUA / JL/ JP
begin
    if (FMemory.FetchIndicator = IND_NOT_EQUAL) then
        FMemory.StorePAK(FMemory.FetchFedSysOperand.M);
end;
procedure T1005FedSysCPU.J2;
// Jump if indicator 2. JG
begin
    if (FMemory.FetchIndicator = IND_GREATER) then
        FMemory.StorePAK(FMemory.FetchFedSysOperand.M);
end;

procedure T1005FedSysCPU.JR;
begin
    FMemory.StoreFedSysX(FMemory.FetchPAK);
    FMemory.StorePAK(FMemory.FetchFedSysOperand.M);
end;


procedure T1005FedSysCPU.JX;
var
    op: T1005FedSysOperand;
    addr: I1005Addr;
begin
    op := FMemory.FetchFedSysOperand;
    addr := FMemory.FetchFedSysX;
    FMemory.StoreByte(op.M, X3_2);              // Store J operand
    op.M.Increment;
    FMemory.StoreByte(op.M, addr.Row);          // Save return addr msb
    op.M.Increment;
    FMemory.StoreByte(op.M, addr.Col);
    op.M.Increment;
    addr.SetAddr(addr.SequentialAddr + 4);
    FMemory.StoreByte(op.M, addr.Row);          // Save return addr lsb
    op.M.Increment;
    FMemory.StoreByte(op.M, addr.Col);
end;

procedure T1005FedSysCPU.LA1;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 10);
    i := 10;
    while (len > 0) do
    begin
        FMemory.StoreAR1(i, FMemory.FetchByte(op.L));
        op.L.Decrement;
        Dec(len);
        Dec(i);
    end;
    while (i > 0) do
    begin
        FMemory.StoreAR1(i, X3_SPACE);
        Dec(i);
    end;
end;

procedure T1005FedSysCPU.LA2;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 21);
    i := 21;
    while (len > 0) do
    begin
        FMemory.StoreAR2(i, FMemory.FetchByte(op.L));
        op.L.Decrement;
        Dec(len);
        Dec(i);
    end;
    while (i > 0) do
    begin
        FMemory.StoreAR2(i, X3_SPACE);
        Dec(i);
    end;
end;

procedure T1005FedSysCPU.LD1;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 10);
    i := 1;
    while (i <= len) do
    begin
        FMemory.StoreAR1(i, FMemory.FetchByte(op.M));
        op.M.Increment;
        Inc(i);
    end;
    while (i <= 10) do
    begin
        FMemory.StoreAR1(i, X3_SPACE);
        Inc(i);
    end;
end;

procedure T1005FedSysCPU.LD2;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 21);
    i := 1;
    while (i <= len) do
    begin
        FMemory.StoreAR2(i, FMemory.FetchByte(op.M));
        op.M.Increment;
        Inc(i);
    end;
    while (i <= 21) do
    begin
        FMemory.StoreAR2(i, X3_SPACE);
        Inc(i);
    end;
end;

procedure T1005FedSysCPU.LN1;
var
    i: Integer;
    b: Byte;
begin
    // Transfer to register
    LA1;
    // Strip zone bits
    for i := 1 to 10 do
    begin
        b := FMemory.FetchAR1(i) and $0f;
        FMemory.StoreAR1(i, b);
    end;
end;

procedure T1005FedSysCPU.LN2;
var
    i: Integer;
    b: Byte;
begin
    // Transfer to register
    LA2;
    // Strip zone bits
    for i := 1 to 21 do
    begin
        b := FMemory.FetchAR2(i) and $0f;
        FMemory.StoreAR2(i, b);
    end;
end;

procedure T1005FedSysCPU.LPR;
var
    len: Integer;
    op: T1005FedSysOperand;
    pr: I1005Addr;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := Min(op.L.SequentialAddr - op.M.SequentialAddr + 1, 132);
    pr := T1005FedSysAddr.Create;
    pr.SetAddr(PRINTER_BUFFER + len);
    MemClear(pr, 132 - len);
    pr.SetAddr(PRINTER_BUFFER);
    MemCpy(pr, op.M, len);
end;

procedure T1005FedSysCPU.LWS;
var
    i: Integer;
    b, sign: Byte;
begin
    // Transfer memory to AR2
    LA2;
    // Determine sign
    b := FMemory.FetchAR2(21);
    if ((b and $20) <> 0) then
        sign := X3_MINUS
    else
        sign := X3_SPACE;
    // Stip zone and shift AR2 left 1 byte
    for i := 2 to 21 do
    begin
        b := FMemory.FetchAR2(i) and $0f;
        FMemory.StoreAR2(i - 1, b);
    end;
    // Add sign to AR2
    FMemory.StoreAR2(21, sign);
end;

procedure T1005FedSysCPU.MemClear(const dest: I1005Addr; len: Integer);
begin
    while (len > 0) do
    begin
        FMemory.StoreByte(dest, X3_SPACE);
        dest.Increment;
        Dec(len);
    end;
end;

procedure T1005FedSysCPU.MemCpy(const dest, src: I1005Addr; len: Integer);
begin
    while (len > 0) do
    begin
        FMemory.StoreByte(dest, FMemory.FetchByte(src));
        dest.Increment;
        src.Increment;
        Dec(len);
    end;
end;

procedure T1005FedSysCPU.MUL;
var
    op: T1005FedSysOperand;
    len: Integer;
    op1, op2, rslt: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := Min(8, op.L.SequentialAddr - op.M.SequentialAddr + 1);
    op1 := FMemory.FetchBcd(op.M, len);
    op1.SignSpecialPlaces := op1.SignSpecialPlaces and $7f;     // clear negative flag
    op2 := FMemory.FetchAR1Bcd(9);
    op2.SignSpecialPlaces := op2.SignSpecialPlaces and $7f;     // clear negative flag
    rslt := op1 * op2;
    FMemory.StoreAR2Bcd(rslt);
end;

procedure T1005FedSysCPU.PrintSpace(count: Integer);
var
    i: Integer;
    addr: I1005Addr;
begin
    FPrinter.Print;
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(PRINTER_BUFFER);
    for i := 1 to 132 do
        FMemory.StoreByte(addr, X3_SPACE);
    FPrinter.Space(count);
    if (FAlt2 and (FMemory.FetchIndicator = IND_LESS)) then
        FPrinter.Skip(7);
end;

procedure T1005FedSysCPU.Punch(stacker: Cardinal; image: Cardinal);
begin
    if (stacker = 0) then
        stacker := 1
    else
        stacker := 2;
    if (image = 0) then
        FPunch.Punch(stacker)
    else
        FPunch.PunchImage(stacker);
end;

procedure T1005FedSysCPU.PunchPaperTape(count: Integer);
begin
    { TODO : Need to implement someday }
    raise Exception.Create('Not implemented');
end;

procedure T1005FedSysCPU.Read(image: Cardinal);
var
    rslt: Boolean;
begin
    if (image = 0) then
        rslt := FReader.ReadImage
    else
        rslt := FReader.Read;
    if (not rslt) then
        raise Exception.Create('Reader error');
end;

procedure T1005FedSysCPU.ReadAux(stacker: Cardinal);
begin
    { TODO : Need to implement someday }
    raise Exception.Create('Not implemented');
end;

procedure T1005FedSysCPU.ReadPaperTape(count: Integer);
begin
    { TODO : Need to implement someday }
    raise Exception.Create('Not implemented');
end;

procedure T1005FedSysCPU.ReadPunch(stacker: Cardinal; image: Cardinal);
begin
    { TODO : Need to implement someday }
    raise Exception.Create('Not implemented');
end;

procedure T1005FedSysCPU.SA1;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    i := 10;
    while ((len > 0) and (i > 0)) do
    begin
        FMemory.StoreByte(op.L, FMemory.FetchAR1(i));
        op.L.Decrement;
        Dec(len);
        Dec(i);
    end;
    while (len > 0) do
    begin
        FMemory.StoreByte(op.L, X3_SPACE);
        op.L.Decrement;
        Dec(len);
    end;
end;

procedure T1005FedSysCPU.SA2;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    i := 21;
    while ((len > 0) and (i > 0)) do
    begin
        FMemory.StoreByte(op.L, FMemory.FetchAR2(i));
        op.L.Decrement;
        Dec(len);
        Dec(i);
    end;
    while (len > 0) do
    begin
        FMemory.StoreByte(op.L, X3_SPACE);
        op.L.Decrement;
        Dec(len);
    end;
end;

procedure T1005FedSysCPU.SD1;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    i := 1;
    while ((i <= 10) and (i <= len)) do
    begin
        FMemory.StoreByte(op.M, FMemory.FetchAR1(i));
        op.M.Increment;
        Inc(i);
    end;
    while (i <= len) do
    begin
        FMemory.StoreByte(op.M, X3_SPACE);
        op.M.Increment;
        Inc(i);
    end;
end;

procedure T1005FedSysCPU.SD2;
var
    len: Integer;
    i: Integer;
    op: T1005FedSysOperand;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    i := 1;
    while ((i <= 21) and (i <= len)) do
    begin
        FMemory.StoreByte(op.M, FMemory.FetchAR2(i));
        op.M.Increment;
        Inc(i);
    end;
    while (i <= len) do
    begin
        FMemory.StoreByte(op.M, X3_SPACE);
        op.M.Increment;
        Inc(i);
    end;
end;

procedure T1005FedSysCPU.SED;
var
    op: T1005FedSysOperand;
    addr1, addr2, addr3, addr4: I1005Addr;
    len: Integer;
begin
    addr1 := T1005FedSysAddr.Create;
    addr2 := T1005FedSysAddr.Create;
    addr3 := T1005FedSysAddr.Create;
    addr4 := T1005FedSysAddr.Create;
    // Transfer AR2 to memory with zero suppression
    SZS;
    // Insert editing characters
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    if (len < 4) then                   // Too few characters to edit
        Exit;
    // Insert the decimal point
    addr1 := op.M;                      // addr1 = MSB
    addr2 := addr1;                     // addr2 = MSB + 1
    addr2.Increment;
    len := len - 3;
    MemCpy(addr1, addr2, len);
    addr3.SetAddr(op.L.SequentialAddr - 3);
    FMemory.StoreByte(addr3, X3_PERIOD);
    // Insert the commas
    len := len - 4;
    addr3.SetAddr(addr3.SequentialAddr - 4);
    addr4.SetAddr(addr3.SequentialAddr + 1);
    while (len > 0) do
    begin
        if ((FMemory.FetchByte(addr3) <> X3_SPACE) and (FMemory.FetchByte(addr4) <> X3_SPACE)) then
        begin
            MemCpy(addr1, addr2, len);
            FMemory.StoreByte(addr3, X3_COMMA);
            addr3.SetAddr(addr3.SequentialAddr - 4);
            addr4.SetAddr(addr4.SequentialAddr - 4);
            len := len - 4;
        end else
            Break;
    end;
end;

procedure T1005FedSysCPU.SHIFTL;
var
    op: T1005FedSysOperand;
    src, dest: I1005Addr;
    count: Integer;
    bank, row, col: Byte;
begin
    src := T1005FedSysAddr.Create;
    dest := T1005FedSysAddr.Create;
    op := FMemory.FetchFedSysOperand;
    op.M.Decode(bank, row, col);
    count := col;
    op.M.SetAddr(bank, row, 1);
    src.SetAddr(op.M.SequentialAddr + count);
    dest := op.M;
    while (src.Compare(op.L) <= 0) do
    begin
        FMemory.StoreByte(dest, FMemory.FetchByte(src));
        src.Increment;
        dest.Increment;
    end;
    while (count > 0) do
    begin
        FMemory.StoreByte(dest, X3_SPACE);
        dest.Increment;
        Dec(count);
    end;
end;

procedure T1005FedSysCPU.SHIFTR;
var
    op: T1005FedSysOperand;
    src, dest: I1005Addr;
    count: Integer;
    bank, row, col: Byte;
begin
    src := T1005FedSysAddr.Create;
    dest := T1005FedSysAddr.Create;
    op := FMemory.FetchFedSysOperand;
    op.L.Decode(bank, row, col);
    count := col;
    op.L.SetAddr(bank, row, 31);
    src.SetAddr(op.L.SequentialAddr - count);
    dest := op.L; 
    while (src.Compare(op.M) >= 0) do
    begin   
        FMemory.StoreByte(dest, FMemory.FetchByte(src));
        src.Decrement;
        dest.Decrement;    
    end;
    while (count > 0) do
    begin
        FMemory.StoreByte(dest, X3_SPACE);
        dest.Decrement;
        Dec(count);
    end;
end;

procedure T1005FedSysCPU.Skip(chan: Cardinal);
begin
    FPrinter.Skip(chan);
end;

procedure T1005FedSysCPU.SM1;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR1Bcd(len);
    zero := 0;
    rslt := op1 - op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreBcd(op.M, len, rslt);
end;

procedure T1005FedSysCPU.SM2;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR2Bcd(len);
    zero := 0;
    rslt := op1 - op2;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreBcd(op.M, len, rslt);
end;

procedure T1005FedSysCPU.SPR;
var
    len: Integer;
    op: T1005FedSysOperand;
    pr: I1005Addr;
begin
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    pr := T1005FedSysAddr.Create;
    pr.SetAddr(PRINTER_BUFFER);
    MemCpy(op.M, pr, Min(len, 132));
    if (len > 132) then
    begin
        op.M.SetAddr(op.M.SequentialAddr + 132);
        MemClear(op.M, len - 132);
    end;
end;

procedure T1005FedSysCPU.SR1;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR1Bcd(len);
    zero := 0;
    rslt := op2 - op1;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreAR1Bcd(rslt);
end;

procedure T1005FedSysCPU.SR2;
var
    op: T1005FedSysOperand;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchFedSysOperand;
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.M, len);
    op2 := FMemory.FetchAR2Bcd(len);
    zero := 0;
    rslt := op2 - op1;
    test := BcdCompare(rslt, zero);
    if (test < 0) then
        FMemory.StoreIndicator(IND_NEGATIVE)
    else if (test > 0) then
        FMemory.StoreIndicator(IND_POSITIVE)
    else
        FMemory.StoreIndicator(IND_ZERO);
    FMemory.StoreAR2Bcd(rslt);
end;

procedure T1005FedSysCPU.Start;
begin
    FState := FState - [ucsHalted, ucsError];
    try
        FetchInstruction;
        repeat
            ExecuteInstruction;
            FetchInstruction;
        until ((FState * [ucsSingleStep, ucsHalted, ucsError]) <> []);
        FState := FState + [ucsHalted];
    except
        on E: Exception do
        begin
            if (Assigned(FOnError)) then
                FOnError(Self, E);
            if (Assigned(FOnDebug)) then
                FOnDebug(Self, E);
            FState := FState + [ucsError];
            if (E is EAssertionFailed) then
                Application.ShowException(E);
        end;
    end;
end;

procedure T1005FedSysCPU.Stop;
begin
    FState := FState + [ucsHalted];
end;

procedure T1005FedSysCPU.SZS;
var
    len: Integer;
    b: Byte;
    op: T1005FedSysOperand;
begin
    // Transfer AR2 to memory
    SA2;
    // Zero suppress result
    op := FedSysOperand(FCrntOperand.M, FCrntOperand.L);
    len := op.L.SequentialAddr - op.M.SequentialAddr + 1;
    while (len > 0) do
    begin
        b := FMemory.FetchByte(op.M);
        if ((b <> X3_SPACE) and (b <> X3_0)) then
            Break;
        FMemory.StoreByte(op.M, X3_SPACE);
        op.M.Increment;
        Dec(len);
    end;
end;

procedure T1005FedSysCPU.XFC;
var
    op: T1005FedSysOperand;
    bit, bits: Cardinal;
begin
    op := FMemory.FedFedSysOperandRaw;
    bits := ((OP.M.Row and $3f) shl 18) or              // Get all bits into a single 32-bit value
            ((OP.M.Col and $3f) shl 12) or
            ((OP.L.Row and $3f) shl 6) or
            (OP.L.Col and $3f);
    bit := $800000;                                     // starting test bit
    while (bit <> 0) do
    begin
        if ((bits and bit) = 0) then                    // if under test not set
        begin
            case bit of
              $400000:  PrintSpace(1);
              $200000:  PrintSpace(2);
              $100000,
              $080000,
              $040000:  Skip(((bits and $1c0000) shr 18) xor $07);
              $010000:  Read(bits and $000008);
              $008000:  ReadAux(bits and $000c00);
              $004000:  ReadPunch(bits and $000200, bits and $000004);
              $002000:  Punch(bits and $000200, bits and $000004);
              $001000:  FState := FState + [ucsHalted];
              $000100:  ReadPaperTape(1);
              $000080:  ReadPaperTape(-1);
              $000040:  ReadPaperTape(80);
              $000020:  PunchPaperTape(1);
              $000002:  PunchPaperTape(-1);
              $000001:  PunchPaperTape(-2);
            end;
        end;
        bit := bit shr 1;
    end;
end;

{ T1005CPU }

constructor T1005CPU.Create(mem: T1005Memory; rdr: T1005Reader; prt: T1005Printer; pch: T1005Punch);
begin
    inherited Create;
    FMemory := mem;
    FReader := rdr;
    FPrinter := prt;
    FPunch := pch;
end;

function T1005CPU.GetSingleStep: Boolean;
begin
    Result := (ucsSingleStep in FState);
end;

procedure T1005CPU.SetAlt1(Value: Boolean);
begin
    FAlt1 := Value;
end;

procedure T1005CPU.SetAlt2(Value: Boolean);
begin
    FAlt2 := Value;
end;

procedure T1005CPU.SetSingleStep(const Value: Boolean);
begin
    if (Value) then
        Include(FState, ucsSingleStep)
    else
        Exclude(FState, ucsSingleStep);
end;

{ T1005CommCPU }

procedure T1005CommCPU.AD;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'AD OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);      // Adjust to start of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op2 := FMemory.FetchBcd(op.Fldb, len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    FConditionCodes := FConditionCodes and (not CC_SIGNS);    // Clear previous sign bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_MINUS
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_PLUS
    else
        FConditionCodes := FConditionCodes or CC_SIGN_ZERO;
    if (BcdPrecision(rslt) > len) then
        FConditionCodes := FConditionCodes or CC_ARITHMETIC_OVERFLOW;
    FMemory.StoreBcd(op.Fldb, len, rslt);
end;

procedure T1005CommCPU.AK;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Flda.SetAddr(1, 32, 2);
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'AK OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op1 := FMemory.FetchBcd(op.Flda, 2);
    op2 := FMemory.FetchBcd(op.Fldb, len);
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    FConditionCodes := FConditionCodes and (not CC_SIGNS);    // Clear previous sign bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_MINUS
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_PLUS
    else
        FConditionCodes := FConditionCodes or CC_SIGN_ZERO;
    if (BcdPrecision(rslt) > len) then
        FConditionCodes := FConditionCodes or CC_ARITHMETIC_OVERFLOW;
    FMemory.StoreBcd(op.Fldb, len, rslt);
end;

procedure T1005CommCPU.AM;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'AM OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);      // Adjust to start of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op1.SignSpecialPlaces := op1.SignSpecialPlaces and $7f;
    op2 := FMemory.FetchBcd(op.Fldb, len);
    op2.SignSpecialPlaces := op2.SignSpecialPlaces and $7f;
    zero := 0;
    rslt := op1 + op2;
    test := BcdCompare(rslt, zero);
    FConditionCodes := FConditionCodes and (not CC_SIGNS);    // Clear previous sign bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_MINUS
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_PLUS
    else
        FConditionCodes := FConditionCodes or CC_SIGN_ZERO;
    if (BcdPrecision(rslt) > len) then
        FConditionCodes := FConditionCodes or CC_ARITHMETIC_OVERFLOW;
    FMemory.StoreBcd(op.Fldb, len, rslt);
end;

procedure T1005CommCPU.BT;
// This implements and undocumented instruction with opcode V. This
// instrcutions seems to be "read card and jump to address zero".
// Which is, effectively a request to boot from card.
begin
    InitBoot;
end;

procedure T1005CommCPU.CA;
var
    op: T1005CommOperands;
    op1, op2: Byte;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'CA OP2 LSL < MSL');
    FConditionCodes := FConditionCodes and (not CC_TESTS);
    while (not op.Fldc.Matches(op.Fldb)) do
    begin
        op1 := FMemory.FetchByte(op.Flda);
        op2 := FMemory.FetchByte(op.Fldc);
        if (op1 <> op2) then
        begin
            FConditionCodes := FConditionCodes or CC_NOT_EQUAL;
            Exit;
        end;
        op.Flda.Decrement;
        op.Fldc.Decrement;
    end;
    op1 := FMemory.FetchByte(op.Flda);
    op2 := FMemory.FetchByte(op.Fldc);
    if (op1 <> op2) then
        FConditionCodes := FConditionCodes or CC_NOT_EQUAL
    else
        FConditionCodes := FConditionCodes or CC_EQUAL;
end;

procedure T1005CommCPU.CC;
var
    op: T1005CommOperands;
    count: TBcd;
    addr: I1005Addr;
    row, col: Byte;
begin
    op := FMemory.FetchCommOperands;
    addr := T1005CommAddr.Create;
    addr.SetAddr(1, 32, 2);
    count := FMemory.FetchBcd(addr, 2);
    addr.SetAddr(op.Fldb.Row, op.Fldb.Col);
    row := FMemory.FetchByte(addr);
    addr.Increment;
    col := FMemory.FetchByte(addr);
    addr.SetAddr(row, col);
    if (count > 0) then
    begin
        addr.Increment(BcdToInteger(count));
    end else if (count < 0) then
    begin
        addr.Decrement(-BcdToInteger(count));
    end;
//    addr.SetAddr(addr.SequentialAddr + BcdToInteger(count));
    FMemory.StoreByte(op.Fldb, addr.Row);
    op.Fldb.Increment;
    FMemory.StoreByte(op.Fldb, addr.Col);
end;

procedure T1005CommCPU.CK;
var
    op: T1005CommOperands;
    count: Integer;
    op1, op2: Byte;
begin
    op := FMemory.FetchCommOperands;
    op.Flda.SetAddr(1, 32, 3);
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'CK OP2 LSL < MSL');
    count := 0;
    FConditionCodes := FConditionCodes and (not CC_TESTS);
    // Check at most first 2 bytes of OP2 for a match to OP1
    while ((not op.Fldc.Matches(op.Fldb)) and (count < 2)) do
    begin
        op1 := FMemory.FetchByte(op.Flda);
        op2 := FMemory.FetchByte(op.Fldc);
        if (op1 <> op2) then
        begin
            FConditionCodes := FConditionCodes or CC_NOT_EQUAL;
            Exit;
        end;
        op.Flda.Decrement;
        op.Fldc.Decrement;
        Inc(count);
    end;
    // No match so far. If there were exactly 1 or 2 bytes to check,
    // check the last one for a match to OP1.
    if (op.Fldc.Matches(op.Fldb)) then
    begin
        op1 := FMemory.FetchByte(op.Flda);
        op2 := FMemory.FetchByte(op.Fldc);
        if (op1 <> op2) then
            FConditionCodes := FConditionCodes or CC_NOT_EQUAL
        else
            FConditionCodes := FConditionCodes or CC_EQUAL;
        Exit;
    end;
    // Check bytes remaining beyond the first 2 for spaces.
    while (not op.Fldc.Matches(op.Fldb)) do
    begin
        op2 := FMemory.FetchByte(op.Fldc);
        if (op2 <> X3_SPACE) then
        begin
            FConditionCodes := FConditionCodes or CC_NOT_EQUAL;
            Exit;
        end;
        op.Fldc.Decrement;
    end;
    op2 := FMemory.FetchByte(op.Fldc);
    if (op2 <> X3_SPACE) then
    begin
        FConditionCodes := FConditionCodes or CC_NOT_EQUAL;
        Exit;
    end;
    FConditionCodes := FConditionCodes or CC_EQUAL;
end;

procedure T1005CommCPU.Clear;
begin
    FState := [ucsHalted, ucsInstructionFetched];
    // Clear all conditions except ALT1 and ALT2.
    FConditionCodes := FConditionCodes and (CC_ALTERNATE_HOLD1 or CC_ALTERNATE_HOLD2);
    //
    InitBoot;
end;

procedure T1005CommCPU.CM;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'CM OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);        // Adjust to beginning of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op1.SignSpecialPlaces := op1.SignSpecialPlaces and $7f;
    op2 := FMemory.FetchBcd(op.Fldb, len);
    op2.SignSpecialPlaces := op2.SignSpecialPlaces and $7f;
    test := BcdCompare(op1, op2);
    FConditionCodes := FConditionCodes and (not CC_TESTS);    // Clear previous comparison bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_LESS_THAN
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_GREATER_THAN
    else
        FConditionCodes := FConditionCodes or CC_EQUAL;
end;

procedure T1005CommCPU.CN;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'CN OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);      // Adjust to start of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op2 := FMemory.FetchBcd(op.Fldb, len);
    test := BcdCompare(op1, op2);
    FConditionCodes := FConditionCodes and (not CC_TESTS);    // Clear previous comparison bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_LESS_THAN
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_GREATER_THAN
    else
        FConditionCodes := FConditionCodes or CC_EQUAL;
end;

constructor T1005CommCPU.Create(mem: T1005Memory; rdr: T1005Reader; prt: T1005Printer; pch: T1005Punch);
begin
    inherited;
    FOpcodes := T1005OpcodeList.Create;
    FOpcodes.AddOpcode(OpCode(X3_0, 'TA', it2Operand, True, TA));
    FOpcodes.AddOpcode(OpCode(X3_4, 'TC', it2Operand, True, TC));
    FOpcodes.AddOpcode(OpCode(X3_RIGHT_SQUARE, 'TD', it2Operand, True, TD));
    FOpcodes.AddOpcode(OpCode(X3_2, 'TK', it2Operand, True, TK));
    FOpcodes.AddOpcode(OpCode(X3_SEMI, 'TN', it2Operand, True, TN));
    FOpcodes.AddOpcode(OpCode(X3_9, 'TR', it2Operand, True, TR));
    FOpcodes.AddOpcode(OpCode(X3_Z, 'TX', it1Operand, False, TX));
    FOpcodes.AddOpcode(OpCode(X3_I, 'CA', it2Operand, True, CA));
    FOpcodes.AddOpcode(OpCode(X3_7, 'CK', it2Operand, True, CK));
    FOpcodes.AddOpcode(OpCode(X3_F, 'CM', it2Operand, True, CM));
    FOpcodes.AddOpcode(OpCode(X3_PERIOD, 'CN', it2Operand, True, CN));
    FOpcodes.AddOpcode(OpCode(X3_1, 'AD', it2Operand, True, AD));
    FOpcodes.AddOpcode(OpCode(X3_5, 'AM', it2Operand, True, AM));
    FOpcodes.AddOpcode(OpCode(X3_AT, 'DV', it2Operand, False, DV));
    FOpcodes.AddOpcode(OpCode(X3_M, 'ML', it2Operand, False, ML));
    FOpcodes.AddOpcode(OpCode(X3_ASTERIX, 'MU', it2Operand, False, MU));
    FOpcodes.AddOpcode(OpCode(X3_MINUS, 'SM', it2Operand, True, SM));
    FOpcodes.AddOpcode(OpCode(X3_COLON, 'SU', it2Operand, True, SU));
    FOpcodes.AddOpcode(OpCode(X3_6, 'AK', it2Operand, True, AK));
    FOpcodes.AddOpcode(OpCode(X3_W, 'CC', it1Operand, False, CC));
    FOpcodes.AddOpcode(OpCode(X3_COMMA, 'EL', it2Operand, False, EL));
    FOpcodes.AddOpcode(OpCode(X3_G, 'ED', it2Operand, True, ED));
    FOpcodes.AddOpcode(OpCode(X3_LOZENGE, 'SC', it1Operand, False, SC));
    FOpcodes.AddOpcode(OpCode(X3_X, 'JC', it1Operand, False, JC));
    FOpcodes.AddOpcode(OpCode(X3_LEFT_PAREN, 'JK', it2Operand, False, JK));
    FOpcodes.AddOpcode(OpCode(X3_J, 'JL', it2Operand, False, JL));
    FOpcodes.AddOpcode(OpCode(X3_N, 'JR', it2Operand, False, JR));
    FOpcodes.AddOpcode(OpCode(X3_Y, 'JT', it1Operand, False, JT));
    FOpcodes.AddOpcode(OpCode(X3_SLASH, 'J', it1Operand, False, J));
    FOpcodes.AddOpcode(OpCode(X3_PERCENT, 'GC', it2Operand, False, GC));
    FOpcodes.AddOpcode(OpCode(X3_DOLLAR, 'RT', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_K, 'WT', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_P, 'RD', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_S, 'SD', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_Q, 'RF', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_U, 'SF', it2Operand));
    FOpcodes.AddOpcode(OpCode(X3_V, 'BT', it1Operand, False, BT));
end;

destructor T1005CommCPU.Destroy;
begin
    FreeAndNil(FOpcodes);
    inherited;
end;

procedure T1005CommCPU.DV;
var
    op: T1005CommOperands;
    op1, op2, op3, op4: I1005Addr;
    dividend, divisor, quotient, remainder: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op1 := op.Flda.Clone;
    op2 := T1005CommAddr.Create;
    op3 := op.Fldc.Clone;
    op4 := T1005CommAddr.Create;
    op1.Decrement(5);
//    op1.SetAddr(op.Flda.SequentialAddr - 5);
    op2 := op.Fldb;
    op3.Decrement(7);
//    op3.SetAddr(op.Fldc.SequentialAddr - 7);
    divisor := FMemory.FetchBcd(op1, 6);
    if (divisor < 0) then
        divisor := divisor * -1;
    dividend := FMemory.FetchBcd(op2, 8);
    if (dividend < 0) then
        dividend := dividend * -1;
    quotient := dividend / divisor;
    FMemory.StoreBcd(op3, 8, quotient);
    // Calculate and save the remainder.
    // First get rid of the fractional part of the result by setting
    // the precision of the result to be the number of places left of
    // the decimal and setting the scale to zero by clearing the rightmost
    // 6 bits of SignSpecialPlaces.
    quotient.Precision := BcdPrecision(quotient);
    quotient.SignSpecialPlaces := quotient.SignSpecialPlaces and $C0;
    remainder := dividend - (divisor * quotient);
    op4.SetAddr(1, 32, 18);
    FMemory.StoreBcd(op4, 6, remainder);
end;

procedure T1005CommCPU.ED;
var
    op: T1005CommOperands;
    op1, fill, xc: Byte;
    x: Integer;
    done, suppress, copy: Boolean;
begin
    op := FMemory.FetchCommOperands;
    // Phase one. Edit the OP1 according to the edit mask in X.
    // X3_BACK_SLASH    - zero suppress and asterix fill
    // X3_DELTA         - zero suppress and space fill
    // X3_LOZENGE       - copy character to output
    // X3_NOT_EQUAL     - end of mask
    done := False;
    suppress := False;
    fill := X3_SPACE;
    x := 1;
    while (not done) do
    begin
        copy := False;
        op1 := FMemory.FetchByte(op.Flda) and $0f;      // Strip off X & Y bits
        xc := FMemory.FetchCommX(x);
        case (xc) of
          X3_BACK_SLASH:
          begin
            suppress := True;
            fill := X3_ASTERIX;
            copy := True;
          end;
          X3_DELTA:
          begin
            suppress := True;
            fill := X3_SPACE;
            copy := True;
          end;
          X3_LOZENGE:
          begin
            copy := True;
          end;
          X3_NOT_EQUAL:
          begin
            done := True;
            FMemory.StoreCommX(x, X3_SPACE);
            copy := False;
          end;
        end;
        if (copy) then
        begin
            if (((op1 = X3_0) or (op1 = X3_COMMA) or (op1 = X3_SPACE)) and
                (suppress)) then
            begin
                FMemory.StoreCommX(x, fill);
            end else
            begin
                FMemory.StoreCommX(x, op1);
                suppress := False;
            end;
            op.Flda.Increment;
        end else
        begin
            if (((xc = X3_0) or (xc = X3_COMMA) or (xc = X3_SPACE)) and suppress) then
            begin
                FMemory.StoreCommX(x, fill);
            end else
            begin
                suppress := False;
            end;
        end;
        Inc(x);
        if (x > 31) then
            done := True;
    end;
    // Phase 2, copy X to OP2
    x := 1;
    while ((not op.Fldb.Matches(op.Fldc)) and (x <= 31)) do
    begin
        FMemory.StoreByte(op.Fldb, FMemory.FetchCommX(x));
        op.Fldb.Increment;
        Inc(x);
    end;
    if (x <= 31) then
        FMemory.StoreByte(op.Fldb, FMemory.FetchCommX(x));
end;

procedure T1005CommCPU.EL;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    FMemory.StoreByte(op.Fldb, FMemory.FetchByte(op.Fldb) and op.Flda.Row);
    FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Fldc) or op.Flda.Col);
end;

procedure T1005CommCPU.ExecuteInstruction;
var
    op: Byte;
begin
    FState := FState - [ucsInstructionFetched];

    if (Assigned(FOnExecuteInstruction)) then
        FOnExecuteInstruction(Self);

    if (Assigned(FOnDebug)) then
        FOnDebug(Self, nil);

    op := FMemory.FetchCommOpcode;
    FCrntOpcode := FOpcodes.FindOpcode(op, 0);
    if (Assigned(FCrntOpcode.Proc)) then
        FCrntOpcode.Proc;
end;

procedure T1005CommCPU.FetchInstruction;
var
    ilen, idx: Integer;
    op: Byte;
    icc, addr: I1005Addr;
    opcode: T1005Opcode;
begin
    if ((FState * [ucsInstructionFetched, ucsHalted, ucsError]) <> []) then
        Exit;

    addr := T1005CommAddr.Create;
    // Get current instruction counter
    icc := FMemory.FetchICC;
    // Copy the instruction to the instruction register and
    // increment the instruction counter
    op := FMemory.FetchByte(icc);
    FMemory.StoreIR(op, 1);
    opcode := FOpcodes.FindOpcode(op, 0);
    icc.Increment;
    case opcode.InstType of
      it1Operand:   ilen := 5;
      it2Operand:   ilen := 7;
      else          raise Exception.Create('OOOPS! Unknown instruction type');
    end;
    idx := 2;
    while (idx <= ilen) do
    begin
        op := FMemory.FetchByte(icc);
        FMemory.StoreIR(op, idx);
        Inc(idx);
        icc.Increment;
    end;
    // Perform address substitution for indirect addressing.
    if (opcode.Indirect) then
    begin
        // operand 1 indirection
        if ((FMemory.FetchByte(1, 32, 6) and $20) <> 0) then
        begin
            addr.SetAddr(FMemory.FetchByte(1, 32, 2), FMemory.FetchByte(1, 32, 3));
            for idx := 2 to 3 do
            begin
                FMemory.StoreIR(FMemory.FetchByte(addr), idx);
                addr.Increment;
            end;
        end;
        // operand 2 indirection
        if ((FMemory.FetchByte(1, 32, 7) and $20) <> 0) then
        begin
            addr.SetAddr(FMemory.FetchByte(1, 32, 4), FMemory.FetchByte(1, 32, 5));
            for idx := 4 to 7 do
            begin
                FMemory.StoreIR(FMemory.FetchByte(addr), idx);
                addr.Increment;
            end;
        end;
    end;
    //
    FMemory.StoreICC(icc);

    if (Assigned(FOnFetchInstruction)) then
        FOnFetchInstruction(Self);

    FState := FState + [ucsInstructionFetched];
end;

procedure T1005CommCPU.GC;
var
    op: T1005CommOperands;
    spaceBefore, skipBefore: Byte;
    image, exec, allOnes, print90: Boolean;
    punch, punchHold, punchClear, punchTest: Boolean;
    i, count: Integer;
    bfr: I1005Addr;
begin
    try
        bfr := T1005CommAddr.Create;
        op := FMemory.FetchCommOperands;
        spaceBefore := 0;
        skipBefore := 0;
        if ((op.Flda.Row and $08) <> 0) then
            Include(FIOFlipFlops, ffPrint);
        if ((op.Flda.Row and $04) <> 0) then
            spaceBefore := 1;
        if ((op.Flda.Row and $02) <> 0) then
            Include(FIOFlipFlops, ffRead);
        exec := (op.Flda.Row and $01) <> 0;
        if ((op.Flda.Col and $02) <> 0) then
            Include(FIOFlipFlops, ffPunchRead);
        image := (op.Flda.Col and $01) <> 0;
        allOnes := (op.Fldb.Row and $20) <> 0;
        if ((op.Fldb.Col and $10) <> 0) then
            skipBefore := skipBefore or 4;
        if ((op.Fldb.Col and $08) <> 0) then
            skipBefore := skipBefore or 2;
        if ((op.Fldb.Col and $04) <> 0) then
            skipBefore := skipBefore or 1;
        if ((op.Fldb.Col and $02) <> 0) then
            spaceBefore := 2;
        print90 := (op.Fldb.Col and $01) <> 0;
        punch := (op.Fldc.Col and $0e) <> 0;
        punchHold := (op.Fldc.Col and $80) <> 0;
        punchClear := (op.Fldc.Col and $04) <> 0;
        punchTest := (op.Fldc.Col and $02) <> 0;
        if (not exec) then
        begin
            // Before execute functions
            if (spaceBefore <> 0) then
            begin
                if (FPrinter.Space(spaceBefore)) then
                    FConditionCodes := FConditionCodes or CC_FORM_OVERFLOW;
            end;
            if (skipBefore <> 0) then
                FPrinter.Skip(skipBefore);
        end;
        // Perform immediate operations
        if (punch) then
        begin
            // Because all I/O in the emulator is syncrhonous, the Punch Test
            // operation is a NOOP.
            if (punchTest) then
                ;
            if (punchHold or punchClear) then
            
            begin
                if (ffStackerSelect in FIOFlipFlops) then
                begin
                    if (image) then
                    begin
                        if (not FPunch.PunchImage(2)) then
                            raise EPunchError.Create('Punch error');
                    end else
                    begin
                        if (not FPunch.Punch(2)) then
                            raise EPunchError.Create('Punch error');
                    end;
                end else
                begin
                    if (image) then
                    begin
                        if (not FPunch.PunchImage(1)) then
                            raise EPunchError.Create('Punch error');
                    end else
                    begin
                        if (not FPunch.Punch(1)) then
                            raise EPunchError.Create('Punch error');
                    end;
                end;
                if (punchClear) then
                begin
                    bfr.SetAddr(PUNCH_BUFFER);
                    if (image) then
                        count := 160
                    else
                        count := 80;
                    for i := 1 to count do
                    begin
                        FMemory.StoreByte(bfr, X3_SPACE);
                        bfr.Increment;
                    end;
                end;
            end;
        end;
        // Perform any pending operations if execute is set
        if (exec) then
        begin
            if (ffPrint in FIOFlipFlops) then
            begin
                if (spaceBefore <> 0) then
                begin
                    if (FPrinter.Space(spaceBefore)) then
                        FConditionCodes := FConditionCodes or CC_FORM_OVERFLOW;
                end;
                if (skipBefore <> 0) then
                    FPrinter.Skip(skipBefore);
                if (print90) then
                    i := 90
                else
                    i := 132;
                FPrinter.Print(i);
                bfr.SetAddr(PRINTER_BUFFER);
                while (i > 0) do
                begin
                    FMemory.StoreByte(bfr, X3_SPACE);
                    bfr.Increment;
                    Dec(i);
                end;
            end;
            if (ffRead in FIOFlipFlops) then
            begin
                if (image) then
                begin
                    if (not FReader.ReadImage(allOnes)) then
                        raise EReaderError.Create('Reader error');
                end else
                begin
                    if (not FReader.Read(allOnes)) then
                        raise EReaderError.Create('Reader error');
                end;
            end;
            if (ffPunchRead in FIOFlipFlops) then
                { TODO : Not yet implemented. Need to read up on this. };
            FIOFlipFlops := [];
        end;
        // After execute options
        if ((op.Fldc.Col and $10) <> 0) then
            Include(FIOFlipFlops, ffStackerSelect);
    except
      on E: EReaderError do
      begin
        // Not sure if this is what I should be doing here, but
        // programs don't seem to detect reader errors, they just
        // expect them to complete. So, decrement the instruction
        // counter to allow this I/O to be reissued when the user presses
        // Run.
        bfr := FMemory.FetchICC;
        bfr.Decrement(7);
//        for i := 1 to 7 do
//            bfr.Decrement;
        FMemory.StoreICC(bfr);
        raise;
      end;
      on E: EPunchError do
      begin
        // Not sure if this is what I should be doing here, but
        // programs don't seem to detect punch errors, they just
        // expect them to complete. So, decrement the instruction
        // counter to allow this I/O to be reissued when the user presses
        // Run.
        bfr := FMemory.FetchICC;
        bfr.Decrement(7);
//        for i := 1 to 7 do
//            bfr.Decrement;
        FMemory.StoreICC(bfr);
        raise;
      end;
      else
        raise;
    end;
end;

procedure T1005CommCPU.InitBoot;
const
    readCard: array [1..7] of Byte = ( X3_PERCENT, X3_0, X3_SPACE, X3_SPACE, X3_SPACE, X3_SPACE, X3_SPACE );
var
    i: Integer;
    icc: I1005Addr;
begin
    for i := 1 to 7 do
        FMemory.StoreIR(readCard[i], i);
    icc := T1005CommAddr.Create;
    icc.SetAddr(X3_SPACE, X3_SPACE);
    FMemory.StoreICC(icc);
    Include(FState, ucsInstructionFetched);
end;

procedure T1005CommCPU.J;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.JC;
var
    op: T1005CommOperands;
    test: Cardinal;
    rslt: Boolean;
begin
    rslt := False;
    op := FMemory.FetchCommOperands;
    test := (op.Flda.Row shl 26) or (op.Flda.Col shl 20);
    if (((test and CC_END_OF_TAPE) = CC_END_OF_TAPE) and
        ((FConditionCodes and CC_END_OF_TAPE) = CC_END_OF_TAPE)) then
    begin
        FConditionCodes := FConditionCodes and (not CC_END_OF_TAPE);
        rslt := True;
    end;
    if (((test and CC_FORM_OVERFLOW) <> 0) and
        ((FConditionCodes and CC_FORM_OVERFLOW) <>0)) then
    begin
        FConditionCodes := FConditionCodes and (not CC_FORM_OVERFLOW);
        rslt := True;
    end;
    if (((test and CC_ARITHMETIC_OVERFLOW) <> 0) and
        ((FConditionCodes and CC_ARITHMETIC_OVERFLOW) <>0)) then
        rslt := True;
    if (((test and CC_SENSE2) <> 0) and
        ((FConditionCodes and CC_SENSE2) <>0)) then
        rslt := True;
    if (((test and CC_SENSE1) <> 0) and
        ((FConditionCodes and CC_SENSE1) <>0)) then
        rslt := True;
    if (((test and CC_ALTERNATE_HOLD2) <> 0) and
        ((FConditionCodes and CC_ALTERNATE_HOLD2) <>0)) then
        rslt := True;
    if (((test and CC_ALTERNATE_HOLD1) <> 0) and
        ((FConditionCodes and CC_ALTERNATE_HOLD1) <>0)) then
        rslt := True;
    if (((test and CC_INTERUUPT) <> 0) and
        ((FConditionCodes and CC_INTERUUPT) <>0)) then
        rslt := True;
    if (((test and CC_UNIT_ALERT) <> 0) and
        ((FConditionCodes and CC_UNIT_ALERT) <>0)) then
        rslt := True;
    if (((test and CC_PARITY_ERROR) <> 0) and
        ((FConditionCodes and CC_PARITY_ERROR) <>0)) then
    begin
        FConditionCodes := FConditionCodes and (not CC_PARITY_ERROR);
        rslt := True;
    end;
    if (((test and CC_SIGN_PLUS) <> 0) and
        ((FConditionCodes and CC_SIGN_PLUS) <>0)) then
        rslt := True;
    if (((test and CC_SIGN_ZERO) <> 0) and
        ((FConditionCodes and CC_SIGN_ZERO) <>0)) then
        rslt := True;
    if (((test and CC_SIGN_MINUS) <> 0) and
        ((FConditionCodes and CC_SIGN_MINUS) <>0)) then
        rslt := True;
    if (rslt) then
        FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.JK;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    if ((op.Flda.Row and FMemory.FetchByte(op.Fldc)) = op.Flda.Row) then
        FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.JL;
var
    op: T1005CommOperands;
    count: TBcd;
    addr: I1005Addr;
begin
    op := FMemory.FetchCommOperands;
    addr := T1005CommAddr.Create;
    addr.SetAddr(1, 32, 2);
    count := FMemory.FetchBcd(addr, 2);
    count := count - 1;
    op.Fldc.Decrement;
    FMemory.StoreBcd(op.Fldc, 2, count);
    if (count >= 0) then
        FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.JR;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    FMemory.StoreByte(op.Fldc, op.Flda.Col);
    op.Fldc.Decrement;
    FMemory.StoreByte(op.Fldc, op.Flda.Row);
    FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.JT;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    if ((FConditionCodes and CC_EQUAL) <> 0) then
        FMemory.StoreICC(op.Flda)
    else if ((FConditionCodes and CC_LESS_THAN) <> 0) then
        FMemory.StoreICC(op.Fldb);
end;

procedure T1005CommCPU.ML;
var
    op: T1005CommOperands;
    op1, op2, op3: I1005Addr;
    multiplicand, multiplier, product: TBcd;
    x: Integer;
begin
    op := FMemory.FetchCommOperands;
    op1 := op.Flda.Clone;
    op2 := T1005CommAddr.Create;
    op3 := op.Fldc.Clone;
    op1.Decrement(8);
//    op1.SetAddr(op.Flda.SequentialAddr - 8);
    op2 := op.Fldb;
    op3.Decrement(19);
//    op3.SetAddr(op.Fldc.SequentialAddr - 19);
    multiplicand := FMemory.FetchBcd(op1, 9);
    if (multiplicand < 0) then
        multiplicand := multiplicand * -1;
    multiplier := FMemory.FetchBcd(op2, 11);
    if (multiplier < 0) then
        multiplier := multiplier * -1;
    product := multiplicand * multiplier;
    FMemory.StoreBcd(op3, 20, product);
    // Store the product in the "product register" in bank 1, row 1, col 11-30
    op.Fldc.Decrement(19);
//    op.Fldc.SetAddr(op.Fldc.SequentialAddr - 19);
    for x := 11 to 30 do
    begin
        FMemory.StoreByte(1, 1, x, FMemory.FetchByte(op3));
        op3.Increment;
    end;
end;

procedure T1005CommCPU.MU;
var
    op: T1005CommOperands;
    op1, op2, op3: I1005Addr;
    multiplicand, multiplier, product: TBcd;
    x: Integer;
begin
    op := FMemory.FetchCommOperands;
    op1 := op.Flda.Clone;
    op2 := T1005CommAddr.Create;
    op3 := op.Fldc.Clone;
    op1.Decrement(3);
//    op1.SetAddr(op.Flda.SequentialAddr - 3);
    op2 := op.Fldb;
    op3.Decrement(9);
//    op3.SetAddr(op.Fldc.SequentialAddr - 9);
    multiplicand := FMemory.FetchBcd(op1, 4);
    if (multiplicand < 0) then
        multiplicand := multiplicand * -1;
    multiplier := FMemory.FetchBcd(op2, 6);
    if (multiplier < 0) then
        multiplier := multiplier * -1;
    product := multiplicand * multiplier;
    FMemory.StoreBcd(op3, 10, product);
    // Store the product in the "product register" in bank 1, row 1, col 21-30
    op.Fldc.Decrement(9);
//    op.Fldc.SetAddr(op.Fldc.SequentialAddr - 9);
    for x := 21 to 30 do
    begin
        FMemory.StoreByte(1, 1, x, FMemory.FetchByte(op.Fldc));
        op.Fldc.Increment;
    end;
end;

procedure T1005CommCPU.SC;
var
    op: T1005CommOperands;
    test: Cardinal;
begin
    op := FMemory.FetchCommOperands;
    test := (op.Fldb.Row shl 6) or op.Fldb.Col;
    if ((test and $800) <> 0) then
        FConditionCodes := FConditionCodes and (not CC_EVEN_PARITY);
    if ((test and $400) <> 0) then
        FConditionCodes := FConditionCodes or CC_EVEN_PARITY;
    if ((test and $200) <> 0) then
        FConditionCodes := FConditionCodes or CC_SENSE2;
    if ((test and $100) <> 0) then
        FConditionCodes := FConditionCodes or CC_SENSE1;
    if ((test and $080) <> 0) then
        FConditionCodes := FConditionCodes and (not CC_SENSE2);
    if ((test and $040) <> 0) then
        FConditionCodes := FConditionCodes and (not CC_SENSE1);
    if ((test and $020) <> 0) then
        FConditionCodes := FConditionCodes and (not CC_PPT);
    if ((test and $010) <> 0) then
        FConditionCodes := FConditionCodes or CC_PPT;
    if ((test and $008) <> 0) then
        FConditionCodes := FConditionCodes and (not CC_SERVO1);
    if ((test and $004) <> 0) then
        FConditionCodes := FConditionCodes or CC_SERVO1;
    if ((test and $002) <> 0) then
    begin
        FConditionCodes := FConditionCodes or CC_INDICATOR2;
        FState := FState + [ucsHalted];
    end;
    if ((test and $001) <> 0) then
    begin
        FConditionCodes := FConditionCodes or CC_INDICATOR1;
        FState := FState + [ucsHalted];
    end;
end;

procedure T1005CommCPU.SetAlt1(Value: Boolean);
begin
    inherited;
    if (Value) then
        FConditionCodes := FConditionCodes or CC_ALTERNATE_HOLD1
    else
        FConditionCodes := FConditionCodes and (not CC_ALTERNATE_HOLD1);
end;

procedure T1005CommCPU.SetAlt2(Value: Boolean);
begin
    inherited;
    if (Value) then
        FConditionCodes := FConditionCodes or CC_ALTERNATE_HOLD2
    else
        FConditionCodes := FConditionCodes and (not CC_ALTERNATE_HOLD2);
end;

procedure T1005CommCPU.SM;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'SM OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);      // Adjust to start of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op1.SignSpecialPlaces := op1.SignSpecialPlaces and $7f;
    op2 := FMemory.FetchBcd(op.Fldb, len);
    op2.SignSpecialPlaces := op2.SignSpecialPlaces and $7f;
    zero := 0;
    rslt := op2 - op1;
    test := BcdCompare(rslt, zero);
    FConditionCodes := FConditionCodes and (not CC_SIGNS);    // Clear previous sign bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_MINUS
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_PLUS
    else
        FConditionCodes := FConditionCodes or CC_SIGN_ZERO;
    if (BcdPrecision(rslt) > len) then
        FConditionCodes := FConditionCodes or CC_ARITHMETIC_OVERFLOW;
    FMemory.StoreBcd(op.Fldb, len, rslt);
end;

procedure T1005CommCPU.Start;
begin
    FState := FState - [ucsHalted, ucsError];
    // Clear halt codes
    FConditionCodes := FConditionCodes and (not (CC_INDICATOR1 or CC_INDICATOR2));
    try
        FetchInstruction;
        repeat
            ExecuteInstruction;
            FetchInstruction;
        until ((FState * [ucsSingleStep, ucsHalted, ucsError]) <> []);
        FState := FState + [ucsHalted];
    except
        on E: Exception do
        begin
            if (Assigned(FOnError)) then
                FOnError(Self, E);
            if (Assigned(FOnDebug)) then
                FOnDebug(Self, E);
            FState := FState + [ucsError];
            if (E is EAssertionFailed) then
                Application.ShowException(E);
        end;
    end;
end;

procedure T1005CommCPU.Stop;
begin
    FState := FState + [ucsHalted];
end;

procedure T1005CommCPU.SU;
var
    op: T1005CommOperands;
    len, test: Integer;
    op1, op2, rslt, zero: TBcd;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'SU OP2 LSL < MSL');
    len := op.Fldc.SequentialAddr - op.Fldb.SequentialAddr + 1;
    op.Flda.Decrement(len - 1);
//    op.Flda.SetAddr(op.Flda.SequentialAddr - len + 1);      // Adjust to start of FldA
    op1 := FMemory.FetchBcd(op.Flda, len);
    op2 := FMemory.FetchBcd(op.Fldb, len);
    zero := 0;
    rslt := op2 - op1;
    test := BcdCompare(rslt, zero);
    FConditionCodes := FConditionCodes and (not CC_SIGNS);    // Clear previous sign bits
    if (test < 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_MINUS
    else if (test > 0) then
        FConditionCodes := FConditionCodes or CC_SIGN_PLUS
    else
        FConditionCodes := FConditionCodes or CC_SIGN_ZERO;
    if (BcdPrecision(rslt) > len) then
        FConditionCodes := FConditionCodes or CC_ARITHMETIC_OVERFLOW;
    FMemory.StoreBcd(op.Fldb, len, rslt);
end;

procedure T1005CommCPU.TA;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'TA OP2 LSL < MSL');
    while (not op.Fldc.Matches(op.Fldb)) do
    begin
        FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda));
        Op.Flda.Decrement;
        op.Fldc.Decrement;
    end;
    FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda));
end;

procedure T1005CommCPU.TC;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'TC OP2 LSL < MSL');
    while (not op.Fldc.Matches(op.Fldb)) do
    begin
        FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda));
        FMemory.StoreByte(op.Flda, X3_SPACE);
        Op.Flda.Decrement;
        op.Fldc.Decrement;
    end;
    FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda));
    FMemory.StoreByte(op.Flda, X3_SPACE);
end;

procedure T1005CommCPU.TD;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    while (not op.Fldb.Matches(op.Fldc)) do
    begin
        FMemory.StoreByte(op.Fldb, FMemory.FetchByte(op.Flda));
        Op.Flda.Increment;
        op.Fldb.Increment;
    end;
    FMemory.StoreByte(op.Fldb, FMemory.FetchByte(op.Flda));
end;

procedure T1005CommCPU.TK;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'TK OP2 LSL < MSL');
    FMemory.StoreByte(op.Fldc, op.Flda.Col);
    op.Fldc.Decrement;
    if (op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr) then
    begin
        FMemory.StoreByte(op.Fldc, op.Flda.Row);
        while (not op.Fldc.Matches(op.Fldb)) do
        begin
            op.Fldc.Decrement;
            FMemory.StoreByte(op.Fldc, X3_SPACE);
        end;
    end;
end;

procedure T1005CommCPU.TN;
var
    op: T1005CommOperands;
begin
    op := FMemory.FetchCommOperands;
    op.Fldc.AdjustBankAsc(op.Fldb);
    Assert(op.Fldc.SequentialAddr >= op.Fldb.SequentialAddr, 'TN OP2 LSL < MSL');
    while (not op.Fldc.Matches(op.Fldb)) do
    begin
        FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda) and $0f);
        Op.Flda.Decrement;
        op.Fldc.Decrement;
    end;
    FMemory.StoreByte(op.Fldc, FMemory.FetchByte(op.Flda) and $0f);
end;

procedure T1005CommCPU.TR;
var
    op: T1005CommOperands;
    b, row, col: Byte;
begin
    op := FMemory.FetchCommOperands;
    while (not op.Fldb.Matches(op.Fldc)) do
    begin
        // Determine the address of the translated value using the 1005's weird translate table
        // addressing scheme. Translate table must be in bank 4.
        b := FMemory.FetchByte(op.Fldb);
        if (b = $1f) then
        begin
            row := 28;
            col := 30;
        end else if (b = $3f) then
        begin
            row := 28;
            col := 31;
        end else if ((b and $20) = 0) then
        begin
            row := 29;
            col := 1;
        end else
        begin
            row := 30;
            col := 1;
        end;
        op.Flda.SetAddr(4, row, col);
        if (col = 1) then
            op.Flda.SetAddr(op.Flda.Row, b or $20);
        // Translate 1 byte
        b := FMemory.FetchByte(op.Flda);
        FMemory.StoreByte(op.Fldb, b);
        op.Fldb.Increment;
    end;
end;

procedure T1005CommCPU.TX;
var
    op: T1005CommOperands;
    idx: Integer;
begin
    op := FMemory.FetchCommOperands;
    idx := 31;
    while ((idx > 0) and (op.Flda.SequentialAddr > op.Fldb.SequentialAddr)) do
    begin
        FMemory.StoreCommX(idx, FMemory.FetchByte(op.Flda));
        op.Flda.Decrement;
        Dec(idx);
    end;
    FMemory.StoreCommX(idx, FMemory.FetchByte(op.Flda));
    Dec(idx);
    while (idx > 0) do
    begin
        FMemory.StoreCommX(idx, X3_SPACE);
        Dec(idx);
    end;
end;

end.
