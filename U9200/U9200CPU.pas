unit U9200CPU;

interface

uses Windows, SysUtils, Classes, Generics.Collections, Generics.Defaults, Forms, U9200Memory, U9200Types,
    FmtBcd, Bcd, U9200IPC, U9200Mux;

type
    TDebugEvent = procedure(Sender: TObject; E: Exception) of object;

    TU92CPU = class(TObject)
    private
        FMemory: TU92Memory;
        FIPC: TU92IPC;
        FMux: TU92Mux;
        FState: TU92CPUState;
        FOpcodes: TU92OpcodeList;
        FHPRCode: Smallint;
        FOnError: TErrorEvent;
        FOnExecuteInstruction: TExecuteInstructionEvent;
        FOnFetchInstruction: TFetchInstructionEvent;
        FOnDebug: TDebugEvent;
        FOnHalt: THaltEvent;
        FRestrictAlter: Boolean;
        procedure CheckInterrupt;
        procedure ExecuteInstruction;
        function GetAddress(val: Smallint): Smallint;
        function GetRegister(val: Byte): Byte;
        function PackedCC(value: TBcd; len: Smallint): Byte;
        procedure PackedOperands(fal: Byte; fad1, fad2: Smallint;
          var l1, op1: Smallint; var bcd1: TBcd;
          var l2, op2: Smallint; var bcd2: TBcd);
        function SumCC(sum: Smallint; ovfl: Boolean): Byte;
        function SumCheckOvfl(op1, op2: Integer; var sum: Smallint): Boolean;
        procedure AH(fal: Byte; fad1, fad2: Smallint);
        procedure AI(fal: Byte; fad1, fad2: Smallint);
        procedure AP(fal: Byte; fad1, fad2: Smallint);
        procedure BAL(fal: Byte; fad1, fad2: Smallint);
        procedure BC(fal: Byte; fad1, fad2: Smallint);
        procedure CH(fal: Byte; fad1, fad2: Smallint);
        procedure CLC(fal: Byte; fad1, fad2: Smallint);
        procedure CLI(fal: Byte; fad1, fad2: Smallint);
        procedure CP(fal: Byte; fad1, fad2: Smallint);
        procedure DP(fal: Byte; fad1, fad2: Smallint);
        procedure ED(fal: Byte; fad1, fad2: Smallint);
        procedure HPR(fal: Byte; fad1, fad2: Smallint);
        procedure LH(fal: Byte; fad1, fad2: Smallint);
        procedure LPSC(fal: Byte; fad1, fad2: Smallint);
        procedure MP(fal: Byte; fad1, fad2: Smallint);
        procedure MVC(fal: Byte; fad1, fad2: Smallint);
        procedure MVI(fal: Byte; fad1, fad2: Smallint);
        procedure MVN(fal: Byte; fad1, fad2: Smallint);
        procedure MVO(fal: Byte; fad1, fad2: Smallint);
        procedure NC(fal: Byte; fad1, fad2: Smallint);
        procedure NI(fal: Byte; fad1, fad2: Smallint);
        procedure NOP(fal: Byte; fad1, fad2: Smallint);
        procedure OC(fal: Byte; fad1, fad2: Smallint);
        procedure OI(fal: Byte; fad1, fad2: Smallint);
        procedure PACK(fal: Byte; fad1, fad2: Smallint);
        procedure SH(fal: Byte; fad1, fad2: Smallint);
        procedure STH(fal: Byte; fad1, fad2: Smallint);
        procedure SP(fal: Byte; fad1, fad2: Smallint);
        procedure SPSC(fal: Byte; fad1, fad2: Smallint);
        procedure SRC(fal: Byte; fad1, fad2: Smallint);
        procedure TIO(fal: Byte; fad1, fad2: Smallint);
        procedure TM(fal: Byte; fad1, fad2: Smallint);
        procedure TR(fal: Byte; fad1, fad2: Smallint);
        procedure UNPK(fal: Byte; fad1, fad2: Smallint);
        procedure XIOF(fal: Byte; fad1, fad2: Smallint);
        procedure ZAP(fal: Byte; fad1, fad2: Smallint);
    public
        constructor Create(mem: TU92Memory; ipc: TU92IPC; mux: TU92Mux);
        destructor Destroy; override;
        procedure Clear;
        procedure ClearInstructionFetched;
        procedure FetchInstruction;
        procedure SetIOState;
        procedure SetProcState;
        procedure SetSingleStep(step: Boolean);
        procedure Start;
        property HPRCode: Smallint read FHPRCode;
        property OnDebug: TDebugEvent read FOnDebug write FOnDebug;
        property OnError: TErrorEvent read FOnError write FOnError;
        property OnExecuteInstruction: TExecuteInstructionEvent read FOnExecuteInstruction write FOnExecuteInstruction;
        property OnFetchInstruction: TFetchInstructionEvent read FOnFetchInstruction write FOnFetchInstruction;
        property OnHalt: THaltEvent read FOnHalt write FOnHalt;
        property RestrictAlter: Boolean read FRestrictAlter;
        property State: TU92CPUState read FState;
    end;

implementation

uses Math;

{ TU92CPU }

procedure TU92CPU.AH(fal: Byte; fad1, fad2: Smallint);
var
    reg: Byte;
    ad: Smallint;
    sum: Smallint;
    ovfl: Boolean;
begin
    reg := GetRegister(fal);
    ad := GetAddress(fad1);
    ovfl := SumCheckOvfl(FMemory.R[FState, reg], FMemory.FetchHalfWord(ad), sum);
    FMemory.R[FState, reg] := sum;
    FMemory.CC[FState] := SumCC(sum, ovfl);
end;

procedure TU92CPU.AI(fal: Byte; fad1, fad2: Smallint);
var
    op1: Smallint;
    ad: Smallint;
    sum: Smallint;
    ovfl: Boolean;
begin
    op1 := fal;
    if ((fal and $80) <> 0) then
        op1 := op1 or Smallint($FF00);
    ad := GetAddress(fad1);
    ovfl := SumCheckOvfl(op1, FMemory.FetchHalfWord(ad), sum);
    FMemory.StoreHalfWord(ad, sum);
    FMemory.CC[FState] := SumCC(sum, ovfl);
end;

procedure TU92CPU.AP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    bcd1, bcd2, rslt: TBcd;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
    rslt := bcd1 + bcd2;
    FMemory.StorePacked(FMemory.ASCII[FState], op1, l1, rslt);
    FMemory.CC[FState] := PackedCC(rslt, l1);
end;

procedure TU92CPU.BAL(fal: Byte; fad1, fad2: Smallint);
var
    reg: Smallint;
begin
    reg := GetRegister(fal);
    FMemory.R[FState, reg] := FMemory.FAP[FState];
    FMemory.FAP[FState] := GetAddress(fad1);
end;

procedure TU92CPU.BC(fal: Byte; fad1, fad2: Smallint);
var
    test: Byte;
    CC: Byte;
    jump: Boolean;
    tgt: Smallint;
begin
    jump := False;
    test := (fal and $F0);
    CC := FMemory.CC[FState];
    case CC of
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
    begin
        tgt := GetAddress(fad1);
        FMemory.CheckHalfWordAddress(tgt);
        FMemory.FAP[FState] := tgt;
    end;
end;

procedure TU92CPU.CH(fal: Byte; fad1, fad2: Smallint);
var
    reg: Byte;
    ad: Smallint;
    op1, op2: Smallint;
    CC: Byte;
begin
    reg := GetRegister(fal);
    ad := GetAddress(fad1);
    op1 := FMemory.R[FState, reg];
    op2 := FMemory.FetchHalfWord(ad);
    if (op1 = op2) then
        CC := 0
    else if (op1 < op2) then
        CC := 1
    else
        CC := 2;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.CheckInterrupt;
var
    dev, status: Byte;
begin
    if (ucsProcessor in FState) then
    begin
        if (FIPC.IntPending(dev, status)) then
        begin
            FState := FState - [ucsProcessor] + [ucsIO];
            FMemory.DS := status;
            FMemory.DA := dev;
            FIPC.IntClear(dev);
        end else if (FMux.IntPending(dev, status)) then
        begin
            FState := FState - [ucsProcessor] + [ucsIO];
            FMemory.DS := status;
            FMemory.DA := dev;
            FMux.IntClear(dev);
        end;
    end;
end;

procedure TU92CPU.CLC(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    op1, op2: Smallint;
    c1, c2: Byte;
    CC: Byte;
begin
    CC := 0;
    len := fal + 1;
    op1 := GetAddress(fad1);
    op2 := GetAddress(fad2);
    while (len > 0) do
    begin
        c1 := FMemory.FetchByte(op1);
        c2 := FMemory.FetchByte(op2);
        if (c1 < c2) then
        begin
            CC := 1;
            Break;
        end
        else if (c1 > c2) then
        begin
            CC := 2;
            Break;
        end;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.Clear;
begin
    FMemory.FAF[[ucsProcessor]] := FMemory.FAF[[ucsIO]];
    FMemory.fal[[ucsProcessor]] := FMemory.fal[[ucsIO]];
    FMemory.fad1[[ucsProcessor]] := FMemory.fad1[[ucsIO]];
    FMemory.fad2[[ucsProcessor]] := FMemory.fad2[[ucsIO]];
    FState := [ucsIO, ucsStalled, ucsInstructionFetched];
    FRestrictAlter := False;
end;

procedure TU92CPU.ClearInstructionFetched;
begin
    FState := FState - [ucsInstructionFetched];
end;

procedure TU92CPU.CLI(fal: Byte; fad1, fad2: Smallint);
var
    op1: Byte;
    op2: Byte;
    ad: Smallint;
    CC: Byte;
begin
    op1 := fal;
    ad := GetAddress(fad1);
    op2 := FMemory.FetchByte(ad);
    if (op2 = op1) then
        CC := 0
    else if (op2 < op1) then
        CC := 1
    else
        CC := 2;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.CP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    CC: Byte;
    bcd1, bcd2: TBcd;
    rslt: Integer;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
    CC := 0;
    rslt := BCDCompare(bcd1, bcd2);
    if (rslt < 0) then
        CC := 1
    else if (rslt > 0) then
        CC := 2;
    FMemory.CC[FState] := CC;
end;

constructor TU92CPU.Create(mem: TU92Memory; ipc: TU92IPC; mux: TU92Mux);

begin
    inherited Create;
    FMemory := mem;
    FIPC := ipc;
    FMux := mux;
    FState := [ucsIO, ucsStalled];
    FOpcodes := TU92OpcodeList.Create;
    FOpcodes.Add(Opcode($00, 4, NOP));
    FOpcodes.Add(Opcode($40, 4, STH));
    FOpcodes.Add(Opcode($45, 4, BAL));
    FOpcodes.Add(Opcode($47, 4, BC));
    FOpcodes.Add(Opcode($48, 4, LH));
    FOpcodes.Add(Opcode($49, 4, CH));
    FOpcodes.Add(Opcode($91, 4, TM));
    FOpcodes.Add(Opcode($92, 4, MVI));
    FOpcodes.Add(Opcode($94, 4, NI));
    FOpcodes.Add(Opcode($95, 4, CLI));
    FOpcodes.Add(Opcode($96, 4, OI));
    FOpcodes.Add(Opcode($A0, 4, SPSC));
    FOpcodes.Add(Opcode($A1, 4, SRC));
    FOpcodes.Add(Opcode($A4, 4, XIOF));
    FOpcodes.Add(Opcode($A5, 4, TIO));
    FOpcodes.Add(Opcode($A6, 4, AI));
    FOpcodes.Add(Opcode($A8, 4, LPSC));
    FOpcodes.Add(Opcode($A9, 4, HPR));
    FOpcodes.Add(Opcode($AA, 4, AH));
    FOpcodes.Add(Opcode($AB, 4, SH));
    FOpcodes.Add(Opcode($D1, 6, MVN));
    FOpcodes.Add(Opcode($D2, 6, MVC));
    FOpcodes.Add(Opcode($D4, 6, NC));
    FOpcodes.Add(Opcode($D5, 6, CLC));
    FOpcodes.Add(Opcode($D6, 6, OC));
    FOpcodes.Add(Opcode($DC, 6, TR));
    FOpcodes.Add(Opcode($DE, 6, ED));
    FOpcodes.Add(Opcode($F1, 6, MVO));
    FOpcodes.Add(Opcode($F2, 6, PACK));
    FOpcodes.Add(Opcode($F3, 6, UNPK));
    FOpcodes.Add(Opcode($F8, 6, ZAP));
    FOpcodes.Add(Opcode($F9, 6, CP));
    FOpcodes.Add(Opcode($FA, 6, AP));
    FOpcodes.Add(Opcode($FB, 6, SP));
    FOpcodes.Add(Opcode($FC, 6, MP));
    FOpcodes.Add(Opcode($FD, 6, DP));
end;

destructor TU92CPU.Destroy;
begin
    FreeAndNil(FOpcodes);
    inherited;
end;

procedure TU92CPU.DP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    op1Sign: Byte;
    rsltLen: Smallint;
    bcd1, bcd2, rslt: TBcd;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
//    if (l1 < l2) then
//        raise EDecimalError.Create('Illegal l1');
    if (bcd2 = NullBcd) then
        raise EDecimalError.Create('Divide by zero');
    // Calculate and save the quotient
    op1Sign := bcd1.SignSpecialPlaces and $80;
    rslt := bcd1 / bcd2;
    rsltLen := l1 - l2 - 1;
    FMemory.StorePacked(FMemory.ASCII[FState], op1, rsltLen, rslt);
    // Calculate and save the remainder.
    // First get rid of the fractional part of the result by setting
    // the precision of the result to be the number of places left of
    // the decimal and setting the scale to zero by clearing the rightmost
    // 6 bits of SignSpecialPlaces.
    rslt.Precision := BcdPrecision(rslt);
    rslt.SignSpecialPlaces := rslt.SignSpecialPlaces and $C0;
    rslt := bcd1 - (bcd2 * rslt);
    if (op1Sign = 0) then
        rslt.SignSpecialPlaces := rslt.SignSpecialPlaces and $7F
    else
        rslt.SignSpecialPlaces := rslt.SignSpecialPlaces or $80;
    FMemory.StorePacked(FMemory.ASCII[FState], op1 + rsltLen + 1, l2, rslt);
end;

procedure TU92CPU.ED(fal: Byte; fad1, fad2: Smallint);
const
    DSB = $20;
    SSB = $21;
    FSB = $22;
var
    l1: Smallint;
    op1, op2: Smallint;
    digit: Byte;
    fill, zone: Byte;
    sIndicator: Boolean;
    mask: Byte;
    shift: Byte;
    b: Byte;
    CC: Byte;
    allZero: Boolean;
begin
    CC := 0;
    allZero := True;
    l1 := fal;
    op1 := GetAddress(fad1);
    op2 := GetAddress(fad2);
    if ((FMemory.FAS[FState] and $20) <> 0) then
        zone := $50 // ASCII mode
    else
        zone := $F0; // EBCDIC
    fill := FMemory.FetchByte(op1);
    Inc(op1);
    Dec(l1);
    sIndicator := False;
    mask := $F0;
    shift := 4;
    while (l1 >= 0) do
    begin
        // Fetch the next nibble
        digit := (FMemory.FetchByte(op2) and mask) shr shift;
        if ((digit = $0A) or (digit = $0C) or (digit = $0E) or (digit = $0F)) then
        begin
            // OP2 is positive, turn sIndicator off and set condition to 2
            sIndicator := False;
            CC := 2;
            Inc(op2);
            mask := $F0;
            shift := 4;
            Continue;
        end else if (digit > $09) then
        begin
            // OP2 is negative, set condition to 1
            CC := 1;
            Inc(op2);
            mask := $F0;
            shift := 4;
            Continue;
        end else if (digit <> 0) then
            allZero := False;
        b := FMemory.FetchByte(op1);
        case b of
          DSB,
          SSB:
          begin
            if (digit <= 9) then
            begin
                if (digit <> 0) then
                    sIndicator := True;
                if (sIndicator) then
                    FMemory.StoreByte(op1, digit or zone)
                else if (digit = 0) then
                    FMemory.StoreByte(op1, fill)
                else
                    FMemory.StoreByte(op1, digit or zone);
                if (b = SSB) then
                    sIndicator := True;
            end;
            mask := mask xor $FF;
            shift := shift xor 4;
            if (mask = $F0) then
                Inc(op2);
          end;
          FSB:
          begin
            sIndicator := False;
            CC := 0;
            allZero := True;
            FMemory.StoreByte(op1, fill);
          end
          else
          begin
            if (not sIndicator) then
                FMemory.StoreByte(op1, fill);
          end;
        end;
        Inc(op1);
        Dec(l1);
    end;
    if (allZero) then
        CC := 0;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.ExecuteInstruction;
var
    op: Byte;
    Opcode: TU92Opcode;
    fal: Byte;
    fad1, fad2: Smallint;
begin
    FState := FState - [ucsInstructionFetched];

    if (Assigned(FOnExecuteInstruction)) then
        FOnExecuteInstruction(Self);

    if (Assigned(FOnDebug)) then
        FOnDebug(Self, nil);

    op := FMemory.FAF[[ucsProcessor]];
    Opcode := FOpcodes.FindOpcode(op);
    fal := FMemory.fal[[ucsProcessor]];
    fad1 := FMemory.fad1[[ucsProcessor]];
    fad2 := FMemory.fad2[[ucsProcessor]];
    Opcode.Proc(fal, fad1, fad2);
end;

procedure TU92CPU.FetchInstruction;
var
    pc: Smallint;
    op: Byte;
    Opcode: TU92Opcode;
begin
    if (ucsInstructionFetched in FState) then
        Exit;

    pc := FMemory.FAP[FState];
    op := FMemory.FetchByte(pc);
    FMemory.FAF[[ucsProcessor]] := op;
    Opcode := FOpcodes.FindOpcode(op);
    Inc(pc);
    FMemory.FAL[[ucsProcessor]] := FMemory.FetchByte(pc);
    Inc(pc);
    FMemory.FAD1[[ucsProcessor]] := FMemory.FetchHalfWord(pc);
    Inc(pc, 2);
    if (Opcode.Length = 6) then
    begin
        FMemory.fad2[[ucsProcessor]] := FMemory.FetchHalfWord(pc);
        Inc(pc, 2);
    end;
    FMemory.FAP[FState] := pc;

    if (Assigned(FOnFetchInstruction)) then
        FOnFetchInstruction(Self);

    FState := FState + [ucsInstructionFetched];
end;

function TU92CPU.GetAddress(val: Smallint): Smallint;
var
    reg: Byte;
begin
    if ((val and $8000) <> 0) then
    begin
        reg := GetRegister((val and $F000) shr 8);
        Result := FMemory.R[FState, reg] + (val and $FFF);
    end
    else
        Result := val;
end;

function TU92CPU.GetRegister(val: Byte): Byte;
begin
    Result := (val and $F0) shr 4;
    if (Result < 8) then
        raise EProcessorError.Create('Illegal register');
end;

procedure TU92CPU.HPR(fal: Byte; fad1, fad2: Smallint);
begin
    FHPRCode := GetAddress(fad1);
    FState := FState + [ucsHalted];
    if (Assigned(FOnHalt)) then
        FOnHalt(Self);
end;

procedure TU92CPU.LH(fal: Byte; fad1, fad2: Smallint);
var
    reg: Byte;
    ad: Smallint;
begin
    reg := GetRegister(fal);
    ad := GetAddress(fad1);
    FMemory.R[FState, reg] := FMemory.FetchHalfWord(ad);
end;

procedure TU92CPU.LPSC(fal: Byte; fad1, fad2: Smallint);
var
    options: Byte;
    op1: Smallint;
    state: TU92CPUState;
    pscAddr: Smallint;
begin
    options := fal;
    op1 := GetAddress(fad1);
    if ((options and $20) <> 0) then
    begin
        state := [ucsIO];
        pscAddr := $10;
    end else
    begin
        state := [ucsProcessor];
        pscAddr := $00;
    end;
    if ((options and $40) = $40) then
    begin
        FMemory.StoreHalfWord(pscAddr, FMemory.FetchHalfWord(op1));
        FMemory.StoreHalfWord(pscAddr + 2, FMemory.FetchHalfWord(op1 + 2));
    end;
    if ((options and $80) = $80) then
        FMemory.ASCII[state] := False;
    if ((options and $C0) = $C0) then
        FMemory.ASCII[state] := True;
    if ((options and $10) = $10) then
        FState := FState + [ucsIO] - [ucsProcessor]
    else
        FState := FState + [ucsProcessor] - [ucsIO];
    if ((options and $80) = $80) then
        FRestrictAlter := True;
    if ((options and $40) = $40) then
        FRestrictAlter := False;
    FState := FState - [ucsInstructionFetched];
end;

procedure TU92CPU.MP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    bcd1, bcd2, rslt: TBcd;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
    rslt := bcd1 * bcd2;
    FMemory.StorePacked(FMemory.ASCII[FState], op1, l1, rslt);
end;

procedure TU92CPU.MVC(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    SRC, dest: Smallint;
begin
    len := fal;
    SRC := GetAddress(fad2);
    dest := GetAddress(fad1);
    while (len >= 0) do
    begin
        FMemory.StoreByte(dest, FMemory.FetchByte(SRC));
        Inc(SRC);
        Inc(dest);
        Dec(len);
    end;
end;

procedure TU92CPU.MVI(fal: Byte; fad1, fad2: Smallint);
var
    op1: Byte;
    ad: Smallint;
begin
    op1 := fal;
    ad := GetAddress(fad1);
    FMemory.StoreByte(ad, op1);
end;

procedure TU92CPU.MVN(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    SRC, dest: Smallint;
    rslt: Byte;
begin
    len := fal;
    SRC := GetAddress(fad2);
    dest := GetAddress(fad1);
    while (len >= 0) do
    begin
        rslt := (FMemory.FetchByte(dest) and $F0) or (FMemory.FetchByte(SRC) and $0F);
        FMemory.StoreByte(dest, rslt);
        Inc(SRC);
        Inc(dest);
        Dec(len);
    end;
end;

procedure TU92CPU.MVO(fal: Byte; fad1, fad2: Smallint);
var
    dlen, slen: Smallint;
    dest, src: Smallint;
    db, sb: Byte;
    dcount, scount: Byte;
begin
    dlen := ((fal and $F0) shr 4);
    slen := (fal and $0F);
    dest := GetAddress(fad1) + dlen;
    src := GetAddress(fad2) + slen;
    // Initialize current dest by with least significant 4-bit
    // of LSB of dest and least signifcant 4 bits of src.
    db := FMemory.FetchByte(dest);
    sb := FMemory.FetchByte(src);
    db := (db and $0f) or ((sb and $0f) shl 4);
    dcount := 2;                // show destination byte full
    scount := 1;                // show source byte half full
    sb := sb shr 4;
    Dec(src);
    Dec(slen);
    while (dlen >= 0) do
    begin
        // save completed dest byte
        if (dcount = 2) then
        begin
            FMemory.StoreByte(dest, db);
            Dec(dest);
            Dec(dlen);
            db := 0;
            dcount := 0;
        end;
        // fetch the next source byte. Zero if source exhausted
        if (scount = 0) then
        begin
            if (slen >= 0) then
            begin
                sb := FMemory.FetchByte(src);
                Dec(src);
                Dec(slen);
            end else
                sb := 0;
            scount := 2;
        end;
        // Shift 4 bits of source into dest
        db := (db shr 4) or ((sb and $0f) shl 4);
        Inc(dcount);
        sb := sb shr 4;
        Dec(scount);
    end;
end;

procedure TU92CPU.NC(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    op1: Smallint;
    op2: Smallint;
    CC: Byte;
    rslt: Byte;
begin
    len := fal;
    op1 := GetAddress(fad1);
    op2 := GetAddress(fad2);
    CC := 0;
    while (len >= 0) do
    begin
        rslt := FMemory.FetchByte(op1) and FMemory.FetchByte(op2);
        FMemory.StoreByte(op1, rslt);
        if (rslt <> 0) then
            CC := 1;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.NI(fal: Byte; fad1, fad2: Smallint);
var
    op1: Byte;
    ad: Smallint;
    rslt: Byte;
    CC: Byte;
begin
    op1 := fal;
    ad := GetAddress(fad1);
    rslt := op1 and FMemory.FetchByte(ad);
    FMemory.StoreByte(ad, rslt);
    if (rslt = 0) then
        CC := 0
    else
        CC := 1;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.NOP(fal: Byte; fad1, fad2: Smallint);
begin
    // NOP - do nothing
    ;
end;

procedure TU92CPU.OC(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    op1: Smallint;
    op2: Smallint;
    CC: Byte;
    rslt: Byte;
begin
    len := fal;
    op1 := GetAddress(fad1);
    op2 := GetAddress(fad2);
    CC := 0;
    while (len >= 0) do
    begin
        rslt := FMemory.FetchByte(op1) or FMemory.FetchByte(op2);
        FMemory.StoreByte(op1, rslt);
        if (rslt <> 0) then
            CC := 1;
        Inc(op1);
        Inc(op2);
        Dec(len);
    end;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.OI(fal: Byte; fad1, fad2: Smallint);
var
    op1: Byte;
    ad: Smallint;
    rslt: Byte;
    CC: Byte;
begin
    op1 := fal;
    ad := GetAddress(fad1);
    rslt := op1 or FMemory.FetchByte(ad);
    FMemory.StoreByte(ad, rslt);
    if (rslt = 0) then
        CC := 0
    else
        CC := 1;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.PACK(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    b: Byte;
begin
    l1 := ((fal and $F0) shr 4);
    l2 := (fal and $0F);
    op1 := GetAddress(fad1) + l1;
    op2 := GetAddress(fad2) + l2;
    b := FMemory.FetchByte(op2);
    FMemory.StoreByte(op1, ((b and $0F) shl 4) or ((b and $F0) shr 4));
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
            b := FMemory.FetchByte(op2) and $0F;
            Dec(op2);
            Dec(l2);
            if (l2 >= 0) then
            begin
                b := b or ((FMemory.FetchByte(op2) and $0F) shl 4);
                Dec(op2);
                Dec(l2);
            end;
        end else
            b := 0;
        FMemory.StoreByte(op1, b);
        Dec(op1);
        Dec(l1);
    end;
end;

function TU92CPU.PackedCC(value: TBcd; len: Smallint): Byte;
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

procedure TU92CPU.PackedOperands(fal: Byte; fad1, fad2: Smallint; var l1, op1: Smallint; var bcd1: TBcd;
  var l2, op2: Smallint; var bcd2: TBcd);
begin
    l1 := ((fal and $F0) shr 4);
    l2 := (fal and $0F);
//    if (l1 < l2) then
//        raise EDecimalError.Create('Illegal L1');
    op1 := GetAddress(fad1);
    op2 := GetAddress(fad2);
    bcd1 := FMemory.FetchPacked(FMemory.ASCII[FState], op1, l1);
    bcd2 := FMemory.FetchPacked(FMemory.ASCII[FState], op2, l2);
end;

procedure TU92CPU.SetIOState;
begin
    FState := FState - [ucsProcessor] + [ucsIO];
end;

procedure TU92CPU.SetProcState;
begin
    FState := (FState + [ucsProcessor]) - [ucsIO];
end;

procedure TU92CPU.SetSingleStep(step: Boolean);
begin
    if (step) then
        FState := FState + [ucsSingleStep]
    else
        FState := FState - [ucsSingleStep];
end;

procedure TU92CPU.SH(fal: Byte; fad1, fad2: Smallint);
var
    reg: Byte;
    ad: Smallint;
    sum: Smallint;
    ovfl: Boolean;
begin
    reg := GetRegister(fal);
    ad := GetAddress(fad1);
    ovfl := SumCheckOvfl(FMemory.R[FState, reg], -FMemory.FetchHalfWord(ad), sum);
    FMemory.R[FState, reg] := sum;
    FMemory.CC[FState] := SumCC(sum, ovfl);
end;

procedure TU92CPU.SP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    bcd1, bcd2, rslt: TBcd;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
    rslt := bcd1 - bcd2;
    FMemory.StorePacked(FMemory.ASCII[FState], op1, l1, rslt);
    FMemory.CC[FState] := PackedCC(rslt, l1);
end;

procedure TU92CPU.SPSC(fal: Byte; fad1, fad2: Smallint);
var
    options: Byte;
    op1: Smallint;
    pscAddr: Smallint;
begin
    options := fal;
    op1 := GetAddress(fad1);
    if ((options and $20) <> 0) then
        pscAddr := $10
    else
        pscAddr := $00;
    FMemory.StoreHalfWord(op1, FMemory.FetchHalfWord(pscAddr));
    FMemory.StoreHalfWord(op1 + 2, FMemory.FetchHalfWord(pscAddr + 2));
end;

procedure TU92CPU.SRC(fal: Byte; fad1, fad2: Smallint);
begin
    FMemory.SRC := fal;
    if (ucsProcessor in FState) then
        FState := FState - [ucsProcessor] + [ucsIO];
end;

procedure TU92CPU.Start;
begin
    FState := FState - [ucsStalled, ucsHalted];
    try
        CheckInterrupt;
        FetchInstruction;
        repeat
            ExecuteInstruction;
            CheckInterrupt;
            FetchInstruction;
        until ((FState * [ucsSingleStep, ucsHalted, ucsStalled, ucsError]) <> []);
    except
        on E: Exception do
        begin
            if (Assigned(FOnError)) then
                FOnError(Self, E);
            if (Assigned(FOnDebug)) then
                FOnDebug(Self, E);
            FState := FState + [ucsError];
//            Application.ShowException(E);
        end;
    end;
end;

procedure TU92CPU.STH(fal: Byte; fad1, fad2: Smallint);
var
    reg: Byte;
    ad: Smallint;
begin
    reg := GetRegister(fal);
    ad := GetAddress(fad1);
    FMemory.StoreHalfWord(ad, FMemory.R[FState, reg]);
end;

function TU92CPU.SumCC(sum: Smallint; ovfl: Boolean): Byte;
begin
    if (ovfl) then
        Result := 3
    else if (sum = 0) then
        Result := 0
    else if (sum < 0) then
        Result := 1
    else
        Result := 2;
end;

function TU92CPU.SumCheckOvfl(op1, op2: Integer; var sum: Smallint): Boolean;
// Return the sum of two half words, checking for overflow
begin
    sum := op1 + op2;
    Result := (((op1 > 0) and (op2 > 0) and (sum < 0)) or
               ((op1 < 0) and (op2 < 0) and (sum > 0)));
end;

procedure TU92CPU.TIO(fal: Byte; fad1, fad2: Smallint);
var
    dev: Byte;
    op1: Smallint;
    cc: Byte;
    stat: Byte;
begin
    dev := fal;
    op1 := GetAddress(fad1);
    FIPC.TestIO(dev, stat, cc);
    if (cc = 3) then
        FMux.TestIO(dev, stat, cc);
    FMemory.StoreByte(op1, stat);
    FMemory.CC[FState] := cc;
end;

procedure TU92CPU.TM(fal: Byte; fad1, fad2: Smallint);
var
    mask: Byte;
    ad: Smallint;
    rslt: Byte;
    CC: Byte;
begin
    mask := fal;
    if (mask = 0) then
        CC := 0
    else
    begin
        ad := GetAddress(fad1);
        rslt := mask and FMemory.FetchByte(ad);
        if (rslt = 0) then
            CC := 0
        else if (rslt = mask) then
            CC := 3
        else
            CC := 1;
    end;
    FMemory.CC[FState] := CC;
end;

procedure TU92CPU.TR(fal: Byte; fad1, fad2: Smallint);
var
    len: Smallint;
    data: Smallint;
    table: Smallint;
begin
    len := fal;
    data := GetAddress(fad1);
    table := GetAddress(fad2);
    while (len >= 0) do
    begin
        FMemory.StoreByte(data, FMemory.FetchByte(table + FMemory.FetchByte(data)));
        Inc(data);
        Dec(len);
    end;
end;

procedure TU92CPU.UNPK(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    b: Byte;
    zone: Byte;
begin
    l1 := ((fal and $F0) shr 4);
    l2 := (fal and $0F);
    op1 := GetAddress(fad1) + l1;
    op2 := GetAddress(fad2) + l2;
    b := FMemory.FetchByte(op2);
    FMemory.StoreByte(op1, ((b and $0F) shl 4) or ((b and $F0) shr 4));
    Dec(op1);
    Dec(op2);
    Dec(l1);
    Dec(l2);
    if ((FMemory.FAS[FState] and $20) <> 0) then
        zone := $50 // ASCII mode
    else
        zone := $F0; // EBCDIC
    while ((l1 >= 0) or (l2 >= 0)) do
    begin
        if (l1 < 0) then
            Break;
        if (l2 >= 0) then
        begin
            b := FMemory.FetchByte(op2);
            Dec(op2);
            Dec(l2);
        end
        else
            b := 0;
        FMemory.StoreByte(op1, (b and $0F) or zone);
        Dec(op1);
        Dec(l1);
        if (l1 >= 0) then
        begin
            FMemory.StoreByte(op1, ((b and $F0) shr 4) or zone);
            Dec(op1);
            Dec(l1);
        end;
    end;
end;

procedure TU92CPU.XIOF(fal: Byte; fad1, fad2: Smallint);
var
    dev: Byte;
    cmd: Smallint;
    cc: Byte;
begin
    dev := fal;
    cmd := fad1;
    cc := FIPC.StartIO(dev, cmd);
    if (cc = 3) then
        cc := FMux.StartIO(dev, cmd);
    FMemory.CC[FState] := cc;
end;

procedure TU92CPU.ZAP(fal: Byte; fad1, fad2: Smallint);
var
    l1, l2: Smallint;
    op1, op2: Smallint;
    bcd1, bcd2: TBcd;
begin
    PackedOperands(fal, fad1, fad2, l1, op1, bcd1, l2, op2, bcd2);
    FMemory.StorePacked(FMemory.ASCII[FState], op1, l1, bcd2);
    FMemory.CC[FState] := PackedCC(bcd2, l1);
end;

end.
