unit U1005Memory;

interface

uses SysUtils, FmtBcd, Bcd, U1005Types;

const
  // Addresses of well known memory locations
  READER_BUFFER = 1;
  PRINTER_BUFFER = 161;
  PUNCH_BUFFER = 293;
  // Federal Systems inidicator bits
  IND_EQUAL = 0;
  IND_ZERO = 0;
  IND_NOT_EQUAL = 1;
  IND_LESS = 1;
  IND_POSITIVE = 1;
  IND_GREATER = 2;
  IND_NEGATIVE = 2;
  // Commercial Systems condition code bits
  CC_FORM_OVERFLOW = $80000000;
  CC_ARITHMETIC_OVERFLOW = $40000000;
  CC_END_OF_TAPE = $C0000000;
  CC_SENSE2 = $20000000;
  CC_SENSE1 = $10000000;
  CC_ALTERNATE_HOLD2 = $08000000;
  CC_ALTERNATE_HOLD1 = $04000000;
  CC_INTERUUPT = $02000000;
  CC_UNIT_ALERT = $01000000;
  CC_PARITY_ERROR = $00800000;
  CC_SIGN_PLUS = $00400000;
  CC_SIGN_ZERO = $00200000;
  CC_SIGN_MINUS = $00100000;
  CC_SIGNS = $00700000;
  CC_EQUAL = $00080000;
  CC_GREATER_THAN = $00040000;
  CC_LESS_THAN = $00020000;
  CC_NOT_EQUAL = CC_LESS_THAN;
  CC_TESTS = $000E0000;
  CC_EVEN_PARITY = $00010000;
  CC_PPT = $00008000;
  CC_SERVO1 = $00004000;
  CC_INDICATOR1 = $00002000;
  CC_INDICATOR2 = $00001000;

type
  T1005Memory = class(TObject)
  private
    FSystemType: T1005SystemType;
    // 1005 memory consists of 4 banks with 32 rows of 32 6-bit bytes of memory.
    FMemory: array [1..4, 1..32, 1..32] of Byte;
  public
    constructor Create(st: T1005SystemType); reintroduce;
    procedure Clear;
    function FetchAR1(i: Integer): Byte; inline;
    function FetchAR1Bcd(len: Integer): TBcd;
    function FetchAR2(i: Integer): Byte; inline;
    function FetchAR2Bcd(len: Integer): TBcd;
    function FetchBcd(addr: I1005Addr; len: Integer): TBcd;
    function FetchByte(addr: I1005Addr): Byte; overload;
    function FetchByte(bank, row, col: Byte): Byte; overload;
    function FetchCommOpcode: Byte; inline;
    function FetchCommOperands: T1005CommOperands; inline;
    function FetchCommX(i: Integer): Byte; inline;
    function FetchFedSysOpcode: Byte; inline;
    function FetchFedSysOperand: T1005FedSysOperand; inline;
    function FedFedSysOperandRaw: T1005FedSysOperand; inline;
    function FetchFedSysX: I1005Addr; inline;
    function FetchICC: I1005Addr; inline;
    function FetchIndicator: Byte; inline;
    function FetchPAK: I1005Addr; inline;
    function FetchZ: I1005Addr; inline;
    procedure StoreAR1(i: Integer; val: Byte); inline;
    procedure StoreAR1Bcd(val: TBcd);
    procedure StoreAR2(i: Integer; val: Byte); inline;
    procedure StoreAR2Bcd(val: TBcd);
    procedure StoreBcd(addr: I1005Addr; len: Integer; val: TBcd);
    procedure StoreByte(addr: I1005Addr; val: Byte); overload;
    procedure StoreByte(bank, row, col, val: Byte); overload;
    procedure StoreCommX(i: Integer; val: Byte); inline;
    procedure StoreFedSysX(addr: I1005Addr); inline;
    procedure StoreICC(addr: I1005Addr); inline;
    procedure StoreIndicator(val: Byte); inline;
    procedure StoreIR(val: Byte; idx: Integer); inline;
    procedure StoreOpcode(val: Byte); inline;
    procedure StoreOperand(val: T1005FedSysOperand); inline;
    procedure StorePAK(addr: I1005Addr); inline;
    procedure StoreZ(addr: I1005Addr); inline;
    property SystemType: T1005SystemType read FSystemType;
  end;

implementation

uses Math, EmulatorTypes;

{ T1005Memory }

procedure T1005Memory.Clear;
var
    i, j, k: Integer;
begin
    for i := 1 to 4 do
        for j := 1 to 32 do
            for k := 1 to 32 do
                FMemory[i, j, k] := X3_SPACE;
end;

constructor T1005Memory.Create(st: T1005SystemType);
begin
    inherited Create;
    FSystemType := st;
    Clear;
end;

function T1005Memory.FetchAR1(i: Integer): Byte;
begin
    Result := FMemory[2, 32, i];
end;

function T1005Memory.FetchAR1Bcd(len: Integer): TBcd;
var
    i, j: Integer;
    b: Byte;
begin
    b := 0;
    Result.Clear;
    Result.Precision := Min(len, Length(Result.Fraction) * 2);
    len := Min(Result.Precision, 10);
    j := 10 - len + 1;
    for i := 0 to len - 1 do
    begin
        b := FetchAr1(j);
        if (b = X3_SPACE) then
            b := X3_0;
        Result.Nibble[i] := (b and $0f) - X3_0;
        Inc(j);
    end;
    if ((b and $20) <> 0) then
        Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
end;

function T1005Memory.FetchAR2(i: Integer): Byte;
begin
    Result := FMemory[2, 32, i + 10];
end;

function T1005Memory.FetchAR2Bcd(len: Integer): TBcd;
var
    i, j: Integer;
    b: Byte;
begin
    b := 0;
    Result.Clear;
    Result.Precision := Min(len, Length(Result.Fraction) * 2);
    len := Min(Result.Precision, 21);
    j := 21 - len + 1;
    for i := 0 to len - 1 do
    begin
        b := FetchAr2(j);
        if (b = X3_SPACE) then
            b := X3_0;
        Result.Nibble[i] := (b and $0f) - X3_0;
        Inc(j);
    end;
    if ((b and $20) <> 0) then
        Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
end;

function T1005Memory.FetchBcd(addr: I1005Addr; len: Integer): TBcd;
var
    i: Integer;
    b: Byte;
    src: I1005Addr;
begin
    b := 0;
    src := addr.Clone;
    Result.Clear;
    Result.Precision := Min(len, Length(Result.Fraction) * 2);
    src.Increment(len - Result.Precision);
    len := Result.Precision;
    for i := 0 to len - 1 do
    begin
        b := FetchByte(src);
        if (b = X3_SPACE) then
            b := X3_0;
        Result.Nibble[i] := (b and $0f) - X3_0;
        src.Increment;
    end;
    if ((b and $20) <> 0) then
        Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
end;

function T1005Memory.FetchByte(addr: I1005Addr): Byte;
var
    bank, row, col: Byte;
begin
    addr.Decode(bank, row, col);
    Result := FMemory[bank, row, col];
end;

function T1005Memory.FetchByte(bank, row, col: Byte): Byte;
var
    addr: I1005Addr;
begin
    // Test given bank, row and col for validity
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(bank, row, col);
    Result := FMemory[bank, row, col];
end;

function T1005Memory.FetchCommOpcode: Byte;
begin
    Result := FMemory[1, 32, 1];
end;

function T1005Memory.FetchCommOperands: T1005CommOperands;
begin
    Result.Init;
    Result.Flda.SetAddr(FMemory[1, 32, 2], FMemory[1, 32, 3]);
    Result.Fldb.SetAddr(FMemory[1, 32, 4], FMemory[1, 32, 5]);
    Result.Fldc.SetAddr(FMemory[1, 32, 6], FMemory[1, 32, 7]);
end;

function T1005Memory.FetchCommX(i: Integer): Byte;
begin
    Result := FMemory[2, 32, i];
end;

function T1005Memory.FetchICC: I1005Addr;
begin
    Result := T1005CommAddr.Create;
    Result.SetAddr(FMemory[1, 32, 8], FMemory[1, 32, 9]);
end;

function T1005Memory.FetchIndicator: Byte;
begin
    Result := FMemory[1, 32, 19]
end;

function T1005Memory.FetchFedSysOpcode: Byte;
begin
    Result := FMemory[1, 32, 11];
end;

function T1005Memory.FetchFedSysOperand: T1005FedSysOperand;
begin
    Result.Init;
    Result.SetAddr(FMemory[1, 32, 12],
                   FMemory[1, 32, 13],
                   FMemory[1, 32, 14],
                   FMemory[1, 32, 15]);
end;

function T1005Memory.FedFedSysOperandRaw: T1005FedSysOperand;
begin
    Result.Init;
    Result.M.SetAddr(FMemory[1, 32, 12], FMemory[1, 32, 13]);
    Result.L.SetAddr(FMemory[1, 32, 14], FMemory[1, 32, 15]);
end;

function T1005Memory.FetchPAK: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.SetAddr(FMemory[1, 32, 17], FMemory[1, 32, 18]);
end;

function T1005Memory.FetchFedSysX: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.SetAddr(FMemory[1, 32, 28], FMemory[1, 32, 29]);
end;

function T1005Memory.FetchZ: I1005Addr;
begin
    Result := T1005FedSysAddr.Create;
    Result.SetAddr(FMemory[1, 32, 1], FMemory[1, 32, 2]);
end;

procedure T1005Memory.StoreAR1(i: Integer; val: Byte);
begin
    FMemory[2, 32, i] := val;
end;

procedure T1005Memory.StoreAR1Bcd(val: TBcd);
var
    i, j: Integer;
    b: Byte;
    lsb: Boolean;
begin
    i := BcdPrecision(val) - 1;
    j := 10;
    lsb := True;
    while ((i >= 0) and (j >= 1)) do
    begin
        b := val.Nibble[i] + X3_0;
        if (lsb and ((val.SignSpecialPlaces and $80) <> 0)) then
            b := b or $20;
        StoreAR1(j, b);
        Dec(i);
        Dec(j);
        lsb := False;
    end;
    while (j >= 1) do
    begin
        StoreAR1(j, X3_0);
        Dec(j);
    end;
    // Set zero & negative bits in MSB
    b := FetchAR1(1);
    if ((val.SignSpecialPlaces and $80) <> 0) then
        b := b or $20;
    if (val = 0) then
        b := b or $10;
    StoreAR1(1, b);
end;

procedure T1005Memory.StoreAR2(i: Integer; val: Byte);
begin
    FMemory[2, 32, i + 10] := val;
end;

procedure T1005Memory.StoreAR2Bcd(val: TBcd);
var
    i, j: Integer;
    b: Byte;
    lsb: Boolean;
begin
    i := BcdPrecision(val) - 1;
    j := 21;
    lsb := True;
    while ((i >= 0) and (j >= 1)) do
    begin
        b := val.Nibble[i] + X3_0;
        if (lsb and ((val.SignSpecialPlaces and $80) <> 0)) then
            b := b or $20;
        StoreAR2(j, b);
        Dec(i);
        Dec(j);
        lsb := False;
    end;
    while (j >= 1) do
    begin
        StoreAR2(j, X3_0);
        Dec(j);
    end;
    // Set zero & negative bits in MSB
    b := FetchAR2(1);
    if ((val.SignSpecialPlaces and $80) <> 0) then
        b := b or $20;
    if (val = 0) then
        b := b or $10;
    StoreAR2(1, b);
end;

procedure T1005Memory.StoreBcd(addr: I1005Addr; len: Integer; val: TBcd);
var
    i, count: Integer;
    b: Byte;
    lsb: Boolean;
    msb: I1005Addr;
begin
    msb := addr.Clone;
    i := BcdPrecision(val) - 1;
    // Increment to LSB
    addr.Increment(len - 1);
    lsb := True;
    while ((i >= 0) and (len > 0)) do
    begin
        b := val.Nibble[i] + X3_0;
        if (lsb and ((val.SignSpecialPlaces and $80) <> 0)) then
            b := b or $20;
        StoreByte(addr, b);
        addr.Decrement;
        Dec(i);
        Dec(len);
        lsb := False;
    end;
    while (len > 0) do
    begin
        StoreByte(addr, X3_0);
        addr.Decrement;
        Dec(len);
    end;
    // Set zero & negative bits in MSB
    b := FetchByte(msb);
    if ((val.SignSpecialPlaces and $80) <> 0) then
        b := b or $20;
    if (val = 0) then
        b := b or $10;
    StoreByte(msb, b);
end;

procedure T1005Memory.StoreByte(bank, row, col, val: Byte);
var
    addr: I1005Addr;
begin
    // Test bank, row and col for validity
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(bank, row, col);
    FMemory[bank, row, col] := val;
end;

procedure T1005Memory.StoreCommX(i: Integer; val: Byte);
begin
    FMemory[2, 32, i] := val;
end;

procedure T1005Memory.StoreByte(addr: I1005Addr; val: Byte);
var
    bank, row, col: Byte;
begin
    addr.Decode(bank, row, col);
    FMemory[bank, row, col] := val;
end;

procedure T1005Memory.StoreICC(addr: I1005Addr);
begin
    FMemory[1, 32, 8] := addr.Row;
    FMemory[1, 32, 9] := addr.Col;
end;

procedure T1005Memory.StoreIndicator(val: Byte);
begin
    FMemory[1, 32, 19] := val;
end;

procedure T1005Memory.StoreIR(val: Byte; idx: Integer);
begin
    FMemory[1, 32, idx] := val;
end;

procedure T1005Memory.StoreOpcode(val: Byte);
begin
    FMemory[1, 32, 11] := val;
end;

procedure T1005Memory.StoreOperand(val: T1005FedSysOperand);
begin
    FMemory[1, 32, 12] := val.M.Row;
    FMemory[1, 32, 13] := val.M.Col;
    FMemory[1, 32, 14] := val.L.Row;
    FMemory[1, 32, 15] := val.L.Col;
end;

procedure T1005Memory.StorePAK(addr: I1005Addr);
begin
    FMemory[1, 32, 17] := addr.Row;
    FMemory[1, 32, 18] := addr.Col;
end;

procedure T1005Memory.StoreFedSysX(addr: I1005Addr);
begin
    FMemory[1, 32, 28] := addr.Row;
    FMemory[1, 32, 29] := addr.Col;
end;

procedure T1005Memory.StoreZ(addr: I1005Addr);
begin
    FMemory[1, 32, 1] := addr.Row;
    FMemory[1, 32, 2] := addr.Col;
end;

end.
