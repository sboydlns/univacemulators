unit Memory;

interface

uses SysUtils, Data.FmtBcd,
     U9030Types;

const
    // Fixed memory addresses
    IOSTCW0 = $10;
    IOSTCW1 = IOSTCW0 + 4;
    IOSTCW2 = IOSTCW0 + 8;
    //
    BCSW0 = $E0;
    BCSW1 = BCSW0 + 4;
    BCSW2 = BCSW0 + 8;
    BCSW3 = BCSW0 + 12;
    //
    REL_REG0 = $B0;
    REL_REG1 = REL_REG0 + 4;
    REL_REG2 = REL_REG0 + 8;
    REL_REG3 = REL_REG0 + 12;
    REL_REG4 = REL_REG0 + 16;
    REL_REG5 = REL_REG0 + 20;
    REL_REG6 = REL_REG0 + 24;
    REL_REG7 = REL_REG0 + 28;
    // PSW addresses
    IOST_OLD = $20;
    IOST_NEW = $28;
    MACH_CHECK_OLD = $30;
    MACH_CHECK_NEW = $38;
    PGM_EXCP_OLD = $40;
    PGM_EXCP_NEW = $48;
    SVC_OLD = $50;
    SVC_NEW = $58;
    TIMER_OLD = $60;
    TIMER_NEW = $68;
    MONITOR_OLD = $80;
    MONITOR_NEW = $88;
    // IOSTSW flags
    IOSTSW_IWE = $10;
    IOSTSW_IDE = $08;
    IOSTSW_CTO = $04;
    IOSTSW_CBW = $02;
    IOSTSW_STF = $01;
    // Misc
    MACHINE_ID = $A4;
    LOAD_ID = $A5;
    REVISION_LEVEL = $A6;
    // BCWs
    CAW = $A0;
    CONS_BCW0 = $100;
    READER_BCW0 = $110;
    PRINTER_BCW0 = $120;
    PUNCH_BCW0 = $130;
    //
    MUX_BCW0 = $200;
    //
    IDA_BCW0 = $f0;
    IDA_BCW1 = IDA_BCW0 + 4;
    IDA_BCW2 = IDA_BCW0 + 8;
    IDA_BCW3 = IDA_BCW0 + 12;
    //



type
  // Main memory. Configured for 512K
  TMemory = class(TObject)
  private
    FMemory: array [0..(512*1024)-1] of Byte;           // 512 K of memory
    FStorageKeys: array [0..255] of Byte;               // Storage protection keys
    procedure CheckAddressRead(key: Byte; var addr: TMemoryAddress); inline;
    procedure CheckAddressWrite(key: Byte; var addr: TMemoryAddress); inline;
    procedure CheckAlignment(addr: TMemoryAddress; align: TAlignment); inline;
  public
    constructor Create;
    procedure Copy(src: PByte; dest: TMemoryAddress; len: Integer);
    function FetchByte(key: Byte; addr: TMemoryAddress): Byte;
    function FetchDblWord(key: Byte; addr: TMemoryAddress): TDblWord;
    function FetchHalfWord(key: Byte; addr: TMemoryAddress): THalfWord;
    function FetchPacked(key: Byte; addr: TMemoryAddress; len: Integer): TBcd;
    function FetchRelReg(key: Byte): TMemoryAddress;
    function FetchStorageKey(addr: TMemoryAddress): Byte;
    function FetchWord(key: Byte; addr: TMemoryAddress): TWord;
    procedure StoreByte(key: Byte; addr: TMemoryAddress; val: Byte);
    procedure StoreDblWord(key: Byte; addr: TMemoryAddress; val: TDblWord);
    procedure StoreHalfWord(key: Byte; addr: TMemoryAddress; val: THalfWord);
    procedure StorePacked(key: Byte; addr: TMemoryAddress; len: Integer; val: TBcd);
    procedure StoreStorageKey(addr: TMemoryAddress; value: Byte);
    procedure StoreWord(key: Byte; addr: TMemoryAddress; val: TWord);
  end;

  // I/O Status Table Control Word wrapper
  TIOSTCW = class
  private
    class function GetAddress: TMemoryAddress; static;
    class function GetChannel: Byte; static;
    class function GetFlags: Byte; static;
    class function GetKey: Byte; static;
    class procedure SetAddress(val: TMemoryAddress); static;
    class procedure SetChannel(val: Byte); static;
    class procedure SetFlags(val: Byte); static;
    class function GetActiveCount: THalfWord; static;
    class procedure SetActiveCount(const Value: THalfWord); static;
    class function GetReplCount: THalfWord; static;
    class procedure SetReplCount(const Value: THalfWord); static;
    class function GetActiveAddr: THalfWord; static;
    class procedure SetActiveAddr(const Value: THalfWord); static;
    class function GetReplAddr: THalfWord; static;
    class procedure SetReplAddr(const Value: THalfWord); static;
    class procedure SetKey(const Value: Byte); static;
  public
    class procedure NextWord;
    class property ActiveAddr: THalfWord read GetActiveAddr write SetActiveAddr;
    class property ActiveCount: THalfWord read GetActiveCount write SetActiveCount;
    class property Address: TMemoryAddress read GetAddress write SetAddress;
    class property Channel: Byte read GetChannel write SetChannel;
    class property Flags: Byte read GetFlags write SetFlags;
    class property Key: Byte read GetKey write SetKey;
    class property ReplAddr: THalfWord read GetReplAddr write SetReplAddr;
    class property ReplCount: THalfWord read GetReplCount write SetReplCount;
  end;

implementation

uses Globals, Math,
     Bcd;

{ TMemory }

procedure TMemory.CheckAddressRead(key: Byte;var addr: TMemoryAddress);
// Check address for valid range and read protection
var
    skey: Byte;
    readProtect: Boolean;
begin
    addr := addr and $ffffff;
    if (addr > High(FMemory)) then
        raise EAddressException.Create('Memory address exceeds installed memory');
    if (key <> 0) then
    begin
        skey := FetchStorageKey(addr);
        readProtect := (skey and $10) <> 0;
        skey := skey shr 5;
        if ((key <> skey) or readProtect) then
            raise EProtectionException.Create('Read protection exception');
    end;
end;

procedure TMemory.CheckAddressWrite(key: Byte; var addr: TMemoryAddress);
// Check address for valid range
var
    skey: Byte;
begin
    addr := addr and $ffffff;
    if (addr > High(FMemory)) then
        raise EAddressException.Create('Memory address exceeds installed memory');
    if (key <> 0) then
    begin
        skey := FetchStorageKey(addr) shr 5;
        if (key <> skey) then
            raise EProtectionException.Create('Write protection exception');
    end;
end;

procedure TMemory.CheckAlignment(addr: TMemoryAddress; align: TAlignment);
// Check address for 2, 4 or 8 byte alignment
var
    ok: Boolean;
begin
    ok := False;
    case align of
      aHalfWord:    ok := (addr and $01) = 0;
      aWord:        ok := (addr and $03) = 0;
      aDblWord:     ok := (addr and $07) = 0;
    end;
    if (not ok) then
        raise ESpecificationException.Create('Memory alignment error');
end;

procedure TMemory.Copy(src: PByte; dest: TMemoryAddress; len: Integer);
// Copy an array of bytes to emulator memory
begin
    while (len > 0) do
    begin
        FMemory[dest] := src^;
        Inc(src);
        Inc(dest);
        Dec(len);
    end;
end;

constructor TMemory.Create;
begin
end;

function TMemory.FetchByte(key: Byte; addr: TMemoryAddress): Byte;
begin
    CheckAddressRead(key, addr);
    Result := FMemory[addr];
end;

function TMemory.FetchDblWord(key: Byte; addr: TMemoryAddress): TDblWord;
var
    b: PByte;
begin
    CheckAddressRead(key, addr);
    CheckAlignment(addr, aDblWord);
    b := PByte(@Result);
    b^ := FMemory[addr + 7];
    (b + 1)^ := FMemory[addr + 6];
    (b + 2)^ := FMemory[addr + 5];
    (b + 3)^ := FMemory[addr + 4];
    (b + 4)^ := FMemory[addr + 3];
    (b + 5)^ := FMemory[addr + 2];
    (b + 6)^ := FMemory[addr + 1];
    (b + 7)^ := FMemory[addr];
end;

function TMemory.FetchHalfWord(key: Byte; addr: TMemoryAddress): THalfWord;
var
    b: PByte;
begin
    CheckAddressRead(key, addr);
    CheckAlignment(addr, aHalfWord);
    b := PByte(@Result);
    b^ := FMemory[addr + 1];
    (b + 1)^ := FMemory[addr];
end;

function TMemory.FetchPacked(key: Byte; addr: TMemoryAddress; len: Integer): TBcd;
var
    i: UInt32;
    b: Byte;
begin
    Result.Clear;
    len := Min(len, Length(Result.Fraction));
    Result.Precision := ((len + 1) * 2) - 1;
    for i := 0 to len do
    begin
        b := FetchByte(key, addr + i);
        Result.Fraction[i] := b;
        if (i = len) then
        begin
            Result.Fraction[i] := b and $F0;
            if (PackedSign(b) < 0) then
                Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
            if ((b and $F0) > $90) then
                raise EDataException.Create('Invalid decimal digit');
        end else
        begin
            if (((b and $F0) > $90) or ((b and $0F) > $09)) then
                raise EDataException.Create('Invalid decimal digit');
        end;
    end;
end;

function TMemory.FetchRelReg(key: Byte): TMemoryAddress;
begin
    Result := TMemoryAddress(FetchWord(0, REL_REG0 + ((key and $7) * 4)));
end;

function TMemory.FetchStorageKey(addr: TMemoryAddress): Byte;
var
    i: UInt32;
begin
    i := addr shr 11;
    if (i > 255) then
        raise EAddressException.Create('Illegal address for storage key access');
    Result := FStorageKeys[i];
end;

function TMemory.FetchWord(key: Byte; addr: TMemoryAddress): TWord;
var
    b: PByte;
begin
    CheckAddressRead(key, addr);
    CheckAlignment(addr, aWord);
    b := PByte(@Result);
    b^ := FMemory[addr + 3];
    (b + 1)^ := FMemory[addr + 2];
    (b + 2)^ := FMemory[addr + 1];
    (b + 3)^ := FMemory[addr];
end;

procedure TMemory.StoreByte(key: Byte; addr: TMemoryAddress; val: Byte);
begin
    CheckAddressWrite(key, addr);
    FMemory[addr] := val;
end;

procedure TMemory.StoreDblWord(key: Byte; addr: TMemoryAddress; val: TDblWord);
var
    b: PByte;
begin
    CheckAddressWrite(key, addr);
    CheckAlignment(addr, aDblWord);
    b := PByte(@val);
    FMemory[addr + 7] := b^;
    FMemory[addr + 6] := (b + 1)^;
    FMemory[addr + 5] := (b + 2)^;
    FMemory[addr + 4] := (b + 3)^;
    FMemory[addr + 3] := (b + 4)^;
    FMemory[addr + 2] := (b + 5)^;
    FMemory[addr + 1] := (b + 6)^;
    FMemory[addr] := (b + 7)^;
end;

procedure TMemory.StoreHalfWord(key: Byte; addr: TMemoryAddress; val: THalfWord);
var
    b: PByte;
begin
    CheckAddressWrite(key, addr);
    CheckAlignment(addr, aHalfWord);
    b := PByte(@val);
    FMemory[addr + 1] := b^;
    FMemory[addr] := (b + 1)^;
end;

procedure TMemory.StorePacked(key: Byte; addr: TMemoryAddress; len: Integer; val: TBcd);
var
    i: Integer;
    p: TMemoryAddress;
    sign: Byte;
    b: Byte;
begin
    // Packed values don't have the concept of scale, so all
    // packed values in memory have zero decimal places. This
    // means that we can ignore any digits past the decimal
    // point.
    i := BcdPrecision(val) - 1;
    if (PSW.Ascii) then
    begin
        // ASCII mode
        if ((val.SignSpecialPlaces and $80) <> 0) then
                sign := $0B
            else
                sign := $0A;
    end else
    begin
        // EBCDIC mode
        if ((val.SignSpecialPlaces and $80) <> 0) then
            sign := $0D
        else
            sign := $0C;
    end;
    // Clear the receiving memory
    p := addr + len;
    StoreByte(key, p, sign);
    Dec(p);
    while (p >= addr) do
    begin
        StoreByte(key, p, 0);
        Dec(p);
    end;
    // Save the BCD value
    p := addr + len;
    StoreByte(key, p, sign or (val.Nibble[i] shl 4));
    Dec(i);
    Dec(p);
    while ((p >= addr) and (i >= 0)) do
    begin
        b := val.Nibble[i];
        Dec(i);
        if (i >= 0) then
            b := b or (val.Nibble[i] shl 4);
        StoreByte(key, p, b);
        Dec(i);
        Dec(p);
    end;
end;

procedure TMemory.StoreStorageKey(addr: TMemoryAddress; value: Byte);
var
    i: UInt32;
begin
    i := addr shr 11;
    if (i > 255) then
        raise EAddressException.Create('Illegal address for storage key access');
    FStorageKeys[i] := value;
end;

procedure TMemory.StoreWord(key: Byte; addr: TMemoryAddress; val: TWord);
var
    b: PByte;
begin
    CheckAddressWrite(key, addr);
    CheckAlignment(addr, aWord);
    b := PByte(@val);
    FMemory[addr + 3] := b^;
    FMemory[addr + 2] := (b + 1)^;
    FMemory[addr + 1] := (b + 2)^;
    FMemory[addr] := (b + 3)^;
end;

{ TIOSTCW }

class function TIOSTCW.GetActiveAddr: THalfWord;
begin
    Result := Core.FetchHalfWord(0, IOSTCW1 + 2);
end;

class function TIOSTCW.GetActiveCount: THalfWord;
begin
    Result := Core.FetchHalfWord(0, IOSTCW0);
end;

class function TIOSTCW.GetAddress: TMemoryAddress;
begin
    Result := Core.FetchWord(0, IOSTCW1) and $ffffff;
end;

class function TIOSTCW.GetChannel: Byte;
begin
    Result := Core.FetchByte(0, IOSTCW2 + 2);
end;

class function TIOSTCW.GetFlags: Byte;
begin
    Result := Core.FetchByte(0, IOSTCW2 + 3);
end;

class function TIOSTCW.GetKey: Byte;
begin
    Result := Core.FetchByte(0, IOSTCW1);
end;

class function TIOSTCW.GetReplAddr: THalfWord;
begin
    Result := Core.FetchHalfWord(0, IOSTCW2);
end;

class function TIOSTCW.GetReplCount: THalfWord;
begin
    Result := Core.FetchHalfWord(0, IOSTCW0 + 2);
end;

class procedure TIOSTCW.NextWord;
// Decrement the IOSTCW active count and increment the active address.
// If the active count reaches zero, set the active count = replacement count
// and the active address = replacement address
var
    actv: THalfWord;
begin
    // increment the address
    actv := Core.FetchHalfWord(0, IOSTCW1 + 2);
    Inc(actv, 4);
    Core.StoreHalfWord(0, IOSTCW1 + 2, actv);
    // decrement the count
    actv := Core.FetchHalfWord(0, IOSTCW0);
    Dec(actv);
    Core.StoreHalfWord(0, IOSTCW0, actv);
    if (actv = 0) then
    begin
        actv := Core.FetchHalfWord(0, IOSTCW0 + 2);
        Core.StoreHalfWord(0, IOSTCW0, actv);
        actv := Core.FetchHalfWord(0, IOSTCW2);
        Core.StoreHalfWord(0, IOSTCW1 + 2, actv);
    end;
end;

class procedure TIOSTCW.SetActiveAddr(const Value: THalfWord);
begin
    Core.StoreHalfWord(0, IOSTCW1 + 2, Value);
end;

class procedure TIOSTCW.SetActiveCount(const Value: THalfWord);
begin
    Core.StoreHalfWord(0, IOSTCW0, Value);
end;

class procedure TIOSTCW.SetAddress(val: TMemoryAddress);
begin
    Core.StoreWord(0, IOSTCW1, (Core.FetchWord(0, IOSTCW1) and $ff000000) or (val and $ffffff));
end;

class procedure TIOSTCW.SetChannel(val: Byte);
begin
    Core.StoreByte(0, IOSTCW2 + 2, val);
end;

class procedure TIOSTCW.SetFlags(val: Byte);
begin
    Core.StoreByte(0, IOSTCW2 + 3, val);
end;

class procedure TIOSTCW.SetKey(const Value: Byte);
begin
    Core.StoreByte(0, IOSTCW1, Value);
end;

class procedure TIOSTCW.SetReplAddr(const Value: THalfWord);
begin
    Core.StoreHalfWord(0, IOSTCW2, Value);
end;

class procedure TIOSTCW.SetReplCount(const Value: THalfWord);
begin
    Core.StoreHalfWord(0, IOSTCW0 + 2, Value);
end;

end.
