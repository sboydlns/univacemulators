unit U494Memory;

interface

uses SysUtils, FmtBcd, Bcd;

const
    MemSize    = (128 * 1024) - 1;
    // Interrupt vector addresses
    IIllegalInstruction = 0;
    IProgramProtection = 1;
    IPowerLoss = 2;
    IParityBank0 = 3;
    IParityBank1 = 4;
    IBcrParity = 5;
    IIOParity = 6;
    IExecutiveReturn = 7;
    IFFUnderflow = 8;
    IFFOverflow = 9;
    ISync0 = 10;
    ISync1 = 11;
    IRTClock = 12;
    IDayClock = 13;
    IEsiExternal = 16;
    IEsiInput = 17;
    IEsiOutput = 18;
    IParityBank2 = 19;
    IIsiExternal = 20;
    IIsiInput = 21;
    IIsiOutput = 22;
    IParityBank3 = 23;
    ITestAndSet = 24;
    // Buffer Control Register addresses
    BcrOut0 = 32;
    BcrOut1 = 33;
    BcrOut2 = 34;
    BcrOut3 = 35;
    BcrOut4 = 36;
    BcrOut5 = 37;
    BcrOut6 = 38;
    BcrOut7 = 39;
    BcrOut8 = 40;
    BcrOut9 = 41;
    BcrOut10 = 42;
    BcrOut11 = 43;
    BcrOut12 = 44;
    BcrOut13 = 45;
    BcrOut14 = 46;
    BcrOut15 = 47;
    BcrOut16 = 48;
    BcrOut17 = 49;
    BcrOut18 = 50;
    BcrOut19 = 51;
    BcrOut20 = 52;
    BcrOut21 = 53;
    BcrOut22 = 54;
    BcrOut23 = 55;
    BcrIn0 = 64;
    BcrIn1 = 65;
    BcrIn2 = 66;
    BcrIn3 = 67;
    BcrIn4 = 68;
    BcrIn5 = 69;
    BcrIn6 = 70;
    BcrIn7 = 71;
    BcrIn8 = 72;
    BcrIn9 = 73;
    BcrIn10 = 74;
    BcrIn11 = 75;
    BcrIn12 = 76;
    BcrIn13 = 77;
    BcrIn14 = 78;
    BcrIn15 = 79;
    BcrIn16 = 80;
    BcrIn17 = 81;
    BcrIn18 = 82;
    BcrIn19 = 83;
    BcrIn20 = 84;
    BcrIn21 = 85;
    BcrIn22 = 86;
    BcrIn23 = 87;
    // Misc low memory locations
    DayClock = 14;
    RTClock = 15;
    RTClock1230 = 112;
    // Bit masks
    BITS15 = $7FFF;
    BITS17 = $1FFFF;
    BITS30 = $3fffffff;
    BIT14 = $4000;                                      // Sign bit for 15-bit half word
    BIT29 = $20000000;                                  // Sign bit for 30-bit word

type
  T494Address = packed record                           // Used to hold 17-bit addresses
  private
    FValue: UInt32;
    function GetValue15: UInt32;
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
  public
    class operator Add(a: T494Address; b: Integer): T494Address;
    class operator Add(a, b: T494Address): T494Address;
    class operator Equal(a, b: T494Address): Boolean;
    class operator GreaterThan(a: T494Address; b: Integer): Boolean;
    class operator GreaterThanOrEqual(a: T494Address; b: Integer): Boolean;
    class operator LessThan(a: T494Address; b: Integer): Boolean;
    class operator Implicit(a: Integer): T494Address;
    class operator NotEqual(a: T494Address; b: Integer): Boolean;
    class operator NotEqual(a, b: T494Address): Boolean;
    class operator Subtract(a: T494Address; b: Integer): T494Address;
    property Value: UInt32 read GetValue write SetValue;
    property Value15: UInt32 read GetValue15;
  end;

  T494HalfWord = packed record
  // Simulate a Univac 494 half word. A 15-bit ones complement value.
  // The value is stored in the lower 15 bits with sign extension
  // to 16 bits.
  private
    FValue: UInt16;
    procedure ExtendSign;
    function GetValue: UInt16;
    procedure SetValue(const Value: UInt16);
  public
    class operator Add(a, b: T494HalfWord): T494HalfWord;
    class operator Explicit(a: T494HalfWord): Integer;
    class operator Implicit(a: Integer): T494HalfWord;
    class operator Implicit(a: T494HalfWord): Integer;
    class operator Implicit(a: T494Address): T494HalfWord;
    class operator LessThan(a: T494HalfWord; b: Integer): Boolean;
    class operator LogicalNot(a: T494HalfWord): T494HalfWord;
    class operator Subtract(a, b: T494HalfWord): T494HalfWord;
    function IsNegative: Boolean;
    property Value: UInt16 read GetValue write SetValue;
  end;

  T494Word = packed record
  // Simulate a Univac 494 word. A 30-bit ones complement value.
  // The value is stored in the lower 30 bits.
  private
    FValue: UInt32;
    procedure ExtendSign;
    function GetH1: T494HalfWord;
    procedure SetH1(const Value: T494HalfWord);
    function GetH2: T494HalfWord;
    procedure SetH2(const Value: T494HalfWord);
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
  public
    class operator Add(a, b: T494Word): T494Word;
    class operator Equal(a: T494Word; b: Integer): Boolean;
    class operator Explicit(a: T494Word): Integer;
    class operator Explicit(a: T494Word): Int64;
    class operator GreaterThan(a: T494Word; b: Integer): Boolean;
    class operator GreaterThanOrEqual(a: T494Word; b: Integer): Boolean;
    class operator Implicit(a: Integer): T494Word;
    class operator Implicit(a: T494Word): Integer;
    class operator Implicit(a: T494Word): Int64;
    class operator LessThan(a: T494Word; b: Integer): Boolean;
    class operator LessThanOrEqual(a: T494Word; b: Integer): Boolean;
    class operator LogicalNot(a: T494Word): T494Word;
    class operator NotEqual(a: T494Word; b: Integer): Boolean;
    class operator Subtract(a, b: T494Word): T494Word;
    function IsNegative: Boolean;
    property H1: T494HalfWord read GetH1 write SetH1;
    property H2: T494HalfWord read GetH2 write SetH2;
    property Value: UInt32 read GetValue write SetValue;
  end;
  P494Word = ^T494Word;

  T494DWord = packed record
  // Simulate a Univac 494 double word. A 60-bit ones complement value;
  // The value is stored in the lower 60 bits.
  private
    FValue: UInt64;
    procedure ExtendSign;
    function GetValue: UInt64;
    procedure SetValue(const Value: UInt64);
  public
    class operator Add(a, b: T494DWord): T494DWord;
    class operator Implicit(a: Int64): T494DWord;
    class operator Implicit(a: T494DWord): Int64;
    class operator LogicalNot(a: T494DWord): T494DWord;
    class operator Subtract(a, b: T494DWord): T494DWord;
    function IsNegative: Boolean;
    property Value: UInt64 read GetValue write SetValue;
  end;

  T494Ifr = packed record                               // Internal function register
  private
    FValue: UInt32;
    FLockedOut: Boolean;
    FDelayCount: Integer;
    function Getf1: T494Address;
    procedure Setf1(const Value: T494Address);
    function GetValue: UInt32;
    function Getf9: Byte;
    procedure Setf9(const Value: Byte);
    function Getf2: Byte;
    procedure Setf2(const Value: Byte);
    procedure SetValue(const Value: UInt32);
    function Getf4: Byte;
    function Getf5: Byte;
    procedure Setf4(const Value: Byte);
    procedure Setf5(const Value: Byte);
  public
    class operator Implicit(a: Integer): T494Ifr;
    procedure DecDelay;
    function f3: Byte;
    function f6: Byte;
    function f7: Byte;
    function f8: Byte;
    procedure SetDelay(count: Integer);
    property f1: T494Address read Getf1 write Setf1;
    property f2: Byte read Getf2 write Setf2;
    property f4: Byte read Getf4 write Setf4;
    property f5: Byte read Getf5 write Setf5;
    property f9: Byte read Getf9 write Setf9;
    property LockedOut: Boolean read FLockedOut write FLockedOut;
    property Value: UInt32 read GetValue write SetValue;
  end;

  T494Rir = packed record                               // Relative index register
  private
    FValue: UInt32;
    FLockedOut: Boolean;
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
  public
    class operator Implicit(a: Integer): T494Rir;
    property LockedOut: Boolean read FLockedOut write FLockedOut;
    property ActualValue: UInt32 read FValue;
    property Value: UInt32 read GetValue write SetValue;
  end;

  T494Plr = packed record                               // Program lock-in register
  private
    FValue: UInt32;
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
  public
    class operator Implicit(a: Integer): T494Plr;
    function LL: T494Address;
    function UL: T494Address;
    property Value: UInt32 read GetValue write SetValue;
  end;

  T494Bcr = packed record                               // Buffer control register
  private
    FValue: UInt32;
    function GetValue: UInt32;
    procedure SetValue(const Value: UInt32);
    function GetAddress: UInt32;
    function GetCount: UInt32;
    procedure SetAddress(const Value: UInt32);
    procedure SetCount(const Value: UInt32);
  public
    property Address: UInt32 read GetAddress write SetAddress;
    property Count: UInt32 read GetCount write SetCount;
    property Value: UInt32 read GetValue write SetValue;
  end;

  T494Inst = packed record                              // Instruction register
  private
    FValue: UInt32;
    Fybar: T494Address;
  public
    class operator Implicit(a: Integer): T494Inst;
    function b: Byte;
    function f: Byte;
    function g: Byte;
    function j: Byte;
    function jhat: Byte;
    function k: Byte;
    function khat: Byte;
    function y: T494Address;
    property Value: UInt32 read FValue;
    property ybar: T494Address read Fybar write Fybar;
  end;

  T494Csr = packed record
  private
    FValue: Byte;
    procedure SetValue(const Value: Byte);
  public
    property Value: Byte read FValue write SetValue;
  end;

  T494Iasr = packed record
  private
    FValue: Byte;
    procedure SetValue(const Value: Byte);
  public
    property Value: Byte read FValue write SetValue;
  end;

  T494Memory = class
  // Simulate Univac 494 memory of 128K, 30-bit words.
  // Also provides a storage area for the various CPU registers.
  private
    FCore: array [0..MemSize] of T494Word;
    function NativeToBcd(value: UInt64): TBcd;
  public
    // registers
    IFR: T494Ifr;                                       // Internal function register
    PLR: T494Plr;                                       // Program lock-in register
    RIR: T494Rir;                                       // Relative index register
    P: T494Address;                                     // P register
    CSR: T494Csr;                                       // Channel select register
    IASR: T494Iasr;                                     // Interrupt address storage register
    Inst: T494Inst;                                     // Instruction register
    Operand: T494Address;                               // Instruction operand
    IoStatus: T494Word;                                 // Most recent I/O status
    X: T494Word;
    Y: T494Word;
    A: T494Word;
    Q: T494Word;
    K: T494Word;
    B: array [0..1, 1..7] of T494Address;               // B (index) registers
                                                        // 0 = exec 1 = user
    function Fetch(addr: Integer; nolimit: Boolean = False): T494Word;
    function FetchAQ: UInt64;
    function FetchBcd(addr: Integer; nolimit: Boolean = False): TBcd;
    function FetchBcdAQ: TBcd;
    function FetchBcr(addr: Integer; nolimit: Boolean = False): T494Bcr;
    function FetchDWord(addr: Integer; nolimit: Boolean = False): T494DWord;
    procedure Store(addr: Integer; value: T494Word; nolimit: Boolean = False);
    procedure StoreAQ(value: UInt64);
    procedure StoreBcdAQ(value: TBcd);
    procedure StoreBcr(addr: Integer; value: T494Bcr; nolimit: Boolean = False);
    procedure StoreDWord(addr: Integer; value: T494DWord; nolimit: Boolean = False);
  end;

implementation

uses U494Util;

{ T494Word }

class operator T494Word.Add(a, b: T494Word): T494Word;
begin
    Result := Integer(a) + Integer(b);
end;

class operator T494Word.Explicit(a: T494Word): Integer;
begin
    a.ExtendSign;
    Result := Integer(a.FValue);
    if (a.IsNegative) then
        Inc(Result);
end;

class operator T494Word.Equal(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) = b;
end;

class operator T494Word.Explicit(a: T494Word): Int64;
begin
    a.ExtendSign;
    Result := Int64(Integer(a.FValue));
    if (a.IsNegative) then
        Inc(Result);
end;

procedure T494Word.ExtendSign;
begin
    if (IsNegative) then
        FValue := FValue or $c0000000
    else
        FValue := FValue and (not $c0000000);
end;

function T494Word.GetH1: T494HalfWord;
// Get upper 15 bits of 30-bit value
begin
    Result.FValue := (FValue and $3fff8000) shr 15;
end;

function T494Word.GetH2: T494HalfWord;
// Get lower 15 bits of 30-bit value
begin
    Result.FValue := (FValue and BITS15);
end;

function T494Word.GetValue: UInt32;
begin
    Result := FValue and BITS30;
end;

class operator T494Word.GreaterThan(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) > b;
end;

class operator T494Word.GreaterThanOrEqual(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) >= b;
end;

class operator T494Word.Implicit(a: Integer): T494Word;
begin
    Result.FValue := a;
    if (Result.IsNegative) then
        Result.FValue := Result.FValue - 1;
end;

class operator T494Word.Implicit(a: T494Word): Integer;
begin
    a.ExtendSign;
    Result := Integer(a.FValue);
    if (a.IsNegative) then
        Inc(Result);
end;

class operator T494Word.Implicit(a: T494Word): Int64;
begin
    a.ExtendSign;
    Result := Int64(Integer(a.FValue));
    if (a.IsNegative) then
        Inc(Result);
end;

function T494Word.IsNegative: Boolean;
begin
    Result := (FValue and BIT29) <> 0;
end;

class operator T494Word.LessThan(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) < b;
end;

class operator T494Word.LessThanOrEqual(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) <= b;
end;

class operator T494Word.LogicalNot(a: T494Word): T494Word;
begin
    Result.FValue := a.FValue xor BITS30;
end;

class operator T494Word.NotEqual(a: T494Word; b: Integer): Boolean;
begin
    Result := Integer(a) <> b;
end;

procedure T494Word.SetH1(const Value: T494HalfWord);
// Set upper 15 bits of 30-bit value
begin
    FValue := (Value.FValue shl 15) or (FValue and BITS15);
    ExtendSign;
end;

procedure T494Word.SetH2(const Value: T494HalfWord);
// Set lower 15 bits of 30-bit value
begin
    FValue := (FValue and (not BITS15)) or (Value.FValue and BITS15);
end;

procedure T494Word.SetValue(const Value: UInt32);
begin
    FValue := value and BITS30;
end;

class operator T494Word.Subtract(a, b: T494Word): T494Word;
begin
    Result := Integer(a) - Integer(b);
end;

{ T494HalfWord }

class operator T494HalfWord.Add(a, b: T494HalfWord): T494HalfWord;
begin
    Result := Integer(a) + Integer(b);
end;

class operator T494HalfWord.Explicit(a: T494HalfWord): Integer;
begin
    a.ExtendSign;
    Result := Integer(a.FValue);
    if (a.IsNegative) then
        Inc(Result);
end;

procedure T494HalfWord.ExtendSign;
begin
    if (IsNegative) then
        FValue := FValue or $8000
    else
        FValue := FValue and (not $8000);
end;

function T494HalfWord.GetValue: UInt16;
begin
    Result := FValue and BITS15;
end;

class operator T494HalfWord.Implicit(a: T494HalfWord): Integer;
begin
    a.ExtendSign;
    Result := Integer(a.FValue);
    if (a.IsNegative) then
        Inc(Result);
end;

class operator T494HalfWord.Implicit(a: Integer): T494HalfWord;
begin
    Result.FValue := a;
    if (Result.IsNegative) then
        Result.FValue := Result.FValue - 1;
end;

class operator T494HalfWord.Implicit(a: T494Address): T494HalfWord;
begin
    Result.FValue := a.Value15;
end;

function T494HalfWord.IsNegative: Boolean;
begin
    Result := (FValue and BIT14) <> 0;
end;

class operator T494HalfWord.LessThan(a: T494HalfWord; b: Integer): Boolean;
begin
    Result := Integer(a) < b;
end;

class operator T494HalfWord.LogicalNot(a: T494HalfWord): T494HalfWord;
begin
    Result.FValue := a.FValue xor BITS15;
end;

procedure T494HalfWord.SetValue(const Value: UInt16);
begin
    FValue := value and BITS15;
end;

class operator T494HalfWord.Subtract(a, b: T494HalfWord): T494HalfWord;
begin
    Result := Integer(a) - Integer(b);
end;

{ T494Inst }

function T494Inst.b: Byte;
begin
    Result := (FValue and $38000) shr 15;
end;

function T494Inst.f: Byte;
begin
    Result := (FValue and $3f000000) shr 24;
end;

function T494Inst.g: Byte;
begin
    Result := (FValue and $fc0000) shr 18;
end;

class operator T494Inst.Implicit(a: Integer): T494Inst;
begin
    Result.FValue := a;
end;

function T494Inst.j: Byte;
begin
    Result := (FValue and $e00000) shr 21;
end;

function T494Inst.jhat: Byte;
begin
    Result := (FValue and $f00000) shr 20;

end;

function T494Inst.k: Byte;
begin
    Result := (FValue and $1c0000) shr 18;
end;

function T494Inst.khat: Byte;
begin
    Result := (FValue and $c0000) shr 18;
end;

function T494Inst.y: T494Address;
begin
    Result.FValue := FValue and BITS15;
end;

{ T494Address }

class operator T494Address.Add(a: T494Address; b: Integer): T494Address;
// Unsigned 17-bit addition
begin
    Result.FValue := (Integer(a.FValue) + b) and BITS17;
end;

class operator T494Address.Add(a, b: T494Address): T494Address;
begin
    Result.FValue := (a.FValue + b.FValue) and BITS17;
end;

class operator T494Address.Equal(a, b: T494Address): Boolean;
begin
    Result := Integer(a) = Integer(b);
end;

function T494Address.GetValue: UInt32;
begin
    Result := FValue and BITS17;
end;

function T494Address.GetValue15: UInt32;
begin
    Result := FValue and BITS15;
end;

class operator T494Address.GreaterThan(a: T494Address; b: Integer): Boolean;
begin
    Result := Integer(a.FValue) > b;
end;

class operator T494Address.GreaterThanOrEqual(a: T494Address; b: Integer): Boolean;
begin
    Result := Integer(a.FValue) >= b;
end;

class operator T494Address.Implicit(a: Integer): T494Address;
begin
    Result.FValue := a and BITS17;
end;

class operator T494Address.LessThan(a: T494Address; b: Integer): Boolean;
begin
    Result := Integer(a.FValue) < b;
end;

class operator T494Address.NotEqual(a, b: T494Address): Boolean;
begin
    Result := a.FValue <> b.FValue;
end;

class operator T494Address.NotEqual(a: T494Address; b: Integer): Boolean;
begin
    Result := Integer(a.FValue) <> b;
end;

procedure T494Address.SetValue(const Value: UInt32);
begin
    FValue := Value and BITS17;
end;

class operator T494Address.Subtract(a: T494Address; b: Integer): T494Address;
begin
    Result.FValue := (Integer(a.FValue) - b) and BITS17;
end;

{ T494Ifr }

procedure T494Ifr.DecDelay;
begin
    if (FDelayCount > 0) then
        Dec(FDelayCount);
end;

function T494Ifr.f3: Byte;
// Protection mode. no gaurd = 00, guard w/ read & write = 01,
// write only = 10, guard w/ write = 11.
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 0
    else
        Result := (FValue and $600000) shr 21;
end;

function T494Ifr.f6: Byte;
// Executive B registers = 0, user B registers = 1
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 0
    else
        Result := (FValue and $2000000) shr 25;
end;

function T494Ifr.f7: Byte;
// All B registers 15-bit = 0, B1-3 15-bit B4-7 17-bit = 1
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 1
    else
        Result := (FValue and $4000000) shr 26;
end;

function T494Ifr.f8: Byte;
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 0
    else
        Result := (FValue and $8000000) shr 27;
end;

function T494Ifr.Getf1: T494Address;
begin
    Result.FValue := FValue and BITS17;
end;

function T494Ifr.Getf2: Byte;
// j designator of repeat instruction
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 0
    else
        Result := (FValue and $1c0000) shr 18;
end;

function T494Ifr.Getf4: Byte;
begin
    Result := (FValue shr 23) and $1;
end;

function T494Ifr.Getf5: Byte;
begin
    Result := (FValue shr 24) and $1;
end;

function T494Ifr.Getf9: Byte;
begin
    if (FLockedOut or (FDelayCount > 0)) then
        Result := 0
    else
        Result := (FValue and BIT29) shr 29;
end;

function T494Ifr.GetValue: UInt32;
begin
    Result := FValue and BITS30;
end;

class operator T494Ifr.Implicit(a: Integer): T494Ifr;
begin
    Result.FValue := a;
end;

procedure T494Ifr.SetDelay(count: Integer);
begin
    FDelayCount := count + 1;
end;

procedure T494Ifr.Setf1(const Value: T494Address);
begin
    if ((not FLockedOut) and (FDelayCount <= 0)) then
        FValue := (FValue and (not BITS17)) or Value.FValue;
end;

procedure T494Ifr.Setf2(const Value: Byte);
begin
    if ((not FLockedOut) and (FDelayCount <= 0)) then
        FValue := (FValue and (not $1c0000)) or ((value and $7) shl 18);
end;

procedure T494Ifr.Setf4(const Value: Byte);
begin
    FValue := (FValue and (not ($1 shl 23))) or ((Value and $1) shl 23);
end;

procedure T494Ifr.Setf5(const Value: Byte);
begin
    FValue := (FValue and (not ($1 shl 24))) or ((Value and $1) shl 24);
end;

procedure T494Ifr.Setf9(const Value: Byte);
begin
    if ((not FLockedOut) and (FDelayCount <= 0)) then
        FValue := (FValue and (not BIT29)) or ((value and $1) shl 29);
end;

procedure T494Ifr.SetValue(const Value: UInt32);
begin
    FValue := Value;
end;

{ T494Plr }

function T494Plr.GetValue: UInt32;
begin
    Result := FValue and BITS30;
end;

class operator T494Plr.Implicit(a: Integer): T494Plr;
begin
    Result.FValue := a;
end;

function T494Plr.LL: T494Address;
begin
    Result := (FValue and $7ff) shl 6;
end;

procedure T494Plr.SetValue(const Value: UInt32);
begin
    FValue := Value;
end;

function T494Plr.UL: T494Address;
begin
    Result := ((FValue and $3ffffff) shr 9) or $3f;
end;

{ T494Memory }

function T494Memory.Fetch(addr: Integer; nolimit: Boolean): T494Word;
begin
    if (not nolimit) then
    begin
        if (IFR.f3 = 01) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    Result := FCore[addr];
end;

function T494Memory.FetchAQ: UInt64;
begin
    Result := (UInt64(A.Value) shl 30) or Q.Value;
end;

function T494Memory.FetchBcd(addr: Integer; nolimit: Boolean): TBcd;
var
    m: UInt64;
begin
    m := (UInt64(Fetch(addr, nolimit).Value) shl 30) or
         Fetch(addr + 1, nolimit).Value;
    Result := NativeToBcd(m);
end;

function T494Memory.FetchBcdAQ: TBcd;
var
    m: UInt64;
begin
    m := (UInt64(A.Value) shl 30) or Q.Value;
    Result := NativeToBcd(m);
end;

function T494Memory.FetchBcr(addr: Integer; nolimit: Boolean): T494Bcr;
begin
    if (not nolimit) then
    begin
        if (IFR.f3 = 01) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    Result.Value := FCore[addr].Value;
end;

function T494Memory.FetchDWord(addr: Integer; nolimit: Boolean): T494DWord;
begin
    if (not nolimit) then
    begin
        if (IFR.f3 = 01) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    Result.Value := Int64(FCore[addr].Value) shl 30 or FCore[addr + 1].Value;
end;

function T494Memory.NativeToBcd(value: UInt64): TBcd;
var
    n: Byte;
    i: Integer;
begin
    Result.Clear;
    Result.Precision := 10;
    for i := 0 to 9 do
    begin
        n := value and $3f;
        value := value shr 6;
        // Negative?
        if ((i = 0) and ((n and $10) = 0)) then
            Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
        Result.Nibble[9 - i] := n and $f;
    end;
end;

procedure T494Memory.Store(addr: Integer; value: T494Word; nolimit: Boolean);
begin
    if (not nolimit) then
    begin
        if (IFR.f3 <> 0) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    FCore[addr].Value := value.Value;
end;

procedure T494Memory.StoreAQ(value: UInt64);
begin
    A.Value := (value shr 30) and BITS30;
    Q.Value := value and BITS30;
end;

procedure T494Memory.StoreBcdAQ(value: TBcd);
var
    i, count: Integer;
    aq, mask: UInt64;
    b, n: Byte;
begin
    aq := (UInt64(A.Value) shl 30) or Q.Value;
    mask := $3f;
    count := 0;
    i := BcdPrecision(value) - 1;
    while ((i >= 0) and (count < 10)) do
    begin
        n := value.Nibble[i];
        b := (aq and mask) shr (6 * count);
        b := (b and (not $f)) or (n and $f);
        if (i = (BcdPrecision(value) - 1)) then
        begin
            if (value < 0) then
                b := b and (not $10)
            else
                b := b or $10;
        end;
        aq := (aq and (not mask)) or (UInt64(b) shl (6 * count));
        Dec(i);
        Inc(count);
        mask := mask shl 6;
    end;
    while (count < 10) do
    begin
        aq := (aq and (not mask)) or (UInt64($30) shl (6 * count));
        Inc(count);
        mask := mask shl 6;
    end;
    A.Value := (aq shr 30) and BITS30;
    Q.Value := aq and BITS30;
end;

procedure T494Memory.StoreBcr(addr: Integer; value: T494Bcr; nolimit: Boolean);
begin
    if (not nolimit) then
    begin
        if (IFR.f3 <> 0) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    FCore[addr].Value := value.Value;
end;

procedure T494Memory.StoreDWord(addr: Integer; value: T494DWord; nolimit: Boolean);
begin
    if (not nolimit) then
    begin
        if (IFR.f3 <> 0) then
            if ((PLR.LL > addr) or (PLR.UL < addr)) then
                raise EProgramProtection.CreateFmt('Address (%d) out of range', [addr]);
    end;
    FCore[addr].Value := value.Value shr 30;
    FCore[addr + 1].Value := value.Value;
end;

{ T494Bcr }

function T494Bcr.GetAddress: UInt32;
begin
    Result := FValue and BITS17;
end;

function T494Bcr.GetCount: UInt32;
begin
    Result := (FValue and $3ffc0000) shr 18;
end;

function T494Bcr.GetValue: UInt32;
begin
    Result := FValue and BITS30;
end;

procedure T494Bcr.SetAddress(const Value: UInt32);
begin
    FValue := (FValue and (not BITS17)) or (Value and BITS17);
end;

procedure T494Bcr.SetCount(const Value: UInt32);
begin
    FValue := (FValue and (not $3ffc0000)) or ((Value and $fff) shl 18);
end;

procedure T494Bcr.SetValue(const Value: UInt32);
begin
    FValue := Value and BITS30;
end;

{ T494Rir }

function T494Rir.GetValue: UInt32;
begin
    if (FLockedOut) then
        Result := 0
    else
        Result := FValue and $1ffc0;
end;

class operator T494Rir.Implicit(a: Integer): T494Rir;
begin
    Result.FValue := a and $1ffc0;
end;

procedure T494Rir.SetValue(const Value: UInt32);
begin
    FValue := Value and $1ffc0;
end;

{ T494Csr }

procedure T494Csr.SetValue(const Value: Byte);
begin
    FValue := Value and $1f;
end;

{ T494DWord }

class operator T494DWord.Add(a, b: T494DWord): T494DWord;
begin
    Result := Int64(a) + Int64(b);
end;

procedure T494DWord.ExtendSign;
begin
    if (IsNegative) then
        FValue := FValue or $f000000000000000
    else
        FValue := FValue and (not $f000000000000000);
end;

function T494DWord.GetValue: UInt64;
begin
    Result := FValue and $fffffffffffffff;
end;

class operator T494DWord.Implicit(a: Int64): T494DWord;
begin
    Result.FValue := a;
    if (Result.IsNegative) then
        Result.FValue := Result.FValue - 1;
end;

class operator T494DWord.Implicit(a: T494DWord): Int64;
begin
    a.ExtendSign;
    Result := Int64(a.FValue);
    if (a.IsNegative) then
        Inc(Result);
end;

function T494DWord.IsNegative: Boolean;
begin
    Result := (FValue and $800000000000000) <> 0;
end;

class operator T494DWord.LogicalNot(a: T494DWord): T494DWord;
begin
    Result.FValue := a.FValue xor $fffffffffffffff;
end;

procedure T494DWord.SetValue(const Value: UInt64);
begin
    FValue := Value and $fffffffffffffff;
end;

class operator T494DWord.Subtract(a, b: T494DWord): T494DWord;
begin
    Result := Int64(a) - Int64(b);
end;

{ T494Iasr }

procedure T494Iasr.SetValue(const Value: Byte);
begin
    FValue := Value and $1f;
end;

end.
