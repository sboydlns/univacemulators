unit U9200Memory;

interface

uses SysUtils, U9200Types, FmtBcd, Bcd;

const
  // Processor states
  PS_PROCESSOR = 0;
  PS_IO = 1;
type
  TU92Memory = class(TObject)
  private
    FSize: Integer;
    FMemory: array of Byte;
    function GetSRC: Byte;
    procedure SetSRC(const Value: Byte);
    function GetFAP(state: TU92CPUState): Smallint;
    procedure SetFAP(state: TU92CPUState; const Value: Smallint);
    function GetFAF(state: TU92CPUState): Byte;
    procedure SetFAF(state: TU92CPUState; const Value: Byte);
    function GetFAL(state: TU92CPUState): Byte; {inline;}
    procedure SetFAL(state: TU92CPUState; const Value: Byte); {inline;}
    function GetFAD1(state: TU92CPUState): Smallint; {inline;}
    function GetFAD2(state: TU92CPUState): Smallint; {inline;}
    procedure SetFAD1(state: TU92CPUState; const Value: Smallint); {inline;}
    procedure SetFAD2(state: TU92CPUState; const Value: Smallint); {inline;}
    function GetR(state: TU92CPUState; reg: Integer): Smallint; {inline;}
    procedure SetR(state: TU92CPUState; reg: Integer; const Value: Smallint); {inline;}
    function GetFAS(state: TU92CPUState): Byte;
    procedure SetFAS(state: TU92CPUState; const Value: Byte);
    function GetMemory: Pointer;
    function GetCC(state: TU92CPUState): Byte;
    procedure SetCC(state: TU92CPUState; const Value: Byte);
    function GetMIR1(state: TU92CPUState): Integer;
    function GetASCII(state: TU92CPUState): Boolean;
    procedure SetASCII(state: TU92CPUState; const Value: Boolean);
    function GetDA: Byte;
    function GetDS: Byte;
    procedure SetDA(const Value: Byte);
    procedure SetDS(const Value: Byte);
    function GetBCW(idx: Integer): Cardinal;
    procedure SetBCW(idx: Integer; const Value: Cardinal);
    function GetPrinterImage(idx: Integer): Byte;
    procedure SetPrinterImage(idx: Integer; const Value: Byte);
  public
    constructor Create(memSize: Integer);
    procedure CheckByteAddress(addr: Integer);
    procedure CheckHalfWordAddress(addr: Integer);
    procedure Clear;
    function FetchByte(addr: Integer): Byte; {inline;}
    function FetchHalfWord(addr: Integer): Smallint; {inline;}
    function FetchPacked(ascii: Boolean; addr, len: Integer): TBcd;
    procedure StoreByte(addr: Integer; value: Byte); {inline;}
    procedure StoreHalfWord(addr: Integer; value: Smallint); {inline;}
    procedure StorePacked(ascii: Boolean; addr, len: Integer; value: TBcd);
    property ASCII[state: TU92CPUState]: Boolean read GetASCII write SetASCII;
    property BCW[idx: Integer]: Cardinal read GetBCW write SetBCW;
    property CC[state: TU92CPUState]: Byte read GetCC write SetCC;
    property DA: Byte read GetDA write SetDA;
    property DS: Byte read GetDS write SetDS;
    property FAD1[state: TU92CPUState]: Smallint read GetFAD1 write SetFAD1;
    property FAD2[state: TU92CPUState]: Smallint read GetFAD2 write SetFAD2;
    property FAF[state: TU92CPUState]: Byte read GetFAF write SetFAF;
    property FAL[state: TU92CPUState]: Byte read GetFAL write SetFAL;
    property FAP[state: TU92CPUState]: Smallint read GetFAP write SetFAP;
    property FAS[state: TU92CPUState]: Byte read GetFAS write SetFAS;
    property MIR1[state: TU92CPUState]: Integer read GetMIR1;
    property Memory: Pointer read GetMemory;
    property ORL: Byte index $05 read FetchByte write StoreByte;
    property PrinterImage[idx: Integer]: Byte read GetPrinterImage write SetPrinterImage;
    property R[state: TU92CPUState; reg: Integer]: Smallint read GetR write SetR;
    property RAD: Byte index $04 read FetchByte write StoreByte;
    property Size: Integer read FSize;
    property SRC: Byte read GetSRC write SetSRC;
    property SS0: Byte index $1D read FetchByte write StoreByte;
    property SS1: Byte index $1E read FetchByte write StoreByte;
    property SS2: Byte index $1F read FetchByte write StoreByte;
  end;

implementation

uses Math;

{ TU92Memory }

procedure TU92Memory.CheckByteAddress(addr: Integer);
begin
    if ((addr < 0) or (addr >= FSize)) then
        raise EMemoryError.Create('Invalid memory address');
end;

procedure TU92Memory.CheckHalfWordAddress(addr: Integer);
begin
    if ((addr < 0) or (addr >= (FSize - 1))) then
        raise EMemoryError.Create('Invalid memory address');
    if ((addr and $01) = 1) then
        raise EMemoryError.Create('Not half word aligned');
end;

procedure TU92Memory.Clear;
begin
    FillChar(FMemory[0], FSize, 0);
end;

constructor TU92Memory.Create(memSize: Integer);
begin
    FSize := memSize;
    SetLength(FMemory, FSize);
    Clear;
end;

function TU92Memory.FetchByte(addr: Integer): Byte;
begin
    CheckByteAddress(addr);
    Result := FMemory[addr];
end;

function TU92Memory.FetchHalfWord(addr: Integer): Smallint;
begin
    CheckHalfWordAddress(addr);
    Result := (Fmemory[addr] shl 8) or FMemory[addr + 1];
end;

function TU92Memory.FetchPacked(ascii: Boolean; addr, len: Integer): TBcd;
var
    i: Integer;
    b: Byte;
begin
    Result.Clear;
    len := Min(len, Length(Result.Fraction));
    Result.Precision := ((len + 1) * 2) - 1;
    for i := 0 to len do
    begin
        b := FetchByte(addr + i);
        Result.Fraction[i] := b;
        if (i = len) then
        begin
            Result.Fraction[i] := Result.Fraction[i] and $F0;
            if (PackedSign(b) < 0) then
                Result.SignSpecialPlaces := Result.SignSpecialPlaces or $80;
        end;
    end;
end;

function TU92Memory.GetASCII(state: TU92CPUState): Boolean;
begin
    Result := ((FAS[state] and $20) <> 0);
end;

function TU92Memory.GetBCW(idx: Integer): Cardinal;
var
    offset: Integer;
begin
    offset := idx * 4;
    Result := (FetchHalfWord(offset + $40) shl 16) or FetchHalfword(offset + $42);
end;

function TU92Memory.GetCC(state: TU92CPUState): Byte;
begin
    Result := (FAS[state] and $C0) shr 6;
end;

function TU92Memory.GetDA: Byte;
begin
    Result := FetchByte($43);
end;

function TU92Memory.GetDS: Byte;
begin
    Result := FetchByte($42);
end;

function TU92Memory.GetFAD1(state: TU92CPUState): Smallint;
begin
    if (ucsIO in state) then
        Result := FetchHalfWord($18)
    else
        Result := FetchHalfWord($08);
end;

function TU92Memory.GetFAD2(state: TU92CPUState): Smallint;
begin
    if (ucsIO in state) then
        Result := FetchHalfWord($1A)
    else
        Result := FetchHalfWord($0A);
end;

function TU92Memory.GetFAF(state: TU92CPUState): Byte;
begin
    if (ucsIO in state) then
        Result := FetchByte($16)
    else
        Result := FetchByte($06);
end;

function TU92Memory.GetFAL(state: TU92CPUState): Byte;
begin
    if (ucsIO in state) then
        Result := FetchByte($17)
    else
        Result := FetchByte($07);
end;

function TU92Memory.GetFAP(state: TU92CPUState): Smallint;
begin
    if (ucsIO in state) then
        Result := FetchHalfWord($12)
    else
        Result := FetchHalfWord($02);
end;

function TU92Memory.GetFAS(state: TU92CPUState): Byte;
begin
    if (ucsIO in state) then
        Result := FetchByte($10)
    else
        Result := FetchByte($00);
end;

function TU92Memory.GetMemory: Pointer;
begin
    Result := @FMemory[0];
end;

function TU92Memory.GetMIR1(state: TU92CPUState): Integer;
begin
    if (ucsIO in state) then
        Result := FetchHalfWord($16)
    else
        Result := FetchHalfWord($06);
end;

function TU92Memory.GetPrinterImage(idx: Integer): Byte;
begin
    if ((idx < 0) or (idx > 132)) then
        raise EMemoryError.Create('Illegal printer image offset');
    Result := FetchByte($80 + idx);
end;

function TU92Memory.GetR(state: TU92CPUState; reg: Integer): Smallint;
begin
    if (ucsIO in state) then
        Result := FetchHalfWord($30 + ((reg and $7) shl 1))
    else
        Result := FetchHalfWord($20 + ((reg and $7) shl 1));
end;

function TU92Memory.GetSRC: Byte;
begin
    Result := FetchByte($11);
end;

procedure TU92Memory.SetASCII(state: TU92CPUState; const Value: Boolean);
begin
    if (Value) then
        FAS[state] := FAS[state] or $20
    else
        FAS[state] := FAS[state] and $DF;
end;

procedure TU92Memory.SetBCW(idx: Integer; const Value: Cardinal);
var
    offset: Integer;
begin
    offset := idx * 4;
    StoreHalfWord(offset + $40, Value shr 16);
    StoreHalfword(offset + $42, Value and $FFFF);
end;

procedure TU92Memory.SetCC(state: TU92CPUState; const Value: Byte);
begin
    FAS[state] := (FAS[state] and $3f) or ((Value and $03) shl 6);
end;

procedure TU92Memory.SetDA(const Value: Byte);
begin
    StoreByte($43, Value);
end;

procedure TU92Memory.SetDS(const Value: Byte);
begin
    StoreByte($42, Value);
end;

procedure TU92Memory.SetFAD1(state: TU92CPUState; const Value: Smallint);
begin
    if (ucsIO in state) then
        StoreHalfWord($18, Value)
    else
        StoreHalfWord($08, Value);
end;

procedure TU92Memory.SetFAD2(state: TU92CPUState; const Value: Smallint);
begin
    if (ucsIO in state) then
        StoreHalfWord($1A, Value)
    else
        StoreHalfWord($0A, Value);
end;

procedure TU92Memory.SetFAF(state: TU92CPUState; const Value: Byte);
begin
    if (ucsIO in state) then
        StoreByte($16, Value)
    else
        StoreByte($06, Value);
end;

procedure TU92Memory.SetFAL(state: TU92CPUState; const Value: Byte);
begin
    if (ucsIO in state) then
        StoreByte($17, Value)
    else
        StoreByte($07, Value);
end;

procedure TU92Memory.SetFAP(state: TU92CPUState; const Value: Smallint);
begin
    if (ucsIO in state) then
        StoreHalfWord($12, Value)
    else
        StoreHalfWord($02, Value);
end;

procedure TU92Memory.SetFAS(state: TU92CPUState; const Value: Byte);
begin
    if (ucsIO in state) then
        StoreByte($10, Value)
    else
        StoreByte($00, Value);
end;

procedure TU92Memory.SetPrinterImage(idx: Integer; const Value: Byte);
begin
    if ((idx < 0) or (idx > 132)) then
        raise EMemoryError.Create('Illegal printer image offset');
    StoreByte($80 + idx, Value);
end;

procedure TU92Memory.SetR(state: TU92CPUState; reg: Integer; const Value: Smallint);
begin
    if (ucsIO in state) then
        StoreHalfWord($30 + ((reg and $7) shl 1), Value)
    else
        StoreHalfWord($20 + ((reg and $7) shl 1), Value);
end;

procedure TU92Memory.SetSRC(const Value: Byte);
begin
    StoreByte($11, Value);
end;

procedure TU92Memory.StoreByte(addr: Integer; value: Byte);
begin
    CheckByteAddress(addr);
    FMemory[addr] := value;
end;

procedure TU92Memory.StoreHalfWord(addr: Integer; value: Smallint);
begin
    CheckHalfWordAddress(addr);
    FMemory[addr] := value shr 8;
    FMemory[addr + 1] := value and $ff;
end;

procedure TU92Memory.StorePacked(ascii: Boolean; addr, len: Integer; value: TBcd);
var
    i: Integer;
    p: Smallint;
    sign: Byte;
    b: Byte;
begin
    // Packed values don't have the concept of scale, so all
    // packed values in memory have zero decimal places. This
    // means that we can ignore any digits past the decimal
    // point.
    i := BcdPrecision(value) - 1;
    if (ascii) then
    begin
        // ASCII mode
        if ((value.SignSpecialPlaces and $80) <> 0) then
                sign := $0B
            else
                sign := $05;
    end else
    begin
        // EBCDIC mode
        if ((value.SignSpecialPlaces and $80) <> 0) then
            sign := $0D
        else
            sign := $0C;
    end;
    // Clear the receiving memory
    p := addr + len;
    StoreByte(p, sign);
    Dec(p);
    while (p >= addr) do
    begin
        StoreByte(p, 0);
        Dec(p);
    end;
    // Save the BCD value
    p := addr + len;
    StoreByte(p, sign or (value.Nibble[i] shl 4));
    Dec(i);
    Dec(p);
    while ((p >= addr) and (i >= 0)) do
    begin
        b := value.Nibble[i];
        Dec(i);
        if (i >= 0) then
            b := b or (value.Nibble[i] shl 4);
        StoreByte(p, b);
        Dec(i);
        Dec(p);
    end;
end;

end.
