unit Memory;

interface

uses SysUtils,
     U9030Types;

type
  TMemory = class(TObject)
  private
    FMemory: array [0..(512*1024)-1] of Byte;           // 512 K of memory
    procedure CheckAddress(addr: TMemoryAddress); inline;
    procedure CheckAlignment(addr: TMemoryAddress; align: TAlignment); inline;
  public
    function FetchByte(addr: TMemoryAddress): Byte;
    function FetchDblWord(addr: TMemoryAddress): TDblWord;
    function FetchHalfWord(addr: TMemoryAddress): THalfWord;
    function FetchWord(addr: TMemoryAddress): TWord;
    procedure StoreByte(addr: TMemoryAddress; val: Byte);
    procedure StoreDblWord(addr: TMemoryAddress; val: TDblWord);
    procedure StoreHalfWord(addr: TMemoryAddress; val: THalfWord);
    procedure StoreWord(addr: TMemoryAddress; val: TWord);
  end;

implementation

{ TMemory }

procedure TMemory.CheckAddress(addr: TMemoryAddress);
// Check address for valid range
begin
    if (addr > High(FMemory)) then
        raise EMemoryError.Create('Memory address exceeds installed memory');
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
        raise EAlignmentError.Create('Memory alignment error');
end;

function TMemory.FetchByte(addr: TMemoryAddress): Byte;
begin
    CheckAddress(addr);
    Result := FMemory[addr];
end;

function TMemory.FetchDblWord(addr: TMemoryAddress): TDblWord;
var
    b: PByte;
begin
    CheckAddress(addr);
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

function TMemory.FetchHalfWord(addr: TMemoryAddress): THalfWord;
var
    b: PByte;
begin
    CheckAddress(addr);
    CheckAlignment(addr, aHalfWord);
    b := PByte(@Result);
    b^ := FMemory[addr + 1];
    (b + 1)^ := FMemory[addr];
end;

function TMemory.FetchWord(addr: TMemoryAddress): TWord;
var
    b: PByte;
begin
    CheckAddress(addr);
    CheckAlignment(addr, aWord);
    b := PByte(@Result);
    b^ := FMemory[addr + 3];
    (b + 1)^ := FMemory[addr + 2];
    (b + 2)^ := FMemory[addr + 1];
    (b + 3)^ := FMemory[addr];
end;

procedure TMemory.StoreByte(addr: TMemoryAddress; val: Byte);
begin
    CheckAddress(addr);
    FMemory[addr] := val;
end;

procedure TMemory.StoreDblWord(addr: TMemoryAddress; val: TDblWord);
var
    b: PByte;
begin
    CheckAddress(addr);
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

procedure TMemory.StoreHalfWord(addr: TMemoryAddress; val: THalfWord);
var
    b: PByte;
begin
    CheckAddress(addr);
    CheckAlignment(addr, aHalfWord);
    b := PByte(@val);
    FMemory[addr + 1] := b^;
    FMemory[addr] := (b + 1)^;
end;

procedure TMemory.StoreWord(addr: TMemoryAddress; val: TWord);
var
    b: PByte;
begin
    CheckAddress(addr);
    CheckAlignment(addr, aWord);
    b := PByte(@val);
    FMemory[addr + 3] := b^;
    FMemory[addr + 2] := (b + 1)^;
    FMemory[addr + 1] := (b + 2)^;
    FMemory[addr] := (b + 3)^;
end;

end.
