unit ObjFile;

interface

uses SysUtils, System.Classes;

const
  BYTE_SIGN = $80;

  RS_ENTRY_POINTS = 1;
  RS_EXTERNALS_POINTER = 2;
  RS_TRANSFER = 3;
  RS_OBJECT_CODE_SIZE = 4;
  RS_OBJECT_CODE = 5;
  RS_EXTERNALS = 6;

  RT_NONE = 0;
  RT_WORD = 1;
  RT_H1 = 2;
  RT_H2 = 3;
  RT_H1H2 = 4;

type
  TRelocatableType = ( rtNone = 0, rtWord, rtH1, rtH2, rtH1H2 );

  TObjFileStream = class(TFileStream)
  protected
    FLocationCounter: UInt32;
    FTransferAddrEmitted: Boolean;
    procedure CheckTransferAddrEmitted;
    procedure PadTo(addr: UInt32); virtual;
  public
    procedure EmitDoubleWord(addr: UInt32; word: UInt64); virtual; abstract;
    procedure EmitEntryPoint(ID: AnsiString; value: UInt32); virtual; abstract;
    procedure EmitExternalID(ID: AnsiString); virtual; abstract;
    procedure EmitExternalReference(addr: UInt32); virtual; abstract;
    procedure EmitMultiWord(addr: UInt32; words: array of UInt32); virtual; abstract;
    procedure EmitObjEnd(addr: UInt32); virtual; abstract;
    procedure EmitSingleWord(addr: UInt32; rel: TRelocatableType; word: UInt32); virtual; abstract;
    procedure EmitTransferAddr(addr: UInt32; objSize: UInt32); virtual;
    function FetchWord(var addr: UInt32; var rel: TRelocatableType; var word: UInt32): Boolean; virtual; abstract;
  end;

  TMemImageStream = class(TObjFileStream)
  public
    procedure EmitDoubleWord(addr: UInt32; dword: UInt64); override;
    procedure EmitEntryPoint(ID: AnsiString; value: UInt32); override;
    procedure EmitExternalID(ID: AnsiString); override;
    procedure EmitExternalReference(addr: UInt32); override;
    procedure EmitMultiWord(addr: UInt32; words: array of UInt32); override;
    procedure EmitObjEnd(addr: UInt32); override;
    procedure EmitSingleWord(addr: UInt32; rel: TRelocatableType; word: UInt32); override;
    procedure EmitTransferAddr(addr: UInt32; objSize: UInt32); override;
    function FetchWord(var addr: UInt32; var rel: TRelocatableType; var word: UInt32): Boolean; override;
  end;
  // 1230 absolute paper tape image
  TAbsoluteStream = class(TMemImageStream)
  protected
    FUpperCheck: UInt32;
    FLowerCheck: UInt32;
  public
    destructor Destroy; override;
    procedure EmitTransferAddr(addr: UInt32; objSize: UInt32); override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function WriteNoCheck(const Buffer; Count: Longint): Longint;
  end;

  // Defintion of an entry point record. The last one in the list will
  // have an ID of all spaces.
  TEntryPoint = packed record
    ID: array [1..10] of AnsiChar;
    Value: UInt32;
    function IDToString: AnsiString;
  end;

  // Definition of an external reference record. The last one in the list will
  // have an ID of all spaces.
  TExternalRef = packed record
    ID: array [1..10] of AnsiChar;
    function IDToString: AnsiString;
  end;
  // Each of the above will have a collection of these telling the linker
  // where to find locations in the object code where the external reference
  // is referenced. The last on in the list will be -0 (all ones).
  TErReference = packed record
    RefAddress: UInt32;
  end;
  // Format for relocatable object files
  //
  // A relocatable file consists of several sections (in the following order):
  //    Transfer address
  //    Object code size
  //    External reference table pointer
  //    Entry point table
  //    Object code
  //    External reference table
  //
  // Each section is preceded by a flag byte which indicates the content of the
  // part which follows. Each flag byte will have its sign bit set with the
  // content in the LSB.
  //    Entry point table = 1
  //    External reference table = 2
  //    Transfer address = 3
  //    Object code size = 4
  //    Object code = 5
  //    External reference table = 6
  //
  // Each word of object code is preceded by a flag byte which indicates
  // the relocatability of the word. The flag byte will have its sign bit
  // set. The last entry of the object code table will have a flag byte of
  // -0 (all ones).
  //    Not relocatable = 0
  //    Entire word = 1
  //    Upper half word = 2
  //    Lower half word = 3
  //    Both half words = 4
  TRelocatableStream = class(TObjFileStream)
  protected
    FObjFlagWritten: Boolean;
    FEpFlagWritten: Boolean;
    FErFlagWritten: Boolean;
    procedure CheckEpFlag;
    procedure CheckErFlag;
    procedure CheckObjFlag;
    procedure PadTo(addr: UInt32); override;
  public
    procedure EmitDoubleWord(addr: UInt32; dword: UInt64); override;
    procedure EmitEntryPoint(ID: AnsiString; value: UInt32); override;
    procedure EmitExternalID(ID: AnsiString); override;
    procedure EmitExternalReference(addr: UInt32); override;
    procedure EmitMultiWord(addr: UInt32; words: array of UInt32); override;
    procedure EmitObjEnd(addr: UInt32); override;
    procedure EmitSingleWord(addr: UInt32; rel: TRelocatableType; word: UInt32); override;
    procedure EmitTransferAddr(addr: UInt32; objSize: UInt32); override;
    function FetchWord(var addr: UInt32; var rel: TRelocatableType; var word: UInt32): Boolean; override;
    procedure FetchExternalsPtr(var ptr: UInt32);
    procedure FetchTransferAddr(var addr: UInt32; var objSize: UInt32);
    function FirstEntryPoint(var ep: TEntryPoint): Boolean;
    function FirstExternal(var er: TExternalRef): Boolean;
    function FirstObjectWord(var rel: TRelocatableType; var word: UInt32): Boolean;
    function NextEntryPoint(var ep: TEntryPoint): Boolean;
    function NextExternal(var er: TExternalRef): Boolean;
    function NextObjectWord(var rel: TRelocatableType; var word: UInt32): Boolean;
    function NextReference(var ref: TErReference): Boolean;
  end;

implementation

uses U494Memory;

{ TMemImageStream }

procedure TMemImageStream.EmitDoubleWord(addr: UInt32; dword: UInt64);
var
    word: Uint32;
begin
    CheckTransferAddrEmitted;
    PadTo(addr);
    word := dword shr 30;
    Write(word, SizeOf(word));
    word := dword and BITS30;
    Write(word, SizeOf(word));
    Inc(FLocationCounter, 2);
end;

procedure TMemImageStream.EmitEntryPoint(ID: AnsiString; value: UInt32);
begin
end;

procedure TMemImageStream.EmitExternalID(ID: AnsiString);
begin
end;

procedure TMemImageStream.EmitExternalReference(addr: UInt32);
begin
end;

procedure TMemImageStream.EmitMultiWord(addr: UInt32; words: array of UInt32);
var
    word: UInt32;
begin
    CheckTransferAddrEmitted;
    PadTo(addr);
    for word in words do
    begin
        Write(word, SizeOf(word));
        Inc(FLocationCounter);
    end;
end;

procedure TMemImageStream.EmitObjEnd(addr: UInt32);
begin
    PadTo(addr - 1);
end;

procedure TMemImageStream.EmitSingleWord(addr: UInt32; rel: TRelocatableType; word: UInt32);
begin
    CheckTransferAddrEmitted;
    PadTo(addr);
    Write(word, SizeOf(word));
    Inc(FLocationCounter);
end;

procedure TMemImageStream.EmitTransferAddr(addr: UInt32; objSize: UInt32);
begin
    inherited;
    Write(addr, SizeOf(addr));
end;

function TMemImageStream.FetchWord(var addr: UInt32; var rel: TRelocatableType; var word: UInt32): Boolean;
var
    temp: UInt32;
begin
    Result := True;
    if (Position = 0) then
        FLocationCounter := UInt32(-1);
    if (Read(temp, SizeOf(temp)) <> SizeOf(temp)) then
    begin
        Result := False;
        Exit;
    end;
    addr := FLocationCounter;
    rel := rtNone;
    word := temp;
    Inc(FLocationCounter);
end;

{ TListFileStream }

procedure TObjFileStream.CheckTransferAddrEmitted;
begin
    if (not FTransferAddrEmitted) then
        raise Exception.Create('EmitTransferAddr must be first method called');
end;

procedure TObjFileStream.EmitTransferAddr(addr: UInt32; objSize: UInt32);
begin
    if (FLocationCounter <> 0) then
        raise Exception.Create('EmitTransferAddr must be first method called');
    FTransferAddrEmitted := True;
end;

procedure TObjFileStream.PadTo(addr: UInt32);
var
    zero: UInt32;
begin
    zero := 0;
    while (FLocationCounter < addr) do
    begin
        Write(zero, SizeOf(zero));
        Inc(FLocationCounter);
    end;
end;

{ TRelocatableStream }

procedure TRelocatableStream.CheckEpFlag;
var
    flags: Byte;
    zero: UInt32;
begin
    if (not FEpFlagWritten) then
    begin
        // Write dummy externals table pointer first
        flags := BYTE_SIGN or RS_EXTERNALS_POINTER;
        Write(flags, SizeOf(flags));
        zero := 0;
        Write(zero, SizeOf(zero));
        //
        flags := BYTE_SIGN or RS_ENTRY_POINTS;
        Write(flags, SizeOf(flags));
        FEpFlagWritten := True;
    end;
end;

procedure TRelocatableStream.CheckErFlag;
var
    flags: Byte;
    curpos: UInt32;
begin
    if (not FErFlagWritten) then
    begin
        // Update the externals table pointer with current file position
        curpos := Position;
        Position := (SizeOf(UInt32) * 2) + 2;
        flags := BYTE_SIGN or RS_EXTERNALS_POINTER;
        Write(flags, SizeOf(flags));
        Write(curpos, SizeOf(curpos));
        //
        Position := curpos;
        flags := BYTE_SIGN or RS_EXTERNALS;
        Write(flags, SizeOf(flags));
        FErFlagWritten := True;
    end;
end;

procedure TRelocatableStream.CheckObjFlag;
var
    flags: Byte;
begin
    if (not FObjFlagWritten) then
    begin
        flags := BYTE_SIGN or RS_OBJECT_CODE;
        Write(flags, SizeOf(flags));
        FObjFlagWritten := True;
    end;
end;

procedure TRelocatableStream.EmitDoubleWord(addr: UInt32; dword: UInt64);
var
    flags: Byte;
    word: Uint32;
begin
    CheckTransferAddrEmitted;
    CheckObjFlag;
    PadTo(addr);
    flags := BYTE_SIGN or RT_NONE;
    word := dword shr 30;
    Write(flags, SizeOf(flags));
    Write(word, SizeOf(word));
    word := dword and BITS30;
    Write(flags, SizeOf(flags));
    Write(word, SizeOf(word));
    Inc(FLocationCounter, 2);
end;

procedure TRelocatableStream.EmitEntryPoint(ID: AnsiString; value: UInt32);
var
    ep: TEntryPoint;
    i: Integer;
begin
    CheckEpFlag;
    i := 1;
    while ((i <= Length(ID)) and (i <= 10)) do
    begin
        ep.ID[i] := ID[i];
        Inc(i);
    end;
    while (i <= 10) do
    begin
        ep.ID[i] := ' ';
        Inc(i);
    end;
    ep.Value := value;
    Write(ep, SizeOf(ep));
end;

procedure TRelocatableStream.EmitExternalID(ID: AnsiString);
var
    er: TExternalRef;
    i: Integer;
begin
    CheckErFlag;
    i := 1;
    while ((i <= Length(ID)) and (i <= 10)) do
    begin
        er.ID[i] := ID[i];
        Inc(i);
    end;
    while (i <= 10) do
    begin
        er.ID[i] := ' ';
        Inc(i);
    end;
    Write(er, SizeOf(er));
end;

procedure TRelocatableStream.EmitExternalReference(addr: UInt32);
var
    er: TErReference;
begin
    er.RefAddress := addr;
    Write(er, SizeOf(er));
end;

procedure TRelocatableStream.EmitMultiWord(addr: UInt32; words: array of UInt32);
var
    flags: Byte;
    word: UInt32;
begin
    CheckTransferAddrEmitted;
    CheckObjFlag;
    PadTo(addr);
    flags := BYTE_SIGN or RT_NONE;
    for word in words do
    begin
        Write(flags, SizeOf(flags));
        Write(word, SizeOf(word));
        Inc(FLocationCounter);
    end;
end;

procedure TRelocatableStream.EmitObjEnd(addr: UInt32);
var
    flags: Byte;
begin
    PadTo(addr - 1);
    flags := $ff;
    Write(flags, SizeOf(flags));
end;

procedure TRelocatableStream.EmitSingleWord(addr: UInt32; rel: TRelocatableType; word: UInt32);
var
    flags: Byte;
begin
    CheckTransferAddrEmitted;
    CheckObjFlag;
    PadTo(addr);
    flags := BYTE_SIGN or Byte(rel);
    Write(flags, SizeOf(flags));
    Write(word, SizeOf(word));
    Inc(FLocationCounter);
end;

procedure TRelocatableStream.EmitTransferAddr(addr, objSize: UInt32);
var
    flags: Byte;
begin
    inherited;
    flags := BYTE_SIGN or RS_TRANSFER;
    Write(flags, SizeOf(flags));
    Write(addr, SizeOf(addr));
    flags := BYTE_SIGN or RS_OBJECT_CODE_SIZE;
    Write(flags, SizeOf(flags));
    Write(objSize, SizeOf(objSize));
end;

procedure TRelocatableStream.FetchExternalsPtr(var ptr: UInt32);
var
    addr, objSize: UInt32;
    flags: Byte;
begin
    Position := 0;
    FetchTransferAddr(addr, objSize);
    Read(flags, SizeOf(flags));
    if (flags <> (BYTE_SIGN or RS_EXTERNALS_POINTER)) then
        raise Exception.Create('Invalid relocatable file. Externals table pointer not found.');
    Read(ptr, SizeOf(ptr));
end;

procedure TRelocatableStream.FetchTransferAddr(var addr, objSize: UInt32);
var
    flags: Byte;
begin
    Position := 0;
    Read(flags, SizeOf(flags));
    if (flags <> (BYTE_SIGN or RS_TRANSFER)) then
        raise Exception.Create('Invalid relocatable file. Transfer address not found.');
    Read(addr, SizeOf(addr));
    Read(flags, SizeOf(flags));
    if (flags <> (BYTE_SIGN or RS_OBJECT_CODE_SIZE)) then
        raise Exception.Create('Invalid relocatable file. Object code size not found.');
    Read(objSize, SizeOf(objSize));
end;

function TRelocatableStream.FetchWord(var addr: UInt32; var rel: TRelocatableType; var word: UInt32): Boolean;
begin
    Result := False;
end;

function TRelocatableStream.FirstEntryPoint(var ep: TEntryPoint): Boolean;
var
    ptr: UInt32;
    flags: Byte;
begin
    FetchExternalsPtr(ptr);                 // position to start of entry pt table
    Read(flags, SizeOf(flags));
    if (flags <> (BYTE_SIGN or RS_ENTRY_POINTS)) then
        raise Exception.Create('Invalid relocatable file. Entry point table not found.');
    Read(ep, SizeOf(ep));
    Result := ep.IDToString <> '';
end;

function TRelocatableStream.FirstExternal(var er: TExternalRef): Boolean;
var
    ptr: UInt32;
    flags: Byte;
begin
    FetchExternalsPtr(ptr);
    Position := ptr;
    Read(flags, SizeOf(flags));
    if (flags <> (BYTE_SIGN or RS_EXTERNALS)) then
        raise Exception.Create('Invalid relocatable file. Externals table not found.');
    Read(er, SizeOf(er));
    Result := er.IDToString <> '';
end;

function TRelocatableStream.FirstObjectWord(var rel: TRelocatableType; var word: UInt32): Boolean;
var
    ep: TEntryPoint;
    stat: Boolean;
    flags: Byte;
begin
    // Skip past entry point table
    stat := FirstEntryPoint(ep);
    while (stat) do
        stat := NextEntryPoint(ep);
    Read(flags, Sizeof(flags));
    if (flags <> (BYTE_SIGN or RS_OBJECT_CODE)) then
        raise Exception.Create('Invalid relocatable file. Object code table not found.');
    Result := NextObjectWord(rel, word);
end;

function TRelocatableStream.NextEntryPoint(var ep: TEntryPoint): Boolean;
begin
    Read(ep, SizeOf(ep));
    Result := ep.IDToString <> '';
end;

function TRelocatableStream.NextExternal(var er: TExternalRef): Boolean;
begin
    Read(er, SizeOf(er));
    Result := er.IDToString <> '';
end;

function TRelocatableStream.NextObjectWord(var rel: TRelocatableType; var word: UInt32): Boolean;
var
    flags: Byte;
begin
    Read(flags, SizeOf(flags));
    rel := TRelocatableType(flags and (not BYTE_SIGN));
    Read(word, SizeOf(word));
    Result := flags <> $ff;
end;

function TRelocatableStream.NextReference(var ref: TErReference): Boolean;
begin
    Read(ref, SizeOf(ref));
    Result := (ref.RefAddress and BITS30) <> BITS30;
end;

procedure TRelocatableStream.PadTo(addr: UInt32);
var
    flags: Byte;
    wzero: UInt32;
begin
    flags := BYTE_SIGN or RT_NONE;
    wzero := 0;
    while (FLocationCounter < addr) do
    begin
        Write(flags, SizeOf(flags));
        Write(wzero, SizeOf(wzero));
        Inc(FLocationCounter);
    end;
end;

{ TEntryPoint }

function TEntryPoint.IDToString: AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in ID do
    begin
        if (c <> ' ') then
            Result := Result + c;
    end;
end;

{ TExternalRef }

function TExternalRef.IDToString: AnsiString;
var
    c: AnsiChar;
begin
    Result := '';
    for c in ID do
    begin
        if (c <> ' ') then
            Result := Result + c;
    end;
end;

{ TAbsoluteStream }

destructor TAbsoluteStream.Destroy;
var
    hdr: UInt32;
begin
    // Write the check sums
    WriteNoCheck(FLowerCheck, SizeOf(FLowerCheck));
    WriteNoCheck(FUpperCheck, SizeOf(FUpperCheck));
    // Write end of file header
    hdr := 0;
    WriteNoCheck(hdr, SizeOf(hdr));
    inherited;
end;

procedure TAbsoluteStream.EmitTransferAddr(addr, objSize: UInt32);
var
    word: UInt32;
begin
    if (FLocationCounter <> 0) then
        raise Exception.Create('EmitTransferAddr must be first method called');
    FTransferAddrEmitted := True;
    word := 62;                         // absolute image flag
    Write(word, SizeOf(word));
    word := ((addr and BITS15) shl 15) or ((addr + objSize - 1) and BITS15);
    Write(word, SizeOf(word));
    FLocationCounter := addr;
    FUpperCheck := 0;
    FLowerCheck := 0;
end;

function TAbsoluteStream.Write(const Buffer; Count: Integer): Longint;
// write a word a 5 six bits bytes
var
    word: UInt32;
    i: Integer;
    bfr: array [1..5] of Byte;
begin
    if (Count <> 4) then
        raise Exception.Create('Write buffer <> UInt32');
    word := UInt32(Buffer);
    for i := 5 downto 1 do
    begin
        bfr[i] := word and $3f;
        word := word shr 6;
    end;
    Result := inherited Write(bfr[1], 5);
    // Compute check sums
    word := UInt32(Buffer);
    if ((FUpperCheck and $20000000) <> 0) then
        FUpperCheck := FUpperCheck + 1 + ((word shr 15) and $7fff)
    else
        FUpperCheck := FUpperCheck + ((word shr 15) and $7fff);
    if ((FLowerCheck and $20000000) <> 0) then
        FLowerCheck := FLowerCheck + 1 + (word and $7fff)
    else
        FLowerCheck := FLowerCheck + (word and $7fff);
end;

function TAbsoluteStream.WriteNoCheck(const Buffer; Count: Integer): Longint;
var
    word: UInt32;
    i: Integer;
    bfr: array [1..5] of Byte;
begin
    if (Count <> 4) then
        raise Exception.Create('Write buffer <> UInt32');
    word := UInt32(Buffer);
    for i := 5 downto 1 do
    begin
        bfr[i] := word and $3f;
        word := word shr 6;
    end;
    Result := inherited Write(bfr[1], 5);
end;

end.
