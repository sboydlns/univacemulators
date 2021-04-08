unit OS3Files;

interface

uses SysUtils, Classes, Generics.Collections, U8418, VTOC;

type
  TPartitionExtent = class(TObject)
  public
    FirstTrack: Integer;
    NumTracks: Integer;
  end;

  TPEList = class(TObjectList<TPartitionExtent>)
  end;

  TPartition = class(TObject)
  private
    FExtentList: TPEList;
    FBlockSize: UInt16;
    FRecordSize: UInt16;
    FLaceFactor: UInt32;
    FRotnAdjust: UInt32;
    function GetExtent(idx: Integer): TPartitionExtent;
    function GetExtentCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddExtent(trk, num: Integer);
    property BlockSize: UInt16 read FBlockSize write FBlockSize;
    property LaceFactor: UInt32 read FLaceFactor write FLaceFactor;
    property RecordSize: UInt16 read FRecordSize write FRecordSize;
    property RotnAdjust: UInt32 read FRotnAdjust write FRotnAdjust;
    property ExtentCount: Integer read GetExtentCount;
    property Extents[idx: Integer]: TPartitionExtent read GetExtent;
  end;

  TPartitionList = class(TObjectList<TPartition>)
  end;

  TLibsHdr = packed record
  private
    FBuffer: array [0..4] of Byte;
    function GetBlockNum: UInt32;
    function GetBlockLength: Byte;
    function GetCheckSum: Byte;
  public
    property BlockNum: UInt32 read GetBlockNum;
    property BlockLength: Byte read GetBlockLength;
    property CheckSum: Byte read GetCheckSum;
  end;

  TLibsDirEntry = packed record
  private
    FBuffer: array [0..12] of Byte;
    function GetBlockNum: UInt32;
    function GetEntryType: Byte;
    function GetName: AnsiString;
    function GetRecordNum: Byte;
  public
    property Name: AnsiString read GetName;
    property EntryType: Byte read GetEntryType;
    property BlockNum: UInt32 read GetBlockNum;
    property RecordNum: Byte read GetRecordNum;
  end;

  TLibsDirBlock = packed record
  public
    Header: TLibsHdr;
    Entries: array [0..18] of TLibsDirEntry;
    Reserved: array [0..3] of Byte;
  end;

  TLibsDataBlock = packed record
    Header: TLibsHdr;
    FBuffer: array [0..250] of Byte;
  end;

  TLibsDataHeader = packed record
  private
    FBuffer: array [0..1] of Byte;
    function GetRecordLength: Byte;
    function GetRecordType: Byte;
  public
    property RecordLength: Byte read GetRecordLength;
    property RecordType: Byte read GetRecordType;
  end;
  PLibsDataHeader = ^TLibsDataHeader;

  TLibsSourceHeader = packed record
  private
    FBuffer: array [0..57] of Byte;
    function GetRecordLength: Byte;
    function GetRecordType: Byte;
    function GetFlags: UInt16;
    function GetName: AnsiString;
    function GetComments: AnsiString;
    function GetDate: UInt32;
    function GetTime: UInt16;
  public
    property RecordLength: Byte read GetRecordLength;
    property RecordType: Byte read GetRecordType;
    property Flags: UInt16 read GetFlags;
    property Name: AnsiString read GetName;
    property Date: UInt32 read GetDate;
    property Time: UInt16 read GetTime;
    property Comments: AnsiString read GetComments;
  end;
  PLibsSourceHeader = ^TLibsSourceHeader;

  TOS3File = class(TObject)
  private
    FDisk: T8418Disk;
    FFileName: String;
    FBlocksPerTrack: Byte;
    FVtocInfo: TVtocFile;
    FPartitions: TPartitionList;
    procedure RelBlockToAbs(part, blkNum: Integer; var cyl, head, rec: Integer);
    function GetPartitionCount: Integer;
    function GetPartition(idx: Integer): TPartition;
  public
    constructor Create(dsk: T8418Disk; fname: String); virtual;
    destructor Destroy; override;
    procedure Open; virtual;
    procedure ReadBlock(part, blkNum: Integer; bfr:PByte); virtual;
    property BlocksPerTrack: Byte read FBlocksPerTrack;
    property FileName: String read FFileName;
    property PartitionCount: Integer read GetPartitionCount;
    property Partitions[idx: Integer]: TPartition read GetPartition;
  end;

  TSATFile = class(TOS3File)
  public
    procedure Open; override;
    procedure LaceAdjust(part: Integer; var rec: Integer);
    procedure ReadBlock(part, blkNum: Integer; bfr:PByte); override;
  end;

  TLibsFile = class(TSATFile)
  private
    FLastDirBlockNum: UInt32;
    FLastDirEntry: Integer;
    FLastDirBlock: TLibsDirBlock;
    FLastDataBlockNum: UInt32;
    FLastDataOffset: Byte;
    FEof: Boolean;
    FLastDataBlock: TLibsDataBlock;
  public
    constructor Create(dsk: T8418Disk; fname: String); override;
    function FindDir(name: String; typ: Byte): TLibsDirEntry;
    function FirstDir: TLibsDirEntry;
    function NextDir: TLibsDirEntry;
    procedure OpenElement(name: string; typ: Byte);
    function ReadSource: AnsiString;
    property Eof: Boolean read FEof;
  end;

implementation

uses EmulatorTypes;

{ TOS3File }

constructor TOS3File.Create(dsk: T8418Disk; fname: String);
begin
    inherited Create;
    FDisk := dsk;
    FFileName := TrimRight(fname);
    FPartitions := TPartitionList.Create;
    FPartitions.OwnsObjects := True;
end;

destructor TOS3File.Destroy;
begin
    FreeAndNil(FPartitions);
    inherited;
end;

function TOS3File.GetPartition(idx: Integer): TPartition;
begin
    Result := FPartitions[idx];
end;

function TOS3File.GetPartitionCount: Integer;
begin
    Result := FPartitions.Count;
end;

procedure TOS3File.Open;
var
    vtoc: TVtoc;
    p: TPartition;
    i, j: Integer;
    ext, trk, numTrk: Integer;
begin
    vtoc := TVtoc.Create;
    try
        vtoc.Open(FDisk);
        FVtocInfo := vtoc.FindFile(FFileName);
        if (not FVtocInfo.FileFound) then
            raise Exception.CreateFmt('Invalid file name (%s)', [FFileName]);
        FBlocksPerTrack := vtoc.Fmt4.DL_BK4;
    finally
        vtoc.Free;
    end;

    for i := 1 to FVtocInfo.Fmt1.DL_PC1 do
    begin
        p := TPartition.Create;
        FPartitions.Add(p);
        p.BlockSize := FVtocInfo.Fmt1.DL_BL1[i];
        p.RecordSize := FVtocInfo.Fmt1.DL_RL1[i];
        for j := 1 to 40 do
        begin
            DecodeVtocPExtent(FVtocInfo.Fmt2.DL_SXAR2[J], ext, trk, numTrk);
            if (ext = i) then
            begin
                p.AddExtent(trk, numTrk);
            end;
        end;
    end;
end;

procedure TOS3File.ReadBlock(part, blkNum: Integer; bfr: PByte);
var
 cyl, head, rec: Integer;
 count: Integer;
begin
    RelBlockToAbs(part, blkNum, cyl, head, rec);
    count := 0;
    while (count < FPartitions[part].BlockSize) do
    begin
        FDisk.ReadSector(cyl, head, rec, bfr);
        Inc(count, FPartitions[part].RecordSize);
    end;
end;

procedure TOS3File.RelBlockToAbs(part, blkNum: Integer; var cyl, head, rec: Integer);
var
    offset, relTrk, i: Integer;
begin
    offset := (blkNum - 1) * FPartitions[part].BlockSize;
    relTrk := offset div FDisk.TrackSize;
    offset := offset - (relTrk * FDisk.TrackSize);
    rec := ((offset mod FDisk.TrackSize) div FDisk.SectorSize) + 1;
    i := 0;
    while ((i < FPartitions[part].ExtentCount) and
           (relTrk > FPartitions[part].Extents[i].NumTracks)) do
    begin
        Dec(relTrk, FPartitions[part].Extents[i].NumTracks);
        Inc(i);
    end;
    if (i >= FPartitions[part].ExtentCount) then
        raise Exception.Create('Invalid block #');
    head := FPartitions[part].Extents[i].FirstTrack + relTrk;
    cyl := head div FDisk.MaxTrack;
    head := head mod FDisk.MaxTrack;
end;

{ TPartition }

procedure TPartition.AddExtent(trk, num: Integer);
var
    pe: TPartitionExtent;
begin
    pe := TPartitionExtent.Create;
    pe.FirstTrack := trk;
    pe.NumTracks := num;
    FExtentList.Add(pe);
end;

constructor TPartition.Create;
begin
    inherited;
    FExtentList := TPEList.Create;
    FExtentList.OwnsObjects := True;
end;

destructor TPartition.Destroy;
begin
    FreeAndNil(FExtentList);
    inherited;
end;

function TPartition.GetExtent(idx: Integer): TPartitionExtent;
begin
    Result := FExtentList[idx];
end;

function TPartition.GetExtentCount: Integer;
begin
    Result := FExtentList.Count;
end;

{ TSATFile }

procedure TSATFile.LaceAdjust(part: Integer; var rec: Integer);
// Calculate the record to read based on the libraries lace factor.
// I'm not completely certain that this is correct but it seems to
// work.
var
    laceRec: Integer;
begin
    laceRec := ((rec - 1) * Integer(FPartitions[part].LaceFactor)) mod FDisk.MaxSector;
    if (rec > Integer(FPartitions[part].RotnAdjust)) then
        Inc(laceRec, (rec - 1) div Integer(FPartitions[part].RotnAdjust));
    Inc(laceRec);
    rec := laceRec;
end;

procedure TSATFile.Open;
begin
    inherited;
    if (FPartitions.Count > 0) then
    begin
        FPartitions[0].FLaceFactor := FVtocInfo.Fmt2.DL_DIRL2;
        FPartitions[0].FRotnAdjust := FVtocInfo.Fmt2.DL_DIRF2;
        if (FPartitions.Count > 1) then
        begin
            FPartitions[1].FLaceFactor := FVtocInfo.Fmt2.DL_TXTL2;
            FPartitions[1].FRotnAdjust := FVtocInfo.Fmt2.DL_TXTF2;
        end;
    end;
end;

procedure TSATFile.ReadBlock(part, blkNum: Integer; bfr: PByte);
var
 cyl, head, rec: Integer;
 count: Integer;
begin
    RelBlockToAbs(part, blkNum, cyl, head, rec);
    LaceAdjust(part, rec);
    count := 0;
    while (count < FPartitions[part].BlockSize) do
    begin
        FDisk.ReadSector(cyl, head, rec, bfr);
        Inc(count, FPartitions[part].RecordSize);
    end;
end;

{ TLibsDirHdr }

function TLibsHdr.GetBlockLength: Byte;
begin
    Result := FBuffer[3];
end;

function TLibsHdr.GetBlockNum: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 0, 2);
end;

function TLibsHdr.GetCheckSum: Byte;
begin
    Result := FBuffer[4];
end;

{ TLibsDirEntry }

function TLibsDirEntry.GetBlockNum: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 9, 11);
end;

function TLibsDirEntry.GetEntryType: Byte;
begin
    Result := FBuffer[8];
end;

function TLibsDirEntry.GetName: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 0, 7));
end;

function TLibsDirEntry.GetRecordNum: Byte;
begin
    Result := FBuffer[12];
end;

{ TLibsFile }

constructor TLibsFile.Create(dsk: T8418Disk; fname: String);
begin
    inherited;
    FLastDirBlockNum := 0;
end;

function TLibsFile.FindDir(name: String; typ: Byte): TLibsDirEntry;
begin
    name := TrimRight(name);
    Result := FirstDir;
    while ((Result.EntryType <> 0) and (Result.EntryType <> $a1)) do
    begin
        if ((name = TrimRight(String(Result.Name))) and (typ = Result.EntryType)) then
            Exit;
        Result := NextDir;
    end;
    FillChar(Result, SizeOf(Result), 0);
end;

function TLibsFile.FirstDir: TLibsDirEntry;
begin
    FLastDirBlockNum := 1;
    FLastDirEntry := -1;
    ReadBlock(0, FLastDirBlockNum, PByte(@FLastDirBlock));
    if (FLastDirBlock.Header.BlockNum <> FLastDirBlockNum) then
        raise Exception.CreateFmt('LIBS read error. Bad block #. Expected %d got %d',
                                  [FLastDirBlockNum, FLastDirBlock.Header.BlockNum]);
    Result := NextDir;
end;

function TLibsFile.NextDir: TLibsDirEntry;
var
    i: Integer;
    done: Boolean;
begin
    if (FLastDirBlockNum = 0) then
        raise Exception.Create('FirstDir must be called before NextDir');

    FillChar(Result, SizeOf(Result), 0);
    done := False;
    while (not done) do
    begin
        for i := FLastDirEntry + 1 to High(FLastDirBlock.Entries) do
        begin
            FLastDirEntry := i;
            if (FLastDirBlock.Entries[i].EntryType <> 0) then
            begin
                if (FLastDirBlock.Entries[i].EntryType = $a1) then
                begin
                    // EOF
                    FLastDirBlockNum := 0;
                    Exit;
                end else
                begin
                    // Got one
                    Result := FLastDirBlock.Entries[i];
                    Exit;
                end;
            end;
        end;
        Inc(FLastDirBlockNum);
        FLastDirEntry := -1;
        ReadBlock(0, FLastDirBlockNum, PByte(@FLastDirBlock));
        if (FLastDirBlock.Header.BlockNum <> FLastDirBlockNum) then
            raise Exception.CreateFmt('LIBS read error. Bad block #. Expected %d got %d',
                                      [FLastDirBlockNum, FLastDirBlock.Header.BlockNum]);
    end;
end;

procedure TLibsFile.OpenElement(name: string; typ: Byte);
var
    dir: TLibsDirEntry;
    sh: PLibsSourceHeader;
begin
    dir := FindDir(name, typ);
    if (dir.EntryType = 0) then
        raise Exception.Create('Unknown element name');
    FLastDataBlockNum := dir.BlockNum;
    FLastDataOffset := dir.RecordNum - SizeOf(FLastDataBlock.Header);
    ReadBlock(1, dir.BlockNum, PByte(@FLastDataBlock));
    if (FLastDataBlockNum <> FLastDataBlock.Header.BlockNum) then
            raise Exception.CreateFmt('LIBS read error. Bad block #. Expected %d got %d',
                                      [FLastDataBlockNum, FLastDataBlock.Header.BlockNum]);
    sh := PLibsSourceHeader(@FLastDataBlock.FBuffer[FLastDataOffset]);
    if (sh.RecordType <> typ) then
        raise Exception.CreateFmt('LIBS read error. Bad header type #. Expected %x got %x',
                                  [typ, sh.RecordType]);
    Inc(FLastDataOffset, sh.RecordLength + 2);
    FEof := False;
end;

function TLibsFile.ReadSource: AnsiString;
var
    dh: PLibsDataHeader;
    src: PByte;
    b, l: Byte;
    rlen: Integer;
begin
    Result := '';
    if (FLastDataOffset >= FLastDataBlock.Header.BlockLength) then
    begin
        // End of block
        Inc(FLastDataBlockNum);
        ReadBlock(1, FLastDataBlockNum, PByte(@FLastDataBlock));
        FLastDataOffset := 0;
    end;

    dh := PLibsDataHeader(@FLastDataBlock.FBuffer[FLastDataOffset]);
    src := @FLastDataBlock.FBuffer[FLastDataOffset + 2];
    rlen := dh.RecordLength;
    if (dh.RecordType = $24) then
    begin
        Result := TCodeTranslator.EbcdicToAscii(ByteToString(src, rlen));
    end else if (dh.RecordType = $25) then
    begin
        while (rlen > 0) do
        begin
            l := (src)^ + 1;                                // length of text
            b := (src + 1)^;                                // # blanks
            Result := Result + StringOfChar(' ', b);
            Result := Result + TCodeTranslator.EbcdicToAscii(ByteToString(src + 2, l));
            Inc(src, l + 2);
            Dec(rlen, l + 2);
        end;
    end else if ((dh.RecordType = 0) or (dh.RecordType = $a1)) then
    begin
        FEof := True;
    end else
        raise Exception.CreateFmt('LIBS read error. Bad header type #. Expected 24 or 25 got %x',
                                  [dh.RecordType]);
    Inc(FLastDataOffset, dh.RecordLength + 2);
end;

{ TLibsDataHeader }

function TLibsDataHeader.GetRecordLength: Byte;
begin
    Result := FBuffer[0];
end;

function TLibsDataHeader.GetRecordType: Byte;
begin
    Result := FBuffer[1];
end;

{ TLibsSourceHeader }

function TLibsSourceHeader.GetComments: AnsiString;
begin
    Result := ByteToString(FBuffer, 28, 57);
end;

function TLibsSourceHeader.GetDate: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 22, 24);
end;

function TLibsSourceHeader.GetFlags: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 3);
end;

function TLibsSourceHeader.GetName: AnsiString;
begin
    Result := ByteToString(FBuffer, 14, 21);
end;

function TLibsSourceHeader.GetRecordLength: Byte;
begin
    Result := FBuffer[0];
end;

function TLibsSourceHeader.GetRecordType: Byte;
begin
    Result := FBuffer[1];
end;

function TLibsSourceHeader.GetTime: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 25);
end;

end.
