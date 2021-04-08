unit VTOC;

// OS/3 VTOC label formats. For a description of these see:
//
// UP-8068 OS/3 Basic Data Management User Guide Appendix D
//

interface

uses SysUtils, Classes, U8418;

const
    VTOC_VOL1_CYL = 0;
    VTOC_VOL1_HEAD = 0;
    VTOC_VOL1_REC = 3;

type
  TVtocVol1 = packed record
  private
    FBuffer: array [0..255] of Byte;
    function GetDL_VL: AnsiString;
    function GetDL_VL1: AnsiString;
    function GetDL_VSN: AnsiString;
    function GetDL_VTC: UInt64;
  public
    property DL_VL: AnsiString read GetDL_VL;
    property DL_VL1: AnsiString read GetDL_VL1;
    property DL_VSN: AnsiString read GetDL_VSN;
    property DL_VTC: UInt64 read GetDL_VTC;
  end;
  PVtocVol1 = ^TVtocVol1;

  TVtocFmt1 = packed record
  private
    FBuffer: array [0..255] of Byte;
    function GetDL_KEY1: AnsiString;
    function GetDL_ID1: AnsiString;
    function GetDL_FS1: AnsiString;
    function GetDL_VS1: UInt16;
    function GetDL_CD1: UInt32;
    function GetDL_ED1: UInt32;
    function GetDL_XC1: Byte;
    function GetDL_OC1: Byte;
    function GetDL_PC1: Byte;
    function GetDL_FT1: Byte;
    function GetDL_FT2: Byte;
    function GetDL_BL1(idx: Integer): UInt16;
    function GetDL_RL1(idx: Integer): UInt16;
    function GetDL_RF1(idx: Integer): Byte;
    function GetDL_DS1: Byte;
    function GetDL_KL1: UInt16;
    function GetDL_SA1: Byte;
    function GetDL_LH1: Byte;
    function GetDL_HH1: Byte;
    function GetDL_XT1(idx: Integer): Byte;
    function GetDL_XS1(idx: Integer): Byte;
    function GetDL_XL1(idx: Integer): UInt32;
    function GetDL_XU1(idx: Integer): UInt32;
    function GetDL_CP1: UInt64;
  public
    property DL_KEY1: AnsiString read GetDL_KEY1;
    property DL_ID1: AnsiString read GetDL_ID1;
    property DL_FS1: AnsiString read GetDL_FS1;
    property DL_VS1: UInt16 read GetDL_VS1;
    property DL_CD1: UInt32 read GetDL_CD1;
    property DL_ED1: UInt32 read GetDL_ED1;
    property DL_XC1: Byte read GetDL_XC1;
    property DL_OC1: Byte read GetDL_OC1;
    property DL_PC1: Byte read GetDL_PC1;
    property DL_FT1: Byte read GetDL_FT1;
    property DL_FT2: Byte read GetDL_FT2;
    property DL_BL1[idx: Integer]: UInt16 read GetDL_BL1;
    property DL_RL1[idx: Integer]: UInt16 read GetDL_RL1;
    property DL_RF1[idx: Integer]: Byte read GetDL_RF1;
    property DL_DS1: Byte read GetDL_DS1;
    property DL_KL1: UInt16 read GetDL_KL1;
    property DL_SA1: Byte read GetDL_SA1;
    property DL_LH1: Byte read GetDL_LH1;
    property DL_HH1: Byte read GetDL_HH1;
    property DL_XT1[idx: Integer]: Byte read GetDL_XT1;
    property DL_XS1[idx: Integer]: Byte read GetDL_XS1;
    property DL_XL1[idx: Integer]: UInt32 read GetDL_XL1;
    property DL_XU1[idx: Integer]: UInt32 read GetDL_XU1;
    property DL_CP1: UInt64 read GetDL_CP1;
  end;
  PVtocFmt1 = ^TVtocFmt1;

  TVtocFmt2 = packed record
  private
    FBuffer: array [0..255] of Byte;
    function GetDL_SID2: Byte;
    function GetDL_SPC2(idx: Integer): Byte;
    function GetDL_SEP2(idx: Integer): UInt32;
    function GetDL_SLF2(idx: Integer): Byte;
    function GetDL_SBPT2: UInt16;
    function GetDL_SXAR2(idx: Integer): UInt32;
    function GetDL_TPC2: UInt16;
    function GetDL_FLH2: UInt16;
    function GetDL_SXCT2: UInt16;
    function GetDL_SFL2: Byte;
    function GetDL_SCID2: UInt64;
    function GetDL_DIRL2: UInt32;
    function GetDL_DIRF2: UInt32;
    function GetDL_TXTL2: UInt32;
    function GetDL_TXTF2: UInt32;
    function GetDL_LBPT2: UInt32;
  public
    property DL_SID2: Byte read GetDL_SID2;
    property DL_SPC2[idx: Integer]: Byte read GetDL_SPC2;
    property DL_SLF2[idx: Integer]: Byte read GetDL_SLF2;
    property DL_SEP2[idx: Integer]: UInt32 read GetDL_SEP2;
    property DL_SBPT2: UInt16 read GetDL_SBPT2;
    property DL_SXAR2[idx: Integer]: UInt32 read GetDL_SXAR2;
    property DL_TPC2: UInt16 read GetDL_TPC2;
    property DL_FLH2: UInt16 read GetDL_FLH2;
    property DL_SXCT2: UInt16 read GetDL_SXCT2;
    property DL_SFL2: Byte read GetDL_SFL2;
    property DL_SCID2: UInt64 read GetDL_SCID2;
    property DL_DIRL2: UInt32 read GetDL_DIRL2;
    property DL_DIRF2: UInt32 read GetDL_DIRF2;
    property DL_TXTL2: UInt32 read GetDL_TXTL2;
    property DL_TXTF2: UInt32 read GetDL_TXTF2;
    property DL_LBPT2: UInt32 read GetDL_LBPT2;
  end;
  PVtocFmt2 = ^TVtocFmt2;

  TVtocFmt3 = packed record
  private
    FBuffer: array [0..255] of Byte;
    function GetDL_ID3: UInt32;
    function GetDL_XT3(idx: Integer): Byte;
    function GetDL_SN3(idx: Integer): Byte;
    function GetDL_XL3(idx: Integer): UInt32;
    function GetDL_XU3(idx: Integer): UInt32;
    function GetDL_CP3: UInt64;
  public
    property DL_ID3: UInt32 read GetDL_ID3;
    property DL_XT3[idx: Integer]: Byte read GetDL_XT3;
    property DL_SN3[idx: Integer]: Byte read GetDL_SN3;
    property DL_XL3[idx: Integer]: UInt32 read GetDL_XL3;
    property DL_XU3[idx: Integer]: UInt32 read GetDL_XU3;
    property DL_CP3: UInt64 read GetDL_CP3;
  end;
  PVtocFmt3 = ^TVtocFmt3;

  TVtocFmt4 = packed record
  private
    FBuffer: array [0..255] of Byte;
    function GetDL_KY4: AnsiString;
    function GetDL_ID4: AnsiString;
    function GetDL_LF4: UInt64;
    function GetDL_AF4: UInt16;
    function GetDL_HA4: UInt32;
    function GetDL_AT4: UInt32;
    function GetDL_VI4: Byte;
    function GetDL_XC4: Byte;
    function GetDL_DS4: UInt32;
    function GetDL_TL4: UInt16;
    function GetDL_RO4: UInt32;
    function GetDL_FG4: Byte;
    function GetDL_TO4: UInt16;
    function GetDL_BK4: Byte;
    function GetDL_LT4: Byte;
    function GetDL_F04: UInt64;
    function GetDL_F64: UInt64;
    function GetDL_XT4: Byte;
    function GetDL_XS4: Byte;
    function GetDL_XL4: UInt32;
    function GetDL_XU4: UInt32;
  public
    property DL_KY4: AnsiString read GetDL_KY4;
    property DL_ID4: AnsiString read GetDL_ID4;
    property DL_LF4: UInt64 read GetDL_LF4;
    property DL_AF4: UInt16 read GetDL_AF4;
    property DL_HA4: UInt32 read GetDL_HA4;
    property DL_AT4: UInt32 read GetDL_AT4;
    property DL_VI4: Byte read GetDL_VI4;
    property DL_XC4: Byte read GetDL_XC4;
    property DL_DS4: UInt32 read GetDL_DS4;
    property DL_TL4: UInt16 read GetDL_TL4;
    property DL_RO4: UInt32 read GetDL_RO4;
    property DL_FG4: Byte read GetDL_FG4;
    property DL_TO4: UInt16 read GetDL_TO4;
    property DL_LT4: Byte read GetDL_LT4;
    property DL_BK4: Byte read GetDL_BK4;
    property DL_F04: UInt64 read GetDL_F04;
    property DL_F64: UInt64 read GetDL_F64;
    property DL_XT4: Byte read GetDL_XT4;
    property DL_XS4: Byte read GetDL_XS4;
    property DL_XL4: UInt32 read GetDL_XL4;
    property DL_XU4: UInt32 read GetDL_XU4;
  end;
  PVtocFmt4 = ^TVtocFmt4;

  TVtocFile = record
  private
    FFileFound: Boolean;
    FFmt1: TVtocFmt1;
    FFmt2: TVtocFmt2;
    FFmt3: TVtocFmt3;
    function GetFmt1: PVtocFmt1;
    function GetFmt2: PVtocFmt2;
    function GetFmt3: PVtocFmt3;
  public
    constructor Create(fmt1: TVtocFmt1);
    property FileFound: Boolean read FFileFound;
    property Fmt1: PVtocFmt1 read GetFmt1;
    property Fmt2: PVtocFmt2 read GetFmt2;
    property Fmt3: PVtocFmt3 read GetFmt3;
  end;

  TVtoc = class(TObject)
  private
    FDisk: T8418Disk;
    FVol1: TVtocVol1;
    FFmt4: TVtocFmt4;
    FFmt4Valid: Boolean;
    FLastFile: TVtocFile;
    FLastFileFmt1: UInt64;
    function GetFmt4: PVtocFmt4;
    function GetVol1: PVtocVol1;
    procedure LoadFileEntry(var fil: TVtocFile);
  public
    constructor Create;
    destructor Destroy; override;
    function FindFile(fname: String): TVtocFile;
    function FirstFile: TVtocFile;
    function NextFile: TVtocFile;
    procedure Open(disk: T8418Disk);
    property Vol1: PVtocVol1 read GetVol1;
    property Fmt4: PVtocFmt4 read GetFmt4;
  end;

  procedure DecodeVtocCCHHR(addr: UInt64; var cyl, trk, sec: Integer);
  procedure DecodeVtocCCHH(addr: UInt32; var cyl, trk: Integer);
  procedure DecodeVtocDate(dt: UInt32; var y, d: Integer);
  procedure DecodeVtocPExtent(pe: UInt32; var ext, trk, numTrk: Integer);

implementation

uses EmulatorTypes;

function VTocCCHHR(bfr: array of Byte; strt: Integer): UInt64;
begin
    Result := (bfr[strt] shl 32) or
              (bfr[strt + 1] shl 24) or
              (bfr[strt + 2] shl 16) or
              (bfr[strt + 3] shl 8) or
               bfr[strt + 4];
end;

procedure DecodeVtocCCHHR(addr: UInt64; var cyl, trk, sec: Integer);
begin
    sec := addr and $ff;
    trk := (addr shr 8) and $ffff;
    cyl := (addr shr 24) and $ffff;
end;

function EncodeVtocCCHHR(cyl, trk, sec: Integer): UInt64;
begin
    Result := UInt64(sec) or
              (UInt64(trk) shl 8) or
              (UInt64(cyl) shl 24);
end;

procedure DecodeVtocCCHH(addr: UInt32; var cyl, trk: Integer);
begin
    trk := addr and $ffff;
    cyl := (addr shr 16) and $ffff;
end;

procedure DecodeVtocDate(dt: UInt32; var y, d: Integer);
begin
    d := dt and $ffff;
    y := (dt shr 16) and $ff;
end;

procedure DecodeVtocPExtent(pe: UInt32; var ext, trk, numTrk: Integer);
begin
    ext := pe shr 29;
    trk := (pe shr 16) and $1fff;
    numTrk := pe and $ffff;
end;

{ TVtocVol1 }

function TVtocVol1.GetDL_VL: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 0, 3));
end;

function TVtocVol1.GetDL_VL1: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 4, 6));
end;

function TVtocVol1.GetDL_VSN: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 8, 13));
end;

function TVtocVol1.GetDL_VTC: UInt64;
begin
    Result := VTocCCHHR(FBuffer, 15);
end;

{ TVtocFmt4 }

function TVtocFmt4.GetDL_AF4: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 50);
end;

function TVtocFmt4.GetDL_AT4: UInt32;
begin
    Result := ByteToUInt16(FBuffer, 56);
end;

function TVtocFmt4.GetDL_BK4: Byte;
begin
    Result := FBuffer[75];
end;

function TVtocFmt4.GetDL_DS4: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 62);
end;

function TVtocFmt4.GetDL_F64: UInt64;
begin
    Result := VTocCCHHR(FBuffer, 100);
end;

function TVtocFmt4.GetDL_FG4: Byte;
begin
    Result := FBuffer[71];
end;

function TVtocFmt4.GetDL_F04: UInt64;
begin
    Result := VTocCCHHR(FBuffer, 76);
end;

function TVtocFmt4.GetDL_HA4: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 52);
end;

function TVtocFmt4.GetDL_ID4: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 44, 44));
end;

function TVtocFmt4.GetDL_KY4: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 0, 43));
end;

function TVtocFmt4.GetDL_LF4: UInt64;
begin
    Result := VTocCCHHR(FBuffer, 45);
end;

function TVtocFmt4.GetDL_LT4: Byte;
begin
    Result := FBuffer[74];
end;

function TVtocFmt4.GetDL_RO4: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 68, 70);
end;

function TVtocFmt4.GetDL_TL4: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 66);
end;

function TVtocFmt4.GetDL_TO4: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 72);
end;

function TVtocFmt4.GetDL_VI4: Byte;
begin
    Result := FBuffer[58];
end;

function TVtocFmt4.GetDL_XC4: Byte;
begin
    Result := FBuffer[59];
end;

function TVtocFmt4.GetDL_XL4: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 107);
end;

function TVtocFmt4.GetDL_XS4: Byte;
begin
    Result := FBuffer[106];
end;

function TVtocFmt4.GetDL_XT4: Byte;
begin
    Result := FBuffer[105];
end;

function TVtocFmt4.GetDL_XU4: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 111);
end;

{ TVtocFmt1 }

function TVtocFmt1.GetDL_BL1(idx: Integer): UInt16;
begin
    Result := ByteToUInt16(FBuffer, ((idx - 1) * 5) + 64);
end;

function TVtocFmt1.GetDL_CD1: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 53, 55);
end;

function TVtocFmt1.GetDL_CP1: UInt64;
begin
    Result := VTocCCHHR(FBuffer, 135);
end;

function TVtocFmt1.GetDL_DS1: Byte;
begin
    Result := FBuffer[99];
end;

function TVtocFmt1.GetDL_ED1: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 56, 58);
end;

function TVtocFmt1.GetDL_FS1: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 45, 50));
end;

function TVtocFmt1.GetDL_FT1: Byte;
begin
    Result := FBuffer[62];
end;

function TVtocFmt1.GetDL_FT2: Byte;
begin
    Result := FBuffer[63];
end;

function TVtocFmt1.GetDL_HH1: Byte;
begin
    Result := FBuffer[104];
end;

function TVtocFmt1.GetDL_ID1: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 44, 44));
end;

function TVtocFmt1.GetDL_KEY1: AnsiString;
begin
    Result := TCodeTranslator.EbcdicToAscii(ByteToString(FBuffer, 0, 43));
end;

function TVtocFmt1.GetDL_KL1: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 100);
end;

function TVtocFmt1.GetDL_LH1: Byte;
begin
    Result := FBuffer[103];
end;

function TVtocFmt1.GetDL_OC1: Byte;
begin
    Result := FBuffer[60];
end;

function TVtocFmt1.GetDL_PC1: Byte;
begin
    Result := FBuffer[61];
end;

function TVtocFmt1.GetDL_RF1(idx: Integer): Byte;
begin
    Result := FBuffer[((idx - 1) * 5) + 68];
end;

function TVtocFmt1.GetDL_RL1(idx: Integer): UInt16;
begin
    Result := ByteToUInt16(FBuffer, ((idx - 1) * 5) + 66);
end;

function TVtocFmt1.GetDL_SA1: Byte;
begin
    Result := FBuffer[102];
end;

function TVtocFmt1.GetDL_VS1: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 51);
end;

function TVtocFmt1.GetDL_XC1: Byte;
begin
    Result := FBuffer[59];
end;

function TVtocFmt1.GetDL_XL1(idx: Integer): UInt32;
var
    start: Integer;
begin
    start := ((idx - 1) * 10) + 107;
    Result := ByteToUInt32(FBuffer, start);
end;

function TVtocFmt1.GetDL_XS1(idx: Integer): Byte;
begin
    Result := FBuffer[((idx - 1) * 10) + 106];
end;

function TVtocFmt1.GetDL_XT1(idx: Integer): Byte;
begin
    Result := FBuffer[((idx - 1) * 10) + 105];
end;

function TVtocFmt1.GetDL_XU1(idx: Integer): UInt32;
var
    start: Integer;
begin
    start := ((idx - 1) * 10) + 111;
    Result := ByteToUInt32(FBuffer, start);
end;

{ TVtoc }

constructor TVtoc.Create;
begin
    inherited;
end;

destructor TVtoc.Destroy;
begin
    inherited;
end;

function TVtoc.FindFile(fname: String): TVtocFile;
begin
    fname := TrimRight(fname);
    Result := FirstFile;
    while (Result.FileFound) do
    begin
        if (fname = TrimRight(String(Result.Fmt1.GetDL_KEY1))) then
            Exit;
        Result := NextFile;
    end;
end;

function TVtoc.FirstFile: TVtocFile;
var
    lowCyl, lowHead: Integer;
    highCyl, highHead: Integer;
    cyl, head, rec: Integer;
    fmt1: TVtocFmt1;
begin
    Result.FFileFound := False;
    FLastFileFmt1 := 0;
    DecodeVtocCCHH(Fmt4.DL_XL4, lowCyl, lowHead);
    DecodeVtocCCHH(Fmt4.DL_XU4, highCyl, highHead);
    for cyl := lowCyl to highCyl do
        for head := lowHead to highHead do
            for rec := 1 to FDisk.MaxSector do
            begin
                FDisk.ReadSector(cyl, head, rec, PByte(@fmt1));
                if (fmt1.DL_ID1 = '1') then
                begin
                    FLastFileFmt1 := EncodeVtocCCHHR(cyl, head, rec);
                    FLastFile := TVtocFile.Create(fmt1);
                    Result := FLastFile;
                    Result.FFileFound := True;
                    LoadFileEntry(Result);
                    Exit;
                end;
            end;
end;

function TVtoc.GetFmt4: PVtocFmt4;
var
    cyl, head, rec: Integer;
begin
    if (not FFmt4Valid) then
    begin
        DecodeVtocCCHHR(FVol1.DL_VTC, cyl, head, rec);
        FDisk.ReadSector(cyl, head, rec, PByte(@FFmt4));
        if (FFmt4.DL_ID4 <> '4') then
            raise Exception.Create('FMT4 label not found');
        FFmt4Valid := True;
    end;
    Result := @FFmt4;
end;

function TVtoc.GetVol1: PVtocVol1;
begin
    Result := @FVol1;
end;

procedure TVtoc.LoadFileEntry(var fil: TVtocFile);
var
    cyl, head, rec, i: Integer;
begin
    DecodeVtocCCHHR(fil.Fmt1.DL_CP1, cyl, head, rec);
    FDisk.ReadSector(cyl, head, rec, PByte(@fil.FFmt2));
    if (fil.Fmt2.DL_SID2 <> 2) then
        raise Exception.Create('FMT2 record not found');
    if (fil.Fmt2.DL_SCID2 <> 0) then
    begin
        { TODO : Need to allow for multiple format 3 records }
        DecodeVtocCCHHR(fil.Fmt2.DL_SCID2, cyl, head, rec);
        FDisk.ReadSector(cyl, head, rec, PByte(@fil.FFmt3));
        if (fil.Fmt3.DL_ID3 <> $03030303) then
            raise Exception.Create('FMT3 record not found');
    end else
    begin
        for i := 0 to 255 do
            fil.FFmt3.FBuffer[i] := 0;
    end;
end;

function TVtoc.NextFile: TVtocFile;
var
    lowCyl, lowHead, lowRec: Integer;
    highCyl, highHead: Integer;
    cyl, head, rec: Integer;
    fmt1: TVtocFmt1;
begin
    Result.FFileFound := False;
    if (FLastFileFmt1 = 0) then
        raise Exception.Create('FirstFile must be called before NextFile');

    DecodeVtocCCHHR(FLastFileFmt1, lowCyl, lowHead, lowRec);
    DecodeVtocCCHH(Fmt4.DL_XU4, highCyl, highHead);
    FLastFileFmt1 := 0;
    for cyl := lowCyl to highCyl do
        for head := lowHead to highHead do
        begin
            for rec := lowRec + 1 to FDisk.MaxSector do
            begin
                FDisk.ReadSector(cyl, head, rec, PByte(@fmt1));
                if (fmt1.DL_ID1 = '1') then
                begin
                    FLastFileFmt1 := EncodeVtocCCHHR(cyl, head, rec);
                    FLastFile := TVtocFile.Create(fmt1);
                    Result := FLastFile;
                    Result.FFileFound := True;
                    LoadFileEntry(Result);
                    Exit;
                end;
            end;
            lowRec := 1;
        end;
end;

procedure TVtoc.Open(disk: T8418Disk);
begin
    FDisk := disk;
    FDisk.ReadSector(VTOC_VOL1_CYL, VTOC_VOL1_HEAD, VTOC_VOL1_REC, PByte(@FVol1));
    if (FVol1.DL_VL <> 'VOL1') then
        raise Exception.Create('VOL1 label not found');
    FFmt4Valid := False;
    FreeAndNil(FLastFile);
    FLastFileFmt1 := 0;
end;

{ TVtocFile }

constructor TVtocFile.Create(fmt1: TVtocFmt1);
begin
    FFmt1 := fmt1;
end;

function TVtocFile.GetFmt1: PVtocFmt1;
begin
    Result := @FFmt1;
end;

function TVtocFile.GetFmt2: PVtocFmt2;
begin
    Result := @FFmt2;
end;

function TVtocFile.GetFmt3: PVtocFmt3;
begin
    Result := @FFmt3;
end;

{ TVtocFmt2 }

function TVtocFmt2.GetDL_DIRF2: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 32);
end;

function TVtocFmt2.GetDL_DIRL2: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 28);
end;

function TVtocFmt2.GetDL_FLH2: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 130);
end;

function TVtocFmt2.GetDL_LBPT2: UInt32;
begin
    Result := ByteToUInt16(FBuffer, 44);
end;

function TVtocFmt2.GetDL_SBPT2: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 46);
end;

function TVtocFmt2.GetDL_SCID2: UInt64;
begin
    Result := VtocCCHHR(FBuffer, 135);
end;

function TVtocFmt2.GetDL_SEP2(idx: Integer): UInt32;
var
    first: Integer;
begin
    first := ((idx - 1) * 6) + 4;
    Result := ByteToUInt32(FBuffer, first, first + 2);
end;

function TVtocFmt2.GetDL_SFL2: Byte;
begin
    Result := FBuffer[134];
end;

function TVtocFmt2.GetDL_SID2: Byte;
begin
    Result := FBuffer[0];
end;

function TVtocFmt2.GetDL_SLF2(idx: Integer): Byte;
begin
    Result := ((idx - 1) * 6) + 3;
end;

function TVtocFmt2.GetDL_SPC2(idx: Integer): Byte;
begin
    Result := ((idx - 1) * 6) + 1;
end;

function TVtocFmt2.GetDL_SXAR2(idx: Integer): UInt32;
var
    start: Integer;
begin
    start := ((idx - 1) * 4) + 48;
    Result := ByteToUInt32(FBuffer, start);
end;

function TVtocFmt2.GetDL_SXCT2: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 132);
end;

function TVtocFmt2.GetDL_TPC2: UInt16;
begin
    Result := ByteToUInt16(FBuffer, 128);
end;

function TVtocFmt2.GetDL_TXTF2: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 40);
end;

function TVtocFmt2.GetDL_TXTL2: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 36);
end;

{ TVtocFmt3 }

function TVtocFmt3.GetDL_CP3: UInt64;
begin
    Result := VtocCCHHR(FBuffer, 135);
end;

function TVtocFmt3.GetDL_ID3: UInt32;
begin
    Result := ByteToUInt32(FBuffer, 0);
end;

function TVtocFmt3.GetDL_SN3(idx: Integer): Byte;
var
    i: Integer;
begin
    i := ((idx - 1) * 10) + 5;
    if (idx > 4) then
        Inc(i);
    Result := FBuffer[i];
end;

function TVtocFmt3.GetDL_XL3(idx: Integer): UInt32;
var
    i: Integer;
begin
    i := ((idx - 1) * 10) + 6;
    if (idx > 4) then
        Inc(i);
    Result := ByteToUInt32(FBuffer, i);
end;

function TVtocFmt3.GetDL_XT3(idx: Integer): Byte;
var
    i: Integer;
begin
    i := ((idx - 1) * 10) + 4;
    if (idx > 4) then
        Inc(i);
    Result := FBuffer[i];
end;

function TVtocFmt3.GetDL_XU3(idx: Integer): UInt32;
var
    i: Integer;
begin
    i := ((idx - 1) * 10) + 10;
    if (idx > 4) then
        Inc(i);
    Result := ByteToUInt32(FBuffer, i);
end;

end.
