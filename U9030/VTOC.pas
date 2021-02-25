unit VTOC;

// OS/3 VTOC label formats. For a description of these see:
//
// UP-8068 OS/3 Basic Data Management User Guide Appendix D
//

interface

uses SysUtils;

type

  TVtocVol1 = packed record
  private
    FBuffer: array [0..83] of Byte;
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
    FBuffer: array [0..139] of Byte;
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

  TVtocFmt4 = packed record
  private
    FBuffer: array [0..139] of Byte;
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

  procedure DecodeVtocAddr(addr: UInt64; var cyl, trk, sec: Integer);
  procedure DecodeVtocCCHH(addr: UInt32; var cyl, trk: Integer);
  procedure DecodeVtocDate(dt: UInt32; var y, d: Integer);

implementation

uses EmulatorTypes;

function StringOf(bfr: array of Byte; strt, fin: Integer): AnsiString;
var
    i: Integer;
    c: AnsiChar;
begin
    Result := '';
    for i := strt to fin do
    begin
        c := TCodeTranslator.EbcdicToAscii(bfr[i]);
        if (c < ' ') then
            c := ' ';
        Result := Result + c;
    end;
end;

function VTocAddr(bfr: array of Byte; strt: Integer): UInt64;
begin
    Result := (bfr[strt] shl 32) or
              (bfr[strt + 1] shl 24) or
              (bfr[strt + 2] shl 16) or
              (bfr[strt + 3] shl 8) or
               bfr[strt + 4];
end;

function VTocHW(bfr: array of Byte; strt: Integer): UInt16;
begin
    Result := (bfr[strt] shl 8) or
               bfr[strt + 1];
end;

function VTocW(bfr: array of Byte; strt: Integer): UInt32;
begin
    Result := (bfr[strt] shl 24) or
              (bfr[strt + 1] shl 16) or
              (bfr[strt + 2] shl 8) or
               bfr[strt + 3];
end;

procedure DecodeVtocAddr(addr: UInt64; var cyl, trk, sec: Integer);
begin
    sec := addr and $ff;
    trk := (addr shr 8) and $ffff;
    cyl := (addr shr 24) and $ffff;
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

{ TVtocVol1 }

function TVtocVol1.GetDL_VL: AnsiString;
begin
    Result := StringOf(FBuffer, 0, 3);
end;

function TVtocVol1.GetDL_VL1: AnsiString;
begin
    Result := StringOf(FBuffer, 4, 6);
end;

function TVtocVol1.GetDL_VSN: AnsiString;
begin
    Result := StringOf(FBuffer, 8, 13);
end;

function TVtocVol1.GetDL_VTC: UInt64;
begin
    Result := VtocAddr(FBuffer, 15);
end;

{ TVtocFmt4 }

function TVtocFmt4.GetDL_AF4: UInt16;
begin
    Result := VtocHW(FBuffer, 50);
end;

function TVtocFmt4.GetDL_AT4: UInt32;
begin
    Result := VtocHW(FBuffer, 56);
end;

function TVtocFmt4.GetDL_BK4: Byte;
begin
    Result := FBuffer[75];
end;

function TVtocFmt4.GetDL_DS4: UInt32;
begin
    Result := VtocW(FBuffer, 62);
end;

function TVtocFmt4.GetDL_F64: UInt64;
begin
    Result := VtocAddr(FBuffer, 100);
end;

function TVtocFmt4.GetDL_FG4: Byte;
begin
    Result := FBuffer[71];
end;

function TVtocFmt4.GetDL_F04: UInt64;
begin
    Result := VtocAddr(FBuffer, 76);
end;

function TVtocFmt4.GetDL_HA4: UInt32;
begin
    Result := VtocW(FBuffer, 52);
end;

function TVtocFmt4.GetDL_ID4: AnsiString;
begin
    Result := StringOf(FBuffer, 44, 44);
end;

function TVtocFmt4.GetDL_KY4: AnsiString;
begin
    Result := StringOf(FBuffer, 0, 43);
end;

function TVtocFmt4.GetDL_LF4: UInt64;
begin
    Result := VtocAddr(FBuffer, 45);
end;

function TVtocFmt4.GetDL_LT4: Byte;
begin
    Result := FBuffer[74];
end;

function TVtocFmt4.GetDL_RO4: UInt32;
begin
    Result := (FBuffer[68] shl 16) or
              (FBuffer[69] shl 8) or
               FBuffer[70];
end;

function TVtocFmt4.GetDL_TL4: UInt16;
begin
    Result := VtocHW(FBuffer, 66);
end;

function TVtocFmt4.GetDL_TO4: UInt16;
begin
    Result := VtocHW(FBuffer, 72);
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
    Result := VtocW(FBuffer, 107);
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
    Result := VtocW(FBuffer, 111);
end;

{ TVtocFmt1 }

function TVtocFmt1.GetDL_BL1(idx: Integer): UInt16;
begin
    Result := VtocHW(FBuffer, ((idx - 1) * 5) + 64);
end;

function TVtocFmt1.GetDL_CD1: UInt32;
begin
    Result := (FBuffer[53] shl 16) or
              (FBuffer[54] shl 8) or
               FBuffer[55];
end;

function TVtocFmt1.GetDL_CP1: UInt64;
begin
    Result := VtocAddr(FBuffer, 135);
end;

function TVtocFmt1.GetDL_DS1: Byte;
begin
    Result := FBuffer[99];
end;

function TVtocFmt1.GetDL_ED1: UInt32;
begin
    Result := (FBuffer[56] shl 16) or
              (FBuffer[57] shl 8) or
               FBuffer[58];
end;

function TVtocFmt1.GetDL_FS1: AnsiString;
begin
    Result := StringOf(FBuffer, 45, 50);
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
    Result := StringOf(FBuffer, 44, 44);
end;

function TVtocFmt1.GetDL_KEY1: AnsiString;
begin
    Result := StringOf(FBuffer, 0, 43);
end;

function TVtocFmt1.GetDL_KL1: UInt16;
begin
    Result := VtocHW(FBuffer, 100);
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
    Result := VtocHW(FBuffer, ((idx - 1) * 5) + 66);
end;

function TVtocFmt1.GetDL_SA1: Byte;
begin
    Result := FBuffer[102];
end;

function TVtocFmt1.GetDL_VS1: UInt16;
begin
    Result := VtocHW(FBuffer, 51);
end;

function TVtocFmt1.GetDL_XC1: Byte;
begin
    Result := FBuffer[59];
end;

function TVtocFmt1.GetDL_XL1(idx: Integer): UInt32;
begin
    Result := VtocW(FBuffer, ((idx - 1) * 10) + 107);
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
begin
    Result := VtocW(FBuffer, ((idx - 1) * 10) + 111);
end;

end.
