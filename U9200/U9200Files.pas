unit U9200Files;

interface

uses System.AnsiStrings, CardFile;

type
  TU92RLD = packed record
    ESID: Byte;
    Flag: Byte;
    Position: Byte;
  end;

  TU92ObjectFile = class(TCardFileStream)
  private
    function GetCardinal(start: Integer): Cardinal;
    function GetCardType: AnsiChar;
    function GetExtSymID: Byte;
    function GetHoleCount: Byte;
    function GetIsAbsolute: Boolean;
    function GetKey: Byte;
    function GetLength: Byte;
    function GetStartAddr: Cardinal;
    function GetSymName: AnsiString;
    function GetCodeLength: Cardinal;
    function GetRelDataLength: Byte;
    function GetLastRelData: Byte;
    function GetSymAddr: Cardinal;
    function GetRelData: Cardinal;
    function GetLoadAddr: Cardinal;
    function GetText(idx: Integer): Byte;
    function GetCardCount: Cardinal;
    function GetHex(start, len: Integer): AnsiString;
    function GetRLD(col: Integer): TU92RLD;
  public
    Buffer: TCardRec;
    function CalcHoleCount: Byte;
    function Read: Integer; reintroduce;
    property CardCount: Cardinal read GetCardCount;
    property CardType: AnsiChar read GetCardType;
    property CodeLength: Cardinal read GetCodeLength;
    property ExtSymID: Byte read GetExtSymID;
    property Hex[start, len: Integer]: AnsiString read GetHex;
    property HoleCount: Byte read GetHoleCount;
    property IsAbsolute: Boolean read GetIsAbsolute;
    property Key: Byte read GetKey;
    property LoadAddr: Cardinal read GetLoadAddr;
    property RelDataLength: Byte read GetRelDataLength;
    property LastRelData: Byte read GetLastRelData;
    property Length: Byte read GetLength;
    property RelData: Cardinal read GetRelData;
    property RLD[col: Integer]: TU92RLD read GetRLD;
    property StartAddr: Cardinal read GetStartAddr;
    property SymAddr: Cardinal read GetSymAddr;
    property SymName: AnsiString read GetSymName;
    property Text[idx: Integer]: Byte read GetText;
  end;

  TU92MacroFile = class(TCardFileStream)
  private
    function GetCardType: AnsiChar;
    function GetHoleCount: Byte;
    function GetDataLength: Byte;
    function GetKey: Byte;
    function GetHex(start, len: Integer): AnsiString;
    function GetEbcdic(start, len: Integer): AnsiString;
    function GetCodeStart: Integer;
    function GetMacroSize: Integer;
    function GetNameStart: Integer;
  public
    Buffer: TCardRec;
    function Read: Integer; reintroduce;
    property CardType: AnsiChar read GetCardType;
    property CodeStart: Integer read GetCodeStart;
    property DataLength: Byte read GetDataLength;
    property Ebcdic[start, len: Integer]: AnsiString read GetEbcdic;
    property Hex[start, len: Integer]: AnsiString read GetHex;
    property HoleCount: Byte read GetHoleCount;
    property Key: Byte read GetKey;
    property MacroSize: Integer read GetMacroSize;
    property NameStart: Integer read GetNameStart;
  end;

implementation

uses EmulatorTypes, U9200Types;

{ TU92ObjectFile }

function TU92ObjectFile.CalcHoleCount: Byte;
// Object deck hole count calculator as reverse engineered
// from original 9200 program loader.
var
    sum: WORD;
    w: WORD;
    i: Integer;
begin
    sum := 0;
    i := 71;
    while (i >= 7) do
    begin
        w := (Buffer.Columns[i] shl 8) or Buffer.Columns[i + 1];
        Inc(sum, w);
        Dec(i);
    end;
    Result := sum and $FF;
end;

function TU92ObjectFile.GetCardCount: Cardinal;
begin
    Result := GetCardinal(10) and $ffffff;
end;

function TU92ObjectFile.GetCardinal(start: Integer): Cardinal;
begin
    Result := (Buffer.Columns[start] shl 24) or
              (Buffer.Columns[start + 1] shl 16) or
              (Buffer.Columns[start + 2] shl 8) or
              (Buffer.Columns[start + 3]);
end;

function TU92ObjectFile.GetCardType: AnsiChar;
begin
    Result := TCodeTranslator.Hollerith8ToAscii(Buffer.Columns[2]);
end;

function TU92ObjectFile.GetCodeLength: Cardinal;
begin
    Result := GetCardinal(33);
end;

function TU92ObjectFile.GetExtSymID: Byte;
begin
    Result := Buffer.Columns[8];
end;

function TU92ObjectFile.GetHex(start, len: Integer): AnsiString;
begin
    Result := '';
    while (len >= 0) do
    begin
        Result := Result + Format('%2.2x', [Buffer.Columns[start]]);
        Inc(start);
        Dec(len);
    end;
end;

function TU92ObjectFile.GetHoleCount: Byte;
begin
    Result := Buffer.Columns[7];
end;

function TU92ObjectFile.GetIsAbsolute: Boolean;
begin
    Result := (Buffer.Columns[6] <> 0);
end;

function TU92ObjectFile.GetKey: Byte;
begin
    Result := Buffer.Columns[1];
end;

function TU92ObjectFile.GetLastRelData: Byte;
begin
    Result := Buffer.Columns[10];
end;

function TU92ObjectFile.GetLength: Byte;
begin
    Result := Buffer.Columns[3];
end;

function TU92ObjectFile.GetLoadAddr: Cardinal;
begin
    Result := GetCardinal(3) and $ffffff;
end;

function TU92ObjectFile.GetRelData: Cardinal;
begin
    Result := (GetCardinal(69) and $ffffff);
end;

function TU92ObjectFile.GetRelDataLength: Byte;
begin
    Result := Buffer.Columns[9];
end;

function TU92ObjectFile.GetRLD(col: Integer): TU92RLD;
// Return the RLD data for the given column. Set ESDI to $FF if none found.
var
    i: Integer;
begin
    Result.ESID := $ff;
    if (RelDataLength = 0) then
        Exit;
    i := 72 - RelDataLength + 1;
    while (i < 72) do
    begin
        Result.ESID := Buffer.Columns[i];
        Result.Flag := Buffer.Columns[i + 1];
        Result.Position := (Buffer.Columns[i + 2] and $7f);
        if (Result.Position = col) then
            Exit;
        Inc(i, 3);
    end;
    Result.ESID := $ff;
end;

function TU92ObjectFile.GetStartAddr: Cardinal;
begin
    Result := GetCardinal(13) and $ffffff;
end;

function TU92ObjectFile.GetSymAddr: Cardinal;
begin
    Result := (GetCardinal(13) and $ffffff);
end;

function TU92ObjectFile.GetSymName: AnsiString;
var
    i: Integer;
begin
    Result := '';
    for i := 17 to 24 do
        Result := Result + TCodeTranslator.EbcdicToAscii(Buffer.Columns[i]);
    Result := TrimRight(Result);
end;

function TU92ObjectFile.GetText(idx: Integer): Byte;
begin
    Result := Buffer.Columns[11 + idx];
end;

function TU92ObjectFile.Read: Integer;
begin
    Result := ReadTranslate(Buffer);
end;

{ TU9200MacroFile }

function TU92MacroFile.GetCardType: AnsiChar;
begin
    Result := TCodeTranslator.Hollerith8ToAscii(Buffer.Columns[3]);
end;

function TU92MacroFile.GetCodeStart: Integer;
begin
    Result := (Buffer.Columns[9] shl 8) or Buffer.Columns[10];
end;

function TU92MacroFile.GetDataLength: Byte;
begin
    Result := Buffer.Columns[4];
end;

function TU92MacroFile.GetEbcdic(start, len: Integer): AnsiString;
begin
    Result := '';
    while (len >= 0) do
    begin
        Result := Result + TCodeTranslator.EbcdicToAscii(Buffer.Columns[start]) + ' ';
        Inc(start);
        Dec(len);
    end;
end;

function TU92MacroFile.GetHex(start, len: Integer): AnsiString;
begin
    Result := '';
    while (len >= 0) do
    begin
        Result := Result + Format('%2.2x', [Buffer.Columns[start]]);
        Inc(start);
        Dec(len);
    end;
end;

function TU92MacroFile.GetHoleCount: Byte;
begin
    Result := Buffer.Columns[2];
end;

function TU92MacroFile.GetKey: Byte;
begin
    Result := Buffer.Columns[1];
end;

function TU92MacroFile.GetMacroSize: Integer;
begin
    Result := (Buffer.Columns[5] shl 8) or Buffer.Columns[6];
end;

function TU92MacroFile.GetNameStart: Integer;
begin
    Result := (Buffer.Columns[7] shl 8) or Buffer.Columns[8];
end;

function TU92MacroFile.Read: Integer;
begin
    Result := ReadTranslate(Buffer);
end;

end.
