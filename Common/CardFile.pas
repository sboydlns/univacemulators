unit CardFile;

interface

uses SysUtils, Classes, Generics.Collections;

type
  TCardRec = record
  private
    function GetColumnAsWord(col: Integer): WORD;
    procedure SetColumnAsWord(col: Integer; const Value: WORD);
  public
    Count: Integer;
    Columns: array [1..160] of Byte;
    procedure Assign(src: TCardRec);
    procedure Clear;
    property ColumnAsWord[col: Integer]: WORD read GetColumnAsWord write SetColumnAsWord;
  end;

  TCardFileType = ( cftUnknown, cftHollerith16, cftHollerith12, cftAscii, cftXS3,
                    cftFieldata );

  // This class read card files where each column of the card
  // occupies 2 bytes (1 half word). The most significant bit
  // in the half word contains the 12 punch, the next bit contains
  // the 11 punch and so on. The least significant 4 bits of
  // the half word are not used. The half word is stored in the
  // file in little-endian format (LSB first).
  TCardFileStream = class(TFileStream)
  private
    FType: TCardFileType;
    FRecNumber: Integer;
    function ReadLine: AnsiString;
  public
    constructor Create(const AFileName: string; Mode: Word); reintroduce;
    function Eof: Boolean; virtual;
    function Merge(const bfr1, bfr2: TCardRec): Integer; virtual;
    class function ReadFieldata(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function ReadFieldata(var bfr: TCardRec): Integer; overload; virtual;
    function ReadImage(var bfr: TCardRec): Integer; overload; virtual;
    class function ReadImage(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function ReadTranslate(var bfr: TCardRec): Integer; overload; virtual;
    class function ReadTranslate(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function ReadRaw(var bfr: TCardRec): Integer; virtual;
    function ReadXS3(var bfr: TCardRec): Integer; overload; virtual;
    class function ReadXS3(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function RecordCount: Integer; virtual;
    procedure SaveToFile(fname: String);
    function WriteFieldata(const bfr: TCardRec): Integer; overload; virtual;
    class function WriteFieldata(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function WriteImage(const bfr: TCardRec): Integer; overload; virtual;
    class function WriteImage(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function WriteRaw(const bfr: TCardRec): Integer; virtual;
    function WriteTranslate(const bfr: TCardRec): Integer; overload; virtual;
    class function WriteTranslate(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    function WriteXS3(const bfr: TCardRec): Integer; overload; virtual;
    class function WriteXS3(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer; overload;
    property RecNumber: Integer read FRecNumber;
  end;

  TBlankCardStream = class(TCardFileStream)
  private
    FCardCount: Integer;
  public
    constructor Create(count: Integer); reintroduce;
    function Eof: Boolean; override;
    function Merge(const bfr1, bfr2: TCardRec): Integer; override;
    function ReadImage(var bfr: TCardRec): Integer; override;
    function ReadTranslate(var bfr: TCardRec): Integer; override;
    function ReadRaw(var bfr: TCardRec): Integer; override;
    function RecordCount: Integer; override;
    function WriteImage(const bfr: TCardRec): Integer; override;
    function WriteTranslate(const bfr: TCardRec): Integer; override;
  end;

  TRPGCardStream = class(TCardFileStream)
  private
    FRPGType : String;
    function TypeMatches(const cr: TCardRec): Boolean;
  public
    constructor Create(const AFileName: string; Mode: Word; rpgType: String); reintroduce;
    function Eof: Boolean; override;
    function ReadImage(var bfr: TCardRec): Integer; override;
    function ReadTranslate(var bfr: TCardRec): Integer; override;
    function ReadRaw(var bfr: TCardRec): Integer; override;
    function RecordCount: Integer; override;
  end;

  TCCLDevice = ( cdUnknown, cdReader, cdPunch );

  TCCLType = ( ctUnknown, ctRootDir, ctData, ctBlanks );

  TCCLRec = record
  public
    FileType: TCCLType;
    Device: TCCLDevice;
    Name: String;
    RPGType: String;
    procedure Clear;
  end;

  TCCLProc = procedure(cmd: String; var rec: TCCLRec) of object;

  TCCLJumpRec = record
  public
    Command: String;
    Handler: TCCLProc;
  end;

  TCCLJumpList = class(TList<TCCLJumpRec>)
  end;

  TCCLStream = class(TFileStream)
  private
    FJumpTable: TCCLJumpList;
    function DeviceType(s: String): TCCLDevice;
    procedure DoBlanks(cmd: String; var rec: TCCLRec);
    procedure DoFile(cmd: String; var rec: TCCLRec);
    procedure DoRoot(cmd: String; var rec: TCCLRec);
  public
    constructor Create(const AFileName: string; Mode: Word); reintroduce;
    destructor Destroy; override;
    function Read(var rec: TCCLRec): Boolean; reintroduce;
    function ReadLine(var line: String): Boolean;
  end;

  TCardFileRec = record
  public
    FileName: String;
    RPGType: String;
    BlankCards: Integer;
  end;

  TCardFileList = class(TList<TCardFileRec>)
  end;

implementation

uses Math, EmulatorTypes;

{ TCardFileStream }

constructor TCardFileStream.Create(const AFileName: string; Mode: Word);
var
    extn: String;
begin
    inherited Create(AFileName, Mode);
    extn := LowerCase(ExtractFileExt(AFileName));
    if ((extn = '.h16') or (extn = '')) then
        FType := cftHollerith16
    else if (extn = '.h80') then
        FType := cftHollerith12
    else if ((extn = '.asc') or (extn = '.asm') or (extn = '.rpg') or
             (extn = '.jcl')) then
        FType := cftAscii
    else if ((extn = '.xs3') or (extn = '.asm3')) then
        FType := cftXS3
    else if (extn = '.fd') then
        FType := cftFieldata
    else
        FType := cftUnknown;
end;

function TCardFileStream.Eof: Boolean;
begin
    Result := (Position >= Size);
end;

function TCardFileStream.Merge(const bfr1, bfr2: TCardRec): Integer;
// Merge the two raw buffers by ORing them together.
// This simulates updating a card by overpunching it.
var
    w1, w2: Word;
    col: Integer;
    len: Integer;
begin
    len := Max(bfr1.Count, bfr2.Count);
    col := 1;
    while (col <= 160) do
    begin
        if (col <= len) then
        begin
            w1 := bfr1.Columns[col] or (bfr1.Columns[col + 1] shl 8);
            w2 := bfr2.Columns[col] or (bfr2.Columns[col + 1] shl 8);
            w1 := w1 or w2;
        end else
            w1 := 0;
        Write(w1, 2);
        Inc(col, 2);
    end;
    Result := len;
end;

function TCardFileStream.ReadImage(var bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    bfr.Clear;
    ReadRaw(rawBfr);
    Result := ReadImage(bfr, rawBfr);
end;

function TCardFileStream.ReadFieldata(var bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    bfr.Clear;
    ReadRaw(rawBfr);
    Result := ReadFieldata(bfr, rawBfr);
end;

class function TCardFileStream.ReadFieldata(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    i: Integer;
begin
    bfrOut.Clear;
    for i := 1 to bfrIn.Count do
        bfrOut.Columns[i] := TCodeTranslator.Hollerith12ToFieldata(bfrIn.ColumnAsWord[i]);
    bfrOut.Count := bfrIn.Count div 2;
    Result := bfrOut.Count;
end;

class function TCardFileStream.ReadImage(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    i: Integer;
    w: WORD;
begin
    bfrOut.Clear;
    i := 1;
    while (i <= bfrIn.Count) do
    begin
        w := bfrIn.Columns[i] or (bfrIn.Columns[i + 1] shl 8);
        bfrOut.Columns[i] := (w and $FC00) shr 10;
        bfrOut.Columns[i + 1] := (w and $03F0) shr 4;
        Inc(bfrOut.Count, 2);
        Inc(i, 2);
    end;
    Result := bfrOut.Count;
end;

function TCardFileStream.ReadLine: AnsiString;
var
    c: AnsiChar;
    done: Boolean;
begin
    Result := '';
    done := False;
    while (not done) do
    begin
        if (Read(c, 1) > 0) then
        begin
            if ((c <> #13) and (c <> #10)) then
                Result := Result + c
            else if (c = #10) then
                done := True;
        end else
            done := True;
    end;
end;

function TCardFileStream.ReadRaw(var bfr: TCardRec): Integer;
// Read the card file, converting it to a 16-bit hollerith
// representation where each card column consist of 2 bytes.
// The first byte contains punches from rows 2 through 9. The
// second byte contains punch from rows 12 through 1 in the
// most significant 4 bits.
var
    b: Byte;
    b3: array [0..2] of Byte;
    col: Integer;
    w: WORD;
    line: AnsiString;
    c: AnsiChar;
begin
    bfr.Clear;
    col := 1;
    while (col <= 160) do
    begin
        case FType of
          cftHollerith16:
          begin
            if (Read(b, 1) > 0) then
            begin
                bfr.Columns[col] := b;
                Inc(bfr.Count);
                Inc(col);
            end else
                Break;
          end;
          cftHollerith12:
          begin
            if (Read((@b3[0])^, 3) = 3) then
            begin
                bfr.Columns[col] := b3[1] and $f0;
                bfr.Columns[col + 1] := b3[0];
                bfr.Columns[col + 2] := (b3[2] and $0f) shl 4;
                bfr.Columns[col + 3] := ((b3[1] and $0f) shl 4) or ((b3[2] and $f0) shr 4);
                Inc(bfr.Count, 4);
                Inc(col, 4);
            end;
          end;
          cftAscii:
          begin
            line := ReadLine;
            for c in line do
            begin
                w := TCodeTranslator.Hollerith8ToHollerith12(TCodeTranslator.AsciiToHollerith8(c));
                bfr.Columns[col + 1] := (w and $ff00) shr 8;
                bfr.Columns[col] := w and $ff;
                Inc(bfr.Count, 2);
                Inc(col, 2);
                if (col > 160) then
                    Break;
            end;
            bfr.Count := 160;
            col := 161;
          end;
          cftXS3:
          begin
            line := ReadLine;
            for c in line do
            begin
                w := TCodeTranslator.XS3ToHollerith12(TCodeTranslator.AsciiToXS3(c));
                bfr.Columns[col + 1] := (w and $ff00) shr 8;
                bfr.Columns[col] := w and $ff;
                Inc(bfr.Count, 2);
                Inc(col, 2);
                if (col > 160) then
                    Break;
            end;
            bfr.Count := 160;
            col := 161;
          end;
          cftFieldata:
          begin
            line := ReadLine;
            for c in line do
            begin
                w := TCodeTranslator.FieldataToHollerith12(TCodeTranslator.AsciiToFieldata(c));
                bfr.Columns[col + 1] := (w and $ff00) shr 8;
                bfr.Columns[col] := w and $ff;
                Inc(bfr.Count, 2);
                Inc(col, 2);
                if (col > 160) then
                    Break;
            end;
            bfr.Count := 160;
            col := 161;
          end;
        end;
    end;
    Inc(FRecNumber);
    Result := bfr.Count;
end;

function TCardFileStream.ReadTranslate(var bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    bfr.Clear;
    ReadRaw(rawBfr);
    Result := ReadTranslate(bfr, rawBfr);
end;

class function TCardFileStream.ReadTranslate(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    i: Integer;
    j: Integer;
    w: WORD;
begin
    bfrOut.Clear;
    i := 1;
    j := 1;
    while (i <= bfrIn.Count) do
    begin
        w := bfrIn.Columns[i] or (bfrIn.Columns[i + 1] shl 8);
        bfrOut.Columns[j] := TCodeTranslator.Hollerith12ToHollerith8(w);
        Inc(bfrOut.Count);
        Inc(i, 2);
        Inc(j);
    end;
    Result := bfrOut.Count;
end;

function TCardFileStream.ReadXS3(var bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    bfr.Clear;
    ReadRaw(rawBfr);
    Result := ReadXS3(bfr, rawBfr);
end;

class function TCardFileStream.ReadXS3(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    i: Integer;
begin
    bfrOut.Clear;
    for i := 1 to bfrIn.Count do
        bfrOut.Columns[i] := TCodeTranslator.Hollerith12ToXS3(bfrIn.ColumnAsWord[i]);
    bfrOut.Count := bfrIn.Count div 2;
    Result := bfrOut.Count;
end;

function TCardFileStream.RecordCount: Integer;
var
    holdPos: Cardinal;
    cardRec: TCardRec;
begin
    case FType of
      cftHollerith16:
      begin
        Result := Size div 160;
        if ((Size mod 160) > 0) then
            Inc(Result);
      end;
      cftHollerith12:
      begin
        Result := Size div 120;
        if ((Size mod 120) > 0) then
            Inc(Result);
      end;
      cftAscii,
      cftXS3,
      cftFieldata:
      begin
        Result := 0;
        holdPos := Position;
        try
            Position := 0;
            while (not Eof) do
            begin
                ReadRaw(cardRec);
                Inc(Result);
            end;
        finally
            Position := HoldPos;
        end;
      end;
      else
        Result := 0;
    end;
end;

procedure TCardFileStream.SaveToFile(fname: String);
var
    fout: TCardFileStream;
    holdPos: Int64;
    bfr: TCardRec;
begin
    fout := TCardFileStream.Create(fname, fmCreate);
    try
        holdPos := Position;
        Position := 0;
        while (not Eof) do
        begin
            ReadRaw(bfr);
            fout.WriteRaw(bfr);
        end;
        Position := holdPos;
    finally
        fout.Free;
    end;
end;

function TCardFileStream.WriteImage(const bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    WriteImage(rawBfr, bfr);
    Result := WriteImage(rawbfr);
end;

function TCardFileStream.WriteFieldata(const bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    WriteXS3(rawBfr, bfr);
    Result := WriteRaw(rawBfr);
end;

class function TCardFileStream.WriteFieldata(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    col: Integer;
begin
    bfrOut.Clear;
    for col := 1 to bfrIn.Count do
        bfrOut.ColumnAsWord[col] := TCodeTranslator.FieldataToHollerith12(AnsiChar(Chr(bfrIn.Columns[col])));
    bfrOut.Count := bfrIn.Count * 2;
    Result := bfrOut.Count;
end;

class function TCardFileStream.WriteImage(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    w: WORD;
    colIn: Integer;
    colOut: Integer;
begin
    bfrOut.Clear;
    colOut := 1;
    colIn := 1;
    while (colIn <= 160) do
    begin
        if (colIn <= bfrIn.Count) then
            w := ((bfrIn.Columns[colIn] and $3f) shl 10) or ((bfrIn.Columns[colIn + 1] and $3f) shl 4)
        else
            w := 0;
        bfrOut.Columns[colOut] := w and $ff;
        bfrOut.Columns[colOut + 1] := (w and $ff00) shr 8;
        Inc(colIn, 2);
        Inc(colOut, 2);
        Inc(bfrOut.Count, 2);
    end;
    Result := bfrOut.Count;
end;

function TCardFileStream.WriteRaw(const bfr: TCardRec): Integer;
var
    col: Integer;
    w: WORD;
    stemp: AnsiString;
    b3: array [0..2] of Byte;

    function NextColumn: WORD;
    begin
        if (col <= bfr.Count) then
            Result := bfr.Columns[col] or (bfr.Columns[col + 1] shl 8)
        else
            Result := 0;
        Inc(col, 2);
    end;

begin
    stemp := '';
    col := 1;
    while (col <= 160) do
    begin
        case FType of
          cftHollerith16:
          begin
            w := NextColumn;
            Write(w, 2);
          end;
          cftHollerith12:
          begin
            FillChar(b3, 3, 0);
            w := NextColumn;
            b3[0] := (w and $ff00) shr 8;
            b3[1] := w and $f0;
            w := NextColumn;
            b3[1] := b3[1] or ((w and $f000) shr 12);
            b3[2] := (w and $ff0) shr 4;
            Write((@b3[0])^, 3);
          end;
          cftAscii:
          begin
            w := NextColumn;
            stemp := stemp + TCodeTranslator.Hollerith8ToAscii(TCodeTranslator.Hollerith12ToHollerith8(w));
          end;
          cftXS3:
          begin
            w := NextColumn;
            stemp := stemp + TCodeTranslator.XS3ToAscii(TCodeTranslator.Hollerith12ToXS3(w));
          end;
          cftFieldata:
          begin
            w := NextColumn;
            stemp := stemp + TCodeTranslator.FieldataToAscii(TCodeTranslator.Hollerith12ToFieldata(w));
          end;
          else
            raise Exception.Create('Unknown file type');
        end;
    end;
    if ((FType = cftAscii) or (FType = cftXS3) or (FType = cftFieldata)) then
    begin
        stemp := AnsiString(Format('%s'#13#10, [TrimRight(String(stemp))]));
        Write(PAnsiChar(stemp)^, Length(stemp));
    end;
    Result := bfr.Count;
end;

class function TCardFileStream.WriteTranslate(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    w: WORD;
    colIn: Integer;
    colOut: Integer;
begin
    bfrOut.Clear;
    colOut := 1;
    for colIn := 1 to 80 do
    begin
        if (colIn <= bfrIn.Count) then
            w := TCodeTranslator.Hollerith8ToHollerith12(bfrIn.Columns[colIn])
        else
            w := 0;
        bfrOut.Columns[colOut + 1] := (w and $FF00) shr 8;
        bfrOut.Columns[colOut] := w and $FF;
        Inc(colOut, 2);
        Inc(bfrOut.Count, 2);
    end;
    Result := bfrOut.Count;
end;

function TCardFileStream.WriteXS3(const bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    WriteXS3(rawBfr, bfr);
    Result := WriteRaw(rawBfr);
end;

class function TCardFileStream.WriteXS3(var bfrOut: TCardRec; const bfrIn: TCardRec): Integer;
var
    col: Integer;
begin
    bfrOut.Clear;
    for col := 1 to bfrIn.Count do
        bfrOut.ColumnAsWord[col] := TCodeTranslator.XS3ToHollerith12(bfrIn.Columns[col]);
    bfrOut.Count := bfrIn.Count * 2;
    Result := bfrOut.Count;
end;

function TCardFileStream.WriteTranslate(const bfr: TCardRec): Integer;
var
    rawBfr: TCardRec;
begin
    WriteTranslate(rawBfr, bfr);
    Result := WriteRaw(rawBfr);
end;

{ TCardRec }

procedure TCardRec.Assign(src: TCardRec);
var
    i: Integer;
begin
    Count := src.Count;
    for i := Low(Columns) to High(Columns) do
        Columns[i] := src.Columns[i];
end;

procedure TCardRec.Clear;
var
    i: Integer;
begin
    Count := 0;
    for i := Low(Columns) to High(Columns) do
        Columns[i] := 0;
end;

function TCardRec.GetColumnAsWord(col: Integer): WORD;
var
    offset: Integer;
begin
    offset := ((col - 1) * 2) + 1;
    Result := Columns[offset] + (Columns[offset + 1] shl 8);
end;

procedure TCardRec.SetColumnAsWord(col: Integer; const Value: WORD);
var
    offset: Integer;
begin
    offset := ((col - 1) * 2) + 1;
    Columns[offset] := Value and $ff;
    Columns[offset + 1] := (Value and $ff00) shr 8;
end;

{ TBlankCardStream }

constructor TBlankCardStream.Create(count: Integer);
begin
    inherited Create('nul:', fmOpenRead);
    FCardCount := count;
end;

function TBlankCardStream.Eof: Boolean;
begin
    Result := (FCardCount = 0);
end;

function TBlankCardStream.Merge(const bfr1, bfr2: TCardRec): Integer;
begin
    Result := 0;
end;

function TBlankCardStream.ReadImage(var bfr: TCardRec): Integer;
begin
    Result := 0;
    bfr.Clear;
    if (FCardCount > 0) then
    begin
        Dec(FCardCount);
        Result := 160;
    end;
end;

function TBlankCardStream.ReadRaw(var bfr: TCardRec): Integer;
begin
    bfr.Clear;
    if (FCardCount > 0) then
    begin
        Dec(FCardCount);
        bfr.Count := 160;
    end;
    Result := bfr.Count;
end;

function TBlankCardStream.ReadTranslate(var bfr: TCardRec): Integer;
begin
    Result := 0;
    bfr.Clear;
    if (FCardCount > 0) then
    begin
        Dec(FCardCount);
        Result := 80;
    end;
end;

function TBlankCardStream.RecordCount: Integer;
begin
    Result := FCardCount;
end;

function TBlankCardStream.WriteImage(const bfr: TCardRec): Integer;
begin
    Result := 0;
end;

function TBlankCardStream.WriteTranslate(const bfr: TCardRec): Integer;
begin
    Result := 0;
end;

{ TCCLStream }

constructor TCCLStream.Create(const AFileName: string; Mode: Word);
var
    jump: TCCLJumpRec;
begin
    inherited Create(AFileName, Mode);
    FJumpTable := TCCLJumpList.Create;
    jump.Command := 'FILE';
    jump.Handler := DoFile;
    FJumpTable.Add(jump);
    jump.Command := 'BLANKS';
    jump.Handler := DoBlanks;
    FJumpTable.Add(jump);
    jump.Command := 'ROOT';
    jump.Handler := DoRoot;
    FJumpTable.Add(jump);
end;

destructor TCCLStream.Destroy;
begin
    FreeAndNil(FJumpTable);
    inherited Destroy;
end;

function TCCLStream.DeviceType(s: String): TCCLDevice;
begin
    if (s = 'RDR') then
        Result := cdReader
    else if (s = 'PUN') then
        Result := cdPunch
    else
        Result := cdUnknown;
end;

procedure TCCLStream.DoBlanks(cmd: String; var rec: TCCLRec);
// Parse a /BLANK command
//
// /BLANK [RDR | PUN],number_of_blanks
var
    dev, blanks: String;
    split: Integer;
begin
    rec.FileType := ctBlanks;
    split := Pos(',', cmd);
    if (split > 0) then
    begin
        dev := UpperCase(Trim(Copy(cmd, 1, split - 1)));
        blanks := Trim(Copy(cmd, split + 1));
    end else
    begin
        dev := UpperCase(Trim(cmd));
        blanks := '';
    end;
    rec.Device := DeviceType(dev);
    rec.Name := blanks;
end;

procedure TCCLStream.DoFile(cmd: String; var rec: TCCLRec);
// Parse a /FILE command
//
// /FILE [RDR | PUN],file_name,RPG_card_type (H/F/I/O/C/E)
var
    dev, fname, rpgType: String;
    split: Integer;
begin
    rec.FileType := ctData;
    dev := '';
    fname := '';
    rpgType := '';
    split := Pos(',', cmd);
    if (split > 0) then
    begin
        dev := UpperCase(Trim(Copy(cmd, 1, split - 1)));
        fname := Trim(Copy(cmd, split + 1));
        split := Pos(',', fname);
        if (split > 0) then
        begin
            rpgType := UpperCase(Trim(Copy(fname, split + 1)));
            fname := Trim(copy(fname, 1, split - 1));
        end;
    end else
        dev := UpperCase(Trim(cmd));
    rec.Device := DeviceType(dev);
    rec.Name := fname;
    rec.RPGType := rpgType;
end;

procedure TCCLStream.DoRoot(cmd: String; var rec: TCCLRec);
// Parse a /ROOT command
//
// /ROOT root_directory
begin
    rec.FileType := ctRootDir;
    rec.Name := cmd;
end;

function TCCLStream.Read(var rec: TCCLRec): Boolean;
var
    line: String;
    char1: String;
    cmd: String;
    params: String;
    split: Integer;
    jr: TCCLJumpRec;
begin
    rec.Clear;
    if (not ReadLine(line)) then
    begin
        Result := False;
        Exit;
    end;
    Result := True;
    // If first char = '/', we have a potential command
    char1 := Copy(line, 1, 1);
    if (char1 = '/') then
    begin
        // Split the command from the params by looking for
        // the first space.
        split := Pos(' ', line);
        if (split > 0) then
        begin
            cmd := UpperCase(Copy(line, 2, split - 2));
            params := Trim(Copy(line, split + 1));
        end else
        begin
            cmd := UpperCase(Copy(line, 2));
            params := '';
        end;
        // Execute the appropriate handler
        for jr in FJumpTable do
        begin
            if (jr.Command = cmd) then
            begin
                jr.Handler(params, rec);
                Exit;
            end;
        end;
    end;
end;

function TCCLStream.ReadLine(var line: String): Boolean;
var
    c: AnsiChar;
    done: Boolean;
begin
    Result := False;
    line := '';
    done := False;
    while (not done) do
    begin
        if (inherited Read(c, 1) > 0) then
        begin
            Result := True;
            if (c = #10) then
                done := True
            else if (c <> #13) then
                line := line + Char(c);
        end else
            done := True;
    end;
end;

{ TCCLRec }

procedure TCCLRec.Clear;
begin
    FileType := ctUnknown;
    Device := cdUnknown;
    Name := '';
end;

{ TRPGCardStrea }

constructor TRPGCardStream.Create(const AFileName: string; Mode: Word; rpgType: String);
begin
    inherited Create(AFileName, Mode);
    FRPGType := rpgType;
end;

function TRPGCardStream.Eof: Boolean;
var
    holdPos: Cardinal;
    cr: TCardRec;
begin
    while (not inherited Eof) do
    begin
        holdPos := Position;
        ReadTranslate(cr);
        if (TypeMatches(cr)) then
        begin
            Result := False;
            Position := holdPos;
            Exit;
        end;
    end;
    Result := True;
end;

function TRPGCardStream.ReadImage(var bfr: TCardRec): Integer;
var
    cr: TCardRec;
begin
    ReadRaw(bfr);
    Result := ReadImage(cr, bfr);
    bfr := cr;
end;

function TRPGCardStream.ReadRaw(var bfr: TCardRec): Integer;
var
    cr: TCardRec;
begin
    Result := inherited ReadRaw(bfr);
    ReadTranslate(cr, bfr);
    while ((not inherited Eof) and (not TypeMatches(cr))) do
    begin
        Result := inherited ReadRaw(bfr);
        ReadTranslate(cr, bfr);
    end;
end;

function TRPGCardStream.ReadTranslate(var bfr: TCardRec): Integer;
begin
    Result := inherited ReadTranslate(bfr);
    while ((not inherited Eof) and (not TypeMatches(bfr))) do
        Result := inherited ReadTranslate(bfr);
end;

function TRPGCardStream.RecordCount: Integer;
var
    holdPos: Cardinal;
    cr: TCardRec;
begin
    Result := 0;
    holdPos := Position;
    try
        Position := 0;
        ReadTranslate(cr);
        while (not Eof) do
        begin
            if (TypeMatches(cr))  then
                Inc(Result);
            ReadTranslate(cr);
        end;
    finally
        Position := holdPos;
    end;
end;

function TRPGCardStream.TypeMatches(const cr: TCardRec): Boolean;
begin
    Result := (FRPGType = '') or
              (Pos(String(Char(TCodeTranslator.Hollerith8ToAscii(cr.Columns[6]))), FRPGType) > 0);
end;

end.
