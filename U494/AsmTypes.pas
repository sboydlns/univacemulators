unit AsmTypes;

interface

uses SysUtils, Classes, Generics.Collections, SrcFile, U494Opcodes;

type
  TAllocationType = ( atNone = 0, atDirect, atRelative, atIndirect );

  TOutputType = ( otImage, otExecutable, otObject );

  TProcName = packed record
  public
    Name: AnsiString;
    Param: Integer;
    FirstSrcLine: Integer;
    IsEntry: Boolean;
  end;

  TProcSegmentType = ( stConstant, stNoIndex, st1Index, st2Index, st2IndexStar,
                       stGo );

  TProcSegment = packed record
  public
    SType: TProcSegmentType;
    Constant: AnsiString;
    Index1, Index2: Integer;
  end;

  TProcSource = packed record
  public
    Segments: array of TProcSegment;
  end;

  TProc = class(TObject)
  private
    FNames: array of TProcName;
    FSrcLines: array of TProcSource;
    function GetID: AnsiString;
    function GetSrcLines(idx: Integer): TProcSource;
    function GetSrcLinesMax: Integer;
    function GetSrcLinesMin: Integer;
    function GetSrcLinesLength: Integer;
  public
    procedure AddName(name: AnsiString; param, fstLine: Integer);
    procedure AddGoto(target: AnsiString);
    procedure AddSource(src: AnsiString);
    function FindName(name: AnsiString; var pname: TProcName): Boolean;
    function IsProcName(pname: TProcName): Boolean;
    property ID: AnsiString read GetID;
    property SrcLines[idx: Integer]: TProcSource read GetSrcLines;
    property SrcLinesLength: Integer read GetSrcLinesLength;
    property SrcLinesMax: Integer read GetSrcLinesMax;
    property SrcLinesMin: Integer read GetSrcLinesMin;
  end;

  TProcList = class(TList<TProc>)
  public
    destructor Destroy; override;
    function Find(name: AnsiString; var proc: TProc): Boolean;
  end;

  TOpcode = class(TObject)
  public
    Mnemonic: String;
    Opcode: Byte;
    InstType: T494InstructionType;
    OperandType: T494OperandType;
    Proc: Procedure (srcFile: TSrcFileStream; op: TOpcode) of object;
    SpurtProc: Procedure (lineNum: Integer; ops: AnsiString; op: TOpcode) of object;
  end;

  TOpcodeList = class(TDictionary<String, TOpcode>)
  public
    destructor Destroy; override;
  end;

  TSymbolType = ( stSystem, stIdentifier, stBDesignator, stJDesignator, stKDesignator, stForm );

  TSymbol = class(TObject)
  public
    ID: AnsiString;
    SourceID: AnsiString;                   // The identifier as codes in the source code.
                                            // This can be different from ID when coding in SPURT.
    Value: UInt64;
    SymbolType: TSymbolType;
    DefLine: Integer;
    DefCount: Integer;
    Relocatable: Boolean;
    Xref: TList<Integer>;
    ExternRef: TList<Integer>;
    IsEntry: Boolean;
    IsExternal: Boolean;
    EquUndefPass1: Boolean;
    AllocationType: TAllocationType;
    SubstituteValue: AnsiString;
    constructor Create;
    destructor Destroy; override;
  end;

  TSymbolList = class(TDictionary<AnsiString, TSymbol>)
  public
    destructor Destroy; override;
  end;

  TExpressionResult = packed record
    Value: Int64;
    Relocatable: Boolean;
    IsExternal: Boolean;
  end;

  TWordFormat = class(Tobject)
  public
    ID: AnsiString;
    Fields: TList<UInt64>;
    constructor Create;
    destructor Destroy; override;
  end;

  TWordFormatList = class(TDictionary<AnsiString, TWordFormat>)
  public
    destructor Destroy; override;
  end;

  TInstruction = packed record
  private
    FValue: Uint32;
    function GetY: UInt32;
    procedure SetY(const Value: UInt32);
    function GetB: Byte;
    function GetG: Byte;
    function GetJ: Byte;
    function GetJhat: Byte;
    function GetK: Byte;
    function GetKhat: Byte;
    procedure SetB(const Value: Byte);
    procedure SetG(const Value: Byte);
    procedure SetJ(const Value: Byte);
    procedure SetJhat(const Value: Byte);
    procedure SetK(const Value: Byte);
    procedure SetKhat(const Value: Byte);
    function GetF: Byte;
    procedure SetF(const Value: Byte);
  public
    procedure Clear;
    property b: Byte read GetB write SetB;
    property f: Byte read GetF write SetF;
    property g: Byte read GetG write SetG;
    property j: Byte read GetJ write SetJ;
    property jhat: Byte read GetJhat write SetJhat;
    property k: Byte read GetK write SetK;
    property khat: Byte read GetKhat write SetKhat;
    property Value: UInt32 read FValue write FValue;
    property y: UInt32 read GetY write SetY;
  end;

implementation

{ TProc }

procedure TProc.AddGoto(target: AnsiString);
var
    i, j: Integer;
begin
    SetLength(FSrcLines, Length(FSrcLines) + 1);
    i := High(FSrcLines);
    SetLength(FSrcLines[i].Segments, Length(FSrcLines[i].Segments) + 1);
    j := High(FSrcLines[i].Segments);
    FSrcLines[i].Segments[j].SType := stGo;
    FSrcLines[i].Segments[j].Constant := target;
end;

procedure TProc.AddName(name: AnsiString; param, fstLine: Integer);
var
    star: Integer;
begin
    SetLength(FNames, Length(FNames) + 1);
    star := Pos('*', String(name));
    if (star <> 0) then
    begin
        name := Copy(name, 1, star - 1);
        FNames[High(FNames)].IsEntry := True;
    end;
    FNames[High(FNames)].Name := name;
    FNames[High(FNames)].Param := param;
    FNames[High(FNames)].FirstSrcLine := fstLine;
end;

procedure TProc.AddSource(src: AnsiString);
// Parse the source line looking for paraforms and create a table of
// source and parameter substitition segments.
var
    pstart, pend, i, j: Integer;
    s, p: AnsiString;
    strings: TStringList;
    itemp1, itemp2: Integer;
    star: Boolean;

    procedure AddConstant(s: AnsiString);
    begin
        SetLength(FSrcLines[i].Segments, Length(FSrcLines[i].Segments) + 1);
        j := High(FSrcLines[i].Segments);
        FSrcLines[i].Segments[j].SType := stConstant;
        FSrcLines[i].Segments[j].Constant := s;
    end;

    procedure AddParam(stype: TProcSegmentType; i1, i2: Integer);
    begin
        SetLength(FSrcLines[i].Segments, Length(FSrcLines[i].Segments) + 1);
        j := High(FSrcLines[i].Segments);
        FSrcLines[i].Segments[j].SType := stype;
        FSrcLines[i].Segments[j].Index1 := i1;
        FSrcLines[i].Segments[j].Index2 := i2;
    end;

begin
    strings := TStringList.Create;
    strings.Delimiter := ',';
    strings.StrictDelimiter := True;
    try
        SetLength(FSrcLines, Length(FSrcLines) + 1);
        i := High(FSrcLines);
        repeat
            pstart := Pos(Format('%s(', [ID]), String(src));
            if (pstart = 0) then
            begin
                AddConstant(AnsiString(TrimRight(String(src))));
                src := '';
            end else
            begin
                s := Copy(src, 1, pstart - 1);
                AddConstant(s);
                src := Copy(src, pstart + 2);
                pend := Pos(')', String(src));
                if (pend <= 0) then
                    raise Exception.Create('Mismatched parentheses');
                p := AnsiString(Trim(Copy(String(src), 1, pend - 1)));
                src := Copy(src, pend + 1);
                if (p = '') then
                begin
                    AddParam(stNoIndex, 0, 0);
                end else
                begin
                    strings.DelimitedText := String(p);
                    if (strings.Count = 1) then
                    begin
                        if (not TryStrToInt(strings[0], itemp1)) then
                            raise Exception.Create('Invalid parameter index');
                        AddParam(st1Index, itemp1, 0);
                    end else if (strings.Count = 2) then
                    begin
                        if (not TryStrToInt(strings[0], itemp1)) then
                            raise Exception.Create('Invalid parameter index1');
                        if (strings[1][1] = '*') then
                        begin
                            star := True;
                            strings[1] := Copy(strings[1], 2);
                        end else
                        begin
                            star := False;
                        end;
                        if (not TryStrToInt(strings[1], itemp2)) then
                            raise Exception.Create('Invalid parameter index2');
                        if (star) then
                            AddParam(st2IndexStar, itemp1, itemp2)
                        else
                            AddParam(st2Index, itemp1, itemp2);
                    end else
                    begin
                        raise Exception.Create('Invalid number of indexes');
                    end;
                end;
            end;
        until src = '';
    finally
        strings.Free;
    end;
end;

function TProc.FindName(name: AnsiString; var pname: TProcName): Boolean;
var
    pn: TProcName;
begin
    for pn in FNames do
    begin
        if (name = pn.Name) then
        begin
            Result := True;
            pname := pn;
            Exit;
        end;
    end;
    Result := False;
end;

function TProc.GetID: AnsiString;
begin
    if (Length(FNames) > 0) then
        Result := FNames[0].Name
    else
        Result := '';
end;

function TProc.GetSrcLines(idx: Integer): TProcSource;
begin
    Result := FSrcLines[idx];
end;

function TProc.GetSrcLinesLength: Integer;
begin
    Result := Length(FSrcLines);
end;

function TProc.GetSrcLinesMax: Integer;
begin
    Result := High(FSrcLines);
end;

function TProc.GetSrcLinesMin: Integer;
begin
    Result := Low(FSrcLines);
end;

function TProc.IsProcName(pname: TProcName): Boolean;
begin
    Result := (pname.Name = ID);
end;

{ TProcList }

destructor TProcList.Destroy;
var
    p: TProc;
begin
    for p in Self do
        p.Free;
    inherited;
end;

function TProcList.Find(name: AnsiString; var proc: TProc): Boolean;
var
    p: TProc;
    pname: TProcName;
begin
    for p in Self do
    begin
        if (p.FindName(name, pname)) then
        begin
            if (pname.IsEntry) then
            begin
                Result := True;
                proc := p;
                Exit;
            end;
        end;
    end;
    Result := False;
end;

{ TOpcodeList }

destructor TOpcodeList.Destroy;
var
    pair: TPair<String, TOpcode>;
begin
    for pair in Self do
        pair.Value.Free;
    inherited;
end;

{ TSymbolList }

destructor TSymbolList.Destroy;
var
    pair: TPair<AnsiString, TSymbol>;
begin
    for pair in Self do
        pair.Value.Free;
    inherited;
end;

{ TSymbol }

constructor TSymbol.Create;
begin
    inherited;
    Xref := TList<Integer>.Create;
    ExternRef := TList<Integer>.Create;
end;

destructor TSymbol.Destroy;
begin
    FreeAndNil(Xref);
    FreeAndNil(ExternRef);
    inherited;
end;

{ TWordFormat }

constructor TWordFormat.Create;
begin
    inherited;
    Fields := TList<UInt64>.Create;
end;

destructor TWordFormat.Destroy;
begin
    FreeAndNil(Fields);
    inherited;
end;

{ FWordFormats }

destructor TWordFormatList.Destroy;
var
    pair: TPair<AnsiString, TWordFormat>;
begin
    for pair in Self do
        pair.Value.Free;
    inherited;
end;

{ TInstruction }

procedure TInstruction.Clear;
begin
    f := 0;
    g := 0;
    k := 0;
    j := 0;
    b := 0;
    y := 0;
end;

function TInstruction.GetB: Byte;
begin
    Result := (FValue shr 15) and $7;
end;

function TInstruction.GetF: Byte;
begin
    Result := (FValue shr 24) and $3f;
end;

function TInstruction.GetG: Byte;
begin
    Result := (FValue shr 18) and $3f;
end;

function TInstruction.GetJ: Byte;
begin
    Result := (FValue shr 21) and $7;
end;

function TInstruction.GetJhat: Byte;
begin
    Result := (FValue shr 20) and $f;
end;

function TInstruction.GetK: Byte;
begin
    Result := (FValue shr 18) and $7;
end;

function TInstruction.GetKhat: Byte;
begin
    Result := (FValue shr 18) and $3;
end;

function TInstruction.GetY: UInt32;
begin
    Result := FValue and $7fff;
end;

procedure TInstruction.SetB(const Value: Byte);
begin
    FValue := (FValue and (not ($7 shl 15))) or ((Value and $7) shl 15);
end;

procedure TInstruction.SetF(const Value: Byte);
begin
    FValue := (FValue and (not ($3f shl 24))) or ((Value and $3f) shl 24);
end;

procedure TInstruction.SetG(const Value: Byte);
begin
    FValue := (FValue and (not ($3f shl 18))) or ((Value and $3f) shl 18);
end;

procedure TInstruction.SetJ(const Value: Byte);
begin
    FValue := (FValue and (not ($7 shl 21))) or ((Value and $7) shl 21);
end;

procedure TInstruction.SetJhat(const Value: Byte);
begin
    FValue := (FValue and (not ($f shl 20))) or ((Value and $f) shl 20);
end;

procedure TInstruction.SetK(const Value: Byte);
begin
    FValue := (FValue and (not ($7 shl 18))) or ((Value and $7) shl 18);
end;

procedure TInstruction.SetKhat(const Value: Byte);
begin
    FValue := (FValue and (not ($3 shl 18))) or ((Value and $3) shl 18);
end;

procedure TInstruction.SetY(const Value: UInt32);
var
    itemp: UInt32;
begin
    if (Integer(Value) < 0) then
        itemp := (Value - 1) and $7fff
    else
        itemp := Value and $7fff;
    FValue := (FValue and (not $7fff)) or itemp;
end;

end.
