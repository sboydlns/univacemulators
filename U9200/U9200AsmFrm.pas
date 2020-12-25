unit U9200AsmFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections, U9200Types;

type
  TSymbol = class(TObject)
  public
    Name: String;
    Addr: Smallint;
    Length: Smallint;
    NumDefines: Integer;
    IsEntry: Boolean;
    IsRelocatable: Boolean;
  end;

  TSymbolTable = class(TDictionary<String, TSymbol>)
  public
    procedure Clear; reintroduce;
  end;

  TDirectiveProc = procedure(operand: String;
                             sym: TSymbol;
                             var pcBump: Smallint;
                             var code: AnsiString) of Object;

  TOpcode = class(TObject)
  public
    Name: String;
    Opcode: Byte;
    InstType: TInstType;
    Proc: TDirectiveProc;
    function Length: Integer;
  end;

  TOpcodeTable = class(TDictionary<String, TOpcode>)
  public
    procedure Clear; reintroduce;
  end;

  TU9200AsmForm = class(TForm)
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    ConvertBtn: TButton;
    CancelBtn: TButton;
    OpenDlg: TOpenDialog;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
  private
    FOpcodes: TOpcodeTable;
    FSymbols: TSymbolTable;
    FPC: Smallint;
    FStmtCount: Integer;
    FXferAddr: Smallint;
    FProgram: array[0..32767] of Byte;
    procedure Compile(fname: String);
    procedure Expression(s: String; var rslt, len: Smallint; var reloc: Boolean);
    procedure GetAddress(var operand: String; var reg, len, offset: Integer);
    procedure Initialize;
    function IsCharacter(s: String; var rslt: AnsiString; var len: Smallint): Boolean;
    function IsHex(s: String; var rslt: AnsiString; var len: Smallint): Boolean;
    function IsNumber(s: String; var num: Integer): Boolean;
    function LabelValid(lbl: String): Boolean;
    procedure Pass1(fname: String);
    procedure Pass2(fname: String);
    function RoundToWord(value: Integer): Integer;
    function ToBinary(s: String): AnsiString;
    procedure TokenizeLine(src: String; var lbl, opcode, operands: String);
    procedure DC(operand: String;
                 sym: TSymbol;
                 var pcBump: Smallint;
                 var code: AnsiString);
    procedure DS(operand: String;
                 sym: TSymbol;
                 var pcBump: Smallint;
                 var code: AnsiString);
    procedure END_(operand: String;
                   sym: TSymbol;
                   var pcBump: Smallint;
                   var code: AnsiString);
    procedure BRANCH(op: TOpcode; operand: String; var code: AnsiString);
    procedure RX(op: TOpcode; operand: String; var code: AnsiString);
    procedure SI(op: TOpcode; operand: String; var code: AnsiString);
    procedure SS1(op: TOpcode; operand: String; var code: AnsiString);
    procedure SS2(op: TOpcode; operand: String; var code: AnsiString);
    procedure START(operand: String;
                    sym: TSymbol;
                    var pcBump: Smallint;
                    var code: AnsiString);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U9200AsmForm: TU9200AsmForm;

implementation

uses Math, EmulatorTypes;

{$R *.dfm}

procedure TU9200AsmForm.BRANCH(op: TOpcode; operand: String; var code: AnsiString);
var
    baseReg, len, offset: Integer;
    flags: Byte;
begin
    if (op.Name = 'BEQ') then
        flags := 8
    else if (op.Name = 'BNE') then
        flags := 6
    else if (op.Name = 'BGT') then
        flags := 2
    else if (op.Name = 'BLT') then
        flags := 4
    else if (op.Name = 'BGE') then
        flags := 10
    else if (op.Name = 'BLE') then
        flags := 12
    else if (op.Name = 'BP') then
        flags := 2
    else if (op.Name = 'BM') then
        flags := 4
    else if (op.Name = 'BOV') then
        flags := 1
    else if (op.Name = 'BZ') then
        flags := 8
    else if (op.Name = 'BNZ') then
        flags := 6
    else if (op.Name = 'B') then
        flags := 15
    else
        flags := 0;

    GetAddress(operand, baseReg, len, offset);
    code := ToBinary(Format('%2.2x%2.2x%1.1x%3.3x', [op.Opcode, flags shl 4, baseReg, offset]));
end;

procedure TU9200AsmForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9200AsmForm.CancelBtnClick(Sender: TObject);
begin
    Close
end;

procedure TU9200AsmForm.Compile(fname: String);
begin
    Initialize;
    Pass1(fname);
    Pass2(fname);
end;

procedure TU9200AsmForm.ConvertBtnClick(Sender: TObject);
begin
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the source file name');
    Compile(FileNameEdt.Text);
end;

constructor TU9200AsmForm.Create(AOwner: TComponent);

    function Opcode(name: String; op: Byte; typ: TInstType; proc: TDirectiveProc): TOpcode;
    begin
        Result := TOpcode.Create;
        Result.Name := UpperCase(name);
        Result.Opcode := op;
        Result.InstType := typ;
        Result.Proc := proc;
    end;

begin
    inherited;
    FOpcodes := TOpcodeTable.Create;
    FSymbols := TSymbolTable.Create;
    FOpcodes.Add('STH', Opcode('STH', $40, itRX, nil));
    FOpcodes.Add('LH', Opcode('LH', $48, itRX, nil));
    FOpcodes.Add('CH', Opcode('CH', $49, itRX, nil));
    FOpcodes.Add('AI', Opcode('AI', $A6, itSI, nil));
    FOpcodes.Add('AH', Opcode('AH', $AA, itRX, nil));
    FOpcodes.Add('SH', Opcode('SH', $AB, itRX, nil));
    FOpcodes.Add('TM', Opcode('TM', $91, itSI, nil));
    FOpcodes.Add('MVI', Opcode('MVI', $92, itSI, nil));
    FOpcodes.Add('NI', Opcode('NI', $94, itSI, nil));
    FOpcodes.Add('CLI', Opcode('CLI', $95, itSI, nil));
    FOpcodes.Add('OI', Opcode('OI', $96, itSI, nil));
    FOpcodes.Add('HPR', Opcode('HPR', $A9, itSI, nil));
    FOpcodes.Add('MVN', Opcode('MVN', $D1, itSS1, nil));
    FOpcodes.Add('MVC', Opcode('MVC', $D2, itSS1, nil));
    FOpcodes.Add('NC', Opcode('NC', $D4, itSS1, nil));
    FOpcodes.Add('CLC', Opcode('CLC', $D5, itSS1, nil));
    FOpcodes.Add('OC', Opcode('OC', $D6, itSS1, nil));
    FOpcodes.Add('TR', Opcode('TR', $DC, itSS1, nil));
    FOpcodes.Add('ED', Opcode('ED', $DE, itSS1, nil));
    FOpcodes.Add('MVO', Opcode('MVO', $F1, itSS2, nil));
    FOpcodes.Add('PACK', Opcode('PACK', $F2, itSS2, nil));
    FOpcodes.Add('UNPK', Opcode('UNPK', $F3, itSS2, nil));
    FOpcodes.Add('ZAP', Opcode('ZAP', $F8, itSS2, nil));
    FOpcodes.Add('CP', Opcode('CP', $F9, itSS2, nil));
    FOpcodes.Add('AP', Opcode('AP', $FA, itSS2, nil));
    FOpcodes.Add('SP', Opcode('SP', $FB, itSS2, nil));
    FOpcodes.Add('MP', Opcode('MP', $FC, itSS2, nil));
    FOpcodes.Add('DP', Opcode('DP', $FD, itSS2, nil));
    FOpcodes.Add('BAL', Opcode('BAL', $45, itRX, nil));
    FOpcodes.Add('BC', Opcode('BC', $47, itRX, nil));
    FOpcodes.Add('SPSC', Opcode('SPSC', $A0, itSI, nil));
    FOpcodes.Add('LPSC', Opcode('LPSC', $A8, itSI, nil));
    FOpcodes.Add('SRC', Opcode('SRC', $A1, itSI, nil));
    FOpcodes.Add('XIOF', Opcode('XIOF', $A4, itSI, nil));
    FOpcodes.Add('TIO', Opcode('TIO', $A5, itSI, nil));
    FOpcodes.Add('BEQ', Opcode('BEQ', $47, itBranch, nil));
    FOpcodes.Add('BNE', Opcode('BNE', $47, itBranch, nil));
    FOpcodes.Add('BGT', Opcode('BGT', $47, itBranch, nil));
    FOpcodes.Add('BLT', Opcode('BLT', $47, itBranch, nil));
    FOpcodes.Add('BGE', Opcode('BGE', $47, itBranch, nil));
    FOpcodes.Add('BLE', Opcode('BLE', $47, itBranch, nil));
    FOpcodes.Add('BP', Opcode('BP', $47, itBranch, nil));
    FOpcodes.Add('BM', Opcode('BM', $47, itBranch, nil));
    FOpcodes.Add('BOV', Opcode('BOV', $47, itBranch, nil));
    FOpcodes.Add('BZ', Opcode('BZ', $47, itBranch, nil));
    FOpcodes.Add('BNZ', Opcode('BNZ', $47, itBranch, nil));
    FOpcodes.Add('B', Opcode('B', $47, itBranch, nil));
    FOpcodes.Add('DC', Opcode('DC', 0, itDirective, DC));
    FOpcodes.Add('DS', Opcode('DS', 0, itDirective, DS));
    FOpcodes.Add('END', Opcode('END', 0, itDirective, END_));
    FOpcodes.Add('START', Opcode('START', 0, itDirective, START));
end;

procedure TU9200AsmForm.DC(operand: String; sym: TSymbol; var pcBump: Smallint;
  var code: AnsiString);
var
    rslt: AnsiString;
    val: Smallint;
    len: Smallint;
    i: Integer;
    stemp: AnsiString;
    hex: Byte;
    reloc: Boolean;
begin
    if (IsCharacter(operand, rslt, len)) then
    begin
        code := rslt;
        pcBump := len;
    end else if (IsHex(operand, rslt, len)) then
    begin
        code := '';
        i := 1;
        while (i <= Length(rslt)) do
        begin
            stemp := AnsiChar('$') + rslt[i] + rslt[i + 1];
            hex := StrToInt(String(stemp));
            code := code + AnsiChar(Chr(hex));
            Inc(i, 2);
        end;
        pcBump := len;
    end else if (Pos('Y(', operand) > 0) then
    begin
        pcBump := 2;
        operand := Copy(operand, 3, Length(operand) - 3);
        Expression(operand, val, len, reloc);
        code := AnsiChar(Chr((val and $FF00) shr 8)) +
                AnsiChar(Chr(val and $FF));
    end else
        raise Exception.Create('Constant expected');
    if (Assigned(sym)) then
        sym.Length := pcBump;
end;

procedure TU9200AsmForm.DS(operand: String; sym: TSymbol; var pcBump: Smallint;
  var code: AnsiString);
begin
    if (Pos('''', operand) = 0) then
        operand := operand + '''''';
    DC(operand, sym, pcBump, code);
    code := '';
end;

procedure TU9200AsmForm.END_(operand: String; sym: TSymbol; var pcBump: Smallint;
  var code: AnsiString);
var
    num: Smallint;
    len: Smallint;
    reloc: Boolean;
begin
    pcBump := 0;
    code := '';
    Expression(operand, num, len, reloc);
    FXferAddr := num;
end;

procedure TU9200AsmForm.Expression(s: String; var rslt, len: Smallint; var reloc: Boolean);
var
    op1: String;
    op2: String;
    op: String;
    c: Char;
    value: Smallint;
    oplen: Smallint;

    procedure OperandValue(op: String; var value, len: Smallint);
    var
        sym: TSymbol;
        stemp: AnsiString;
        val: Integer;
        itemp: Smallint;
    begin
        value := 0;
        len := 0;
        if (LabelValid(op)) then
        begin
            if (FSymbols.TryGetValue(op, sym)) then
            begin
                if (not sym.NumDefines > 0) then
                    raise Exception.CreateFmt('Undefined identifier %s', [op]);
                value := sym.Addr;
                len := sym.Length;
            end else
            begin
                sym := TSymbol.Create;
                sym.Name := op;
                FSymbols.Add(op, sym);
                raise Exception.CreateFmt('Undefined identifier %s', [op]);
            end;
        end else if (IsNumber(op, val)) then
        begin
            value := Smallint(val);
            len := 1;
        end else if (IsCharacter(op, stemp, itemp)) then
        begin
            if (Length(stemp) <> 1) then
                raise Exception.CreateFmt('Character literal not 1 character', [op]);
            value := Ord(stemp[1]);
            len := 1;
        end;
    end;

    function Evaluate(op1, op2, op: String; var len: Smallint): Smallint;
    var
        val1: Smallint;
        val2: Smallint;
        len1: Smallint;
        len2: Smallint;
    begin
        OperandValue(op1, val1, len1);
        OperandValue(op2, val2, len2);
        case (op + ' ')[1] of
          '+':  Result := val1 + val2;
          '-':  Result := val1 - val2;
          else  raise Exception.CreateFmt('Illegal operator %s', [op]);
        end;
        len := Max(len1, len2);
    end;

begin
    op1 := '';
    op2 := '';
    for c in s do
    begin
        if ((c = '+') or (c = '-')) then
        begin
            op := c;
            if (op1 = '') then
            begin
                op1 := op2;
                op2 := '';
            end else
            begin
                value := Evaluate(op1, op2, op, oplen);
                op1 := IntToStr(value);
                op2 := '';
            end;
        end else
            op2 := op2 + c;
    end;
    if (op1 = '') then
        OperandValue(op2, rslt, len)
    else
        rslt := Evaluate(op1, op2, op, len);
end;

procedure TU9200AsmForm.GetAddress(var operand: String; var reg, len, offset: Integer);
var
    i: Integer;
    addr: String;
    l: String;
    r: String;
    c: Char;
    parenFound: Boolean;
    commaFound: Boolean;
    rslt: Smallint;
    reloc: Boolean;
    l1: Smallint;
begin
    reg := 0;
    len := 0;
    offset := 0;
    i := 1;
    addr := '';
    r := '';
    l := '';
    parenFound := False;
    commaFound := False;
    c := ' ';
    // Get the operand address
    while (i <= Length(operand)) do
    begin
        c := operand[i];
        if ((c = ',') or (c = '(')) then
            Break;
        addr := addr + c;
        Inc(i);
    end;
    // Get the length and register
    if (c = '(') then
    begin
        parenFound := True;
        Inc(i);
        while (i <= Length(operand)) do
        begin
            c := operand[i];
            if (c = ',') then
                commaFound := True
            else if (c = ')') then
            begin
                Inc(i);
                Break;
            end else
                if (commaFound) then
                    r := r + c
                else
                    l := l + c;
            Inc(i);
        end;
    end;
    operand := Copy(operand, i + 1);
    if (parenFound and (c <> ')')) then
        raise Exception.Create('Unbalanced parentheses');
    if (commaFound and (r = '')) then
        raise Exception.Create('Index register expected');
    Expression(addr, rslt, l1, reloc);
    len := l1;
    offset := rslt;
    if (l <> '') then
    begin
        if (not TryStrToInt(l, len)) then
            raise Exception.Create('Illegal length');
    end;
    if (r <> '') then
    begin
        if (not TryStrToInt(r, reg)) then
            raise Exception.Create('Illegal index register');
        if ((reg < 8) or (reg > 15)) then
            raise Exception.Create('Illegal index register');
    end;
    if (reg = 0) then
    begin
        reg := (offset and $F000) shr 12;
        offset := offset and $0FFF;
    end;
    if (offset > 4096) then
        raise Exception.Create('Offset > 4096');
end;

procedure TU9200AsmForm.Initialize;
var
    i: Integer;
begin
    FSymbols.Clear;
    for i := Low(FProgram) to High(FProgram) do
        FProgram[i] := 0;
end;

function TU9200AsmForm.IsCharacter(s: String; var rslt: AnsiString; var len: Smallint): Boolean;
var
    stemp: String;
    c: Char;
    c1: String;
    l1: String;
    split: Integer;
    aposSeen: Boolean;
    num: String;
    itemp: Integer;
begin
    Result := False;
    aposSeen := False;
    rslt := '';
    len := 0;
    stemp := '';
    c1 := Copy(s, 1, 1);
    if (c1 <> 'C') then
        Exit;
    s := Copy(s, 2);
    l1 := Copy(s, 1, 1);
    if (l1 = 'L') then
    begin
        s := Copy(s, 2);
        split := Pos('''', s);
        if (split <= 1) then
            raise Exception.Create('Illegal charcter constant. Length expected.');
        num := Copy(s, 1, split - 1);
        if (not TryStrToInt(num, itemp)) then
            raise Exception.Create('Illegal charcter constant. Length expected.');
        len := itemp;
        s := Copy(s, split);
    end else if (l1 <> '''') then
        Exit;
    stemp := Copy(s, 2, Length(s) - 2);
    for c in stemp do
    begin
        if (c = '''') then
        begin
            if (not aposSeen) then
                rslt := rslt + AnsiChar(c);
            aposSeen := not AposSeen;
        end else
            rslt := rslt + AnsiChar(c);
    end;
    if (len = 0) then
        len := Length(rslt);
    rslt := TCodeTranslator.AsciiToEbcdic(Copy(rslt + AnsiString(StringOfChar(' ', len)), 1, len));
    Result := True;
end;

function TU9200AsmForm.IsHex(s: String; var rslt: AnsiString; var len: Smallint): Boolean;
var
    stemp: String;
    c: Char;
    c1: String;
    l1: String;
    split: Integer;
    num: String;
    itemp: Integer;
begin
    Result := False;
    rslt := '';
    len := 0;
    stemp := '';
    c1 := Copy(s, 1, 1);
    if (c1 <> 'X') then
        Exit;
    s := Copy(s, 2);
    l1 := Copy(s, 1, 1);
    if (l1 = 'L') then
    begin
        s := Copy(s, 2);
        split := Pos('''', s);
        if (split <= 1) then
            raise Exception.Create('Illegal charcter constant. Length expected.');
        num := Copy(s, 1, split - 1);
        if (not TryStrToInt(num, itemp)) then
            raise Exception.Create('Illegal charcter constant. Length expected.');
        len := itemp;
        s := Copy(s, split);
    end else if (l1 <> '''') then
        Exit;
    stemp := Copy(s, 2, Length(s) - 2);
    for c in stemp do
    begin
        if (((c < 'A') or (c > 'F')) and
            ((c < '0') or (c > '9'))) then
            Exit;
        rslt := rslt + AnsiChar(c);
    end;
    if (len = 0) then
        len := (Length(rslt) div 2) + (Length(rslt) mod 2);
    if (Length(rslt) < (len * 2)) then
        rslt := AnsiString(StringOfChar('0', (len * 2) - Length(rslt))) + rslt
    else if (Length(rslt) > (len * 2)) then
        rslt := Copy(rslt, 1, (len * 2));
    Result := True;
end;

function TU9200AsmForm.IsNumber(s: String; var num: Integer): Boolean;
var
    test: Cardinal;
    rslt: AnsiString;
    len: Smallint;
begin
    Result := False;
    if (IsHex(s, rslt, len)) then
    begin
        if (len > 2) then
            Exit;
        Result := TryStrToInt('$' + String(rslt), num);
    end else
    begin
        Result := TryStrToInt(s, num);
    end;
    test := num and $ffff0000;
    if ((test <> 0) and (test <> $ffff0000)) then
        raise Exception.Create('Numeric constant out of range');
end;

function TU9200AsmForm.LabelValid(lbl: String): Boolean;
var
    c: Char;
begin
    Result := False;
    if ((lbl = '') or (Length(lbl) > 4)) then
        Exit;
    c := lbl[1];
    if ((c < 'A') or (c > 'Z')) then
        Exit;
    for c in lbl do
    begin
        if (((c < 'A') or (c > 'Z')) and
            ((c < '0') or (c > '9'))) then
            Exit;
    end;
    Result := True;
end;

procedure TU9200AsmForm.Pass1(fname: String);
var
    srcFile: TextFile;
    src: String;
    lbl, opcode, operands: String;
    sym: TSymbol;
    op: TOpcode;
    len: Smallint;
    code: AnsiString;
begin
    FPC := 0;
    FStmtCount := 0;
    AssignFile(srcFile, fname);
    Reset(srcFile);
    while (not Eof(srcFile)) do
    begin
        ReadLn(srcFile, src);
        src := UpperCase(src);
        TokenizeLine(src, lbl, opcode, operands);
        len := 0;
        op := nil;
        if (opcode <> '') then
        begin
            if (FOpcodes.TryGetValue(opcode, op)) then
            begin
                len := op.Length;
            end;
        end;
        sym := nil;
        if ((lbl <> '') and LabelValid(lbl)) then
        begin
            if (not FSymbols.ContainsKey(lbl)) then
            begin
                sym := TSymbol.Create;
                sym.Name := lbl;
                sym.Addr := FPC;
                sym.Length := len;
                sym.IsRelocatable := True;
                sym.NumDefines := 1;
                FSymbols.Add(lbl, sym);
            end else
            begin
                FSymbols.TryGetValue(lbl, sym);
                Inc(sym.NumDefines);
                if (sym.NumDefines = 1) then
                begin
                    sym.Addr := FPC;
                    sym.Length := len;
                    sym.IsRelocatable := True;
                end;
            end;
        end;
        if (Assigned(op)) then
        begin
            if (op.InstType = itDirective) then
            begin
                try
                    op.Proc(operands, sym, len, code);
                except
                    ;
                end;
            end;
            Inc(FStmtCount);
        end;
        FPC := FPC + len;
    end;
    CloseFile(srcFile);
end;

procedure TU9200AsmForm.Pass2(fname: String);
var
    srcFile: TextFile;
    pname: String;
    oname: String;
    prnFile: TextFile;
    src: String;
    lbl, opcode, operands: String;
    errMsg: String;
    split: Integer;
    len: Smallint;
    op: TOpcode;
    lineNum: Integer;
    code: AnsiString;
    i: Integer;
    pc: Integer;
    objFile: TFileStream;
    errCount: Integer;

    function HexOf(s: AnsiString): String;
    var
        c: AnsiChar;
    begin
        Result := '';
        for c in s do
            Result := Result + Format('%2.2x', [Ord(c)]);
    end;

    procedure PrintLine;
    var
        hex: String;
    begin
        hex := HexOf(code);
        WriteLn(prnFile, Format('%5d  %4.4x  %-12.12s   %s', [lineNum, FPC, Copy(hex, 1, 12), src]));
        if (errMsg <> '') then
            WriteLn(prnFile, Format('**** %s', [errMsg]));
        hex := Copy(hex, 13);
        while (hex <> '') do
        begin
            WriteLn(prnFile, Format('             %-12.12s', [Copy(hex, 1, 12)]));
            hex := Copy(hex, 13);
        end;
    end;

begin
    errCount := 0;
    FPC := 0;
    FStmtCount := 0;
    lineNum := 0;
    AssignFile(srcFile, fname);
    Reset(srcFile);
    split := LastDelimiter('.', fname);
    if (split > 0) then
    begin
        pname := Copy(fname, 1, split - 1) + '.lst';
        oname := Copy(fname, 1, split - 1) + '.hex';
    end else
    begin
        pname := fname + '.lst';
        oname := fname + '.hex';
    end;
    AssignFile(prnFile, pname);
    Rewrite(prnFile);
    while (not Eof(srcFile)) do
    begin
        ReadLn(srcFile, src);
        Inc(lineNum);
        src := UpperCase(src);
        try
            TokenizeLine(src, lbl, opcode, operands);
            len := 0;
            op := nil;
            code := '';
            if (opcode <> '') then
            begin
                if (FOpcodes.TryGetValue(opcode, op)) then
                begin
                    len := op.Length;
                    case op.InstType of
                      itDirective:
                      begin
                        op.Proc(operands, nil, len, code);
                      end;
                      itRX:
                      begin
                        RX(op, operands, code);
                      end;
                      itSI:
                      begin
                        SI(op, operands, code);
                      end;
                      itSS1:
                      begin
                        SS1(op, operands, code);
                      end;
                      itSS2:
                      begin
                        SS2(op, operands, code);
                      end;
                      itBranch:
                      begin
                        BRANCH(op, operands, code);
                      end;
                    end;
                    pc := FPC;
                    for i := 1 to Length(code) do
                    begin
                        if (pc <= High(FProgram)) then
                        begin
                            FProgram[pc] := Byte(code[i]);
                            Inc(pc);
                        end;
                    end;
                end else
                begin
                    raise Exception.CreateFmt('Illegal opcode %s', [opcode]);
                end;
            end;
        except
          on E: Exception do
          begin
              errMsg := E.Message;
              errCount := errCount + 1;
          end;
        end;
        PrintLine;
        FPC := FPC + len;
        errMsg := '';
    end;
    WriteLn(prnFile, '');
    WriteLn(prnFile, Format('%d errors', [errCount]));
    CloseFile(srcFile);
    CloseFile(prnFile);
    //
    objFile := TFileStream.Create(oname, fmCreate);
    try
        objFile.Write(FProgram[0], FPC);
    finally
        objFile.Free;
    end;

    ShowMessageFmt('%d errors', [errCount]);
end;

function TU9200AsmForm.RoundToWord(value: Integer): Integer;
var
    rem: Integer;
begin
    Result := value;
    rem := Result mod 4;
    if (rem <> 0) then
        Inc(Result, 4 - rem);
end;

procedure TU9200AsmForm.RX(op: TOpcode; operand: String; var code: AnsiString);
var
    split: Integer;
    stemp1: String;
    reg: Integer;
    baseReg, len, offset: Integer;
begin
    // Get the target register
    split := Pos(',', operand);
    if (split = 0) then
        raise Exception.Create('Register expected');
    stemp1 := Copy(operand, 1, split - 1);
    operand := Copy(operand, split + 1);
    if (not TryStrToInt(stemp1, reg)) then
        raise Exception.Create('Register expected');
    if ((op.Opcode <> $47) and ((reg < 8) or (reg > 15))) then
        raise Exception.Create('Illegal register number');
    // Get the operand address
    GetAddress(operand, baseReg, len, offset);
    code := ToBinary(Format('%2.2x%2.2x%1.1x%3.3x', [op.Opcode, reg shl 4, baseReg, offset]));
end;

procedure TU9200AsmForm.SI(op: TOpcode; operand: String; var code: AnsiString);
var
    baseReg, len, offset: Integer;
    idata: Smallint;
    len1: Smallint;
    reloc: Boolean;
begin
    // Get the operand address
    GetAddress(operand, baseReg, len, offset);
    // Get the immediate data
    Expression(operand, idata, len1, reloc);
    //
    code := ToBinary(Format('%2.2x%2.2x%1.1x%3.3x', [op.Opcode, idata, baseReg, offset]));
end;

procedure TU9200AsmForm.SS1(op: TOpcode; operand: String; var code: AnsiString);
var
    baseReg1, len1, offset1: Integer;
    baseReg2, len2, offset2: Integer;
begin
    // Get addresses and lengths
    GetAddress(operand, baseReg1, len1, offset1);
    GetAddress(operand, baseReg2, len2, offset2);
    //
    if ((len1 < 1) or (len1 > 256)) then
        raise Exception.CreateFmt('Illegal length %d', [len1]);
    code := ToBinary(Format('%2.2x%2.2x%1.1x%3.3x%1.1x%3.3x',
                            [op.Opcode, len1 - 1, baseReg1, offset1, baseReg2, offset2]));
end;

procedure TU9200AsmForm.SS2(op: TOpcode; operand: String; var code: AnsiString);
var
    baseReg1, len1, offset1: Integer;
    baseReg2, len2, offset2: Integer;
begin
    // Get addresses and lengths
    GetAddress(operand, baseReg1, len1, offset1);
    GetAddress(operand, baseReg2, len2, offset2);
    //
    if ((len1 < 1) or (len1 > 16)) then
        raise Exception.CreateFmt('Illegal length %d', [len1]);
    if ((len2 < 1) or (len2 > 16)) then
        raise Exception.CreateFmt('Illegal length %d', [len2]);
    code := ToBinary(Format('%2.2x%1.1x%1.1x%1.1x%3.3x%1.1x%3.3x',
                            [op.Opcode, len1 - 1, len2 - 1, baseReg1, offset1, baseReg2, offset2]));
end;

procedure TU9200AsmForm.START(operand: String; sym: TSymbol; var pcBump: Smallint;
  var code: AnsiString);
var
    num: Smallint;
    len: Smallint;
    reloc: Boolean;
begin
    pcBump := 0;
    code := '';
    if (FStmtCount <> 0) then
        raise Exception.Create('START must be first statement in program. Ignored.');
    Expression(operand, num, len, reloc);
    num := RoundToWord(num);
    FPC := num;
    if (Assigned(sym)) then
    begin
        sym.Addr := num;
        sym.IsRelocatable := True;
        sym.IsEntry := True;
        sym.Length := 0;
    end;
end;

function TU9200AsmForm.ToBinary(s: String): AnsiString;
var
    i: Integer;
    stemp: String;
    hex: Integer;
begin
    Result := '';
    i := 1;
    while (i <= Length(s)) do
    begin
        stemp := '$' + s[i] + s[i + 1];
        hex := StrToInt(stemp);
        Result := Result + AnsiChar(Chr(hex));
        Inc(i, 2);
    end;
end;

procedure TU9200AsmForm.TokenizeLine(src: String; var lbl, opcode, operands: String);
var
    split: Integer;
    c: Char;
    aposFound: Boolean;
begin
    lbl := '';
    opcode := '';
    operands := '';
    src := TrimRight(src);
    if (src = '') then                              // blank line
        Exit;
    c := (src + ' ')[1];
    if (c = '*') then                               // Comment
        Exit;
    // Get the label, if present
    if (c <> ' ') then                              // has label
    begin
        split := Pos(' ', src);                     // get the label and shift opcode to col 1
        if (split > 0) then
        begin
            lbl := Copy(src, 1, split - 1);
            src := TrimLeft(Copy(src, split + 1));
        end else
        begin
            lbl := src;
            src := '';
        end;
    end else
        src := TrimLeft(src);                       // shift opcode to col 1
    // Get the opcode, if present
    split := Pos(' ', src);
    if (split > 0) then
    begin
        opcode := Copy(src, 1, split - 1);
        src := TrimLeft(Copy(src, split + 1));
    end else
    begin
        opcode := src;
        src := '';
    end;
    // Get the operands. We can't just look for a space here
    // because spaces contained between apostrophes is part of the 
    // operand.
    aposFound := False;
    for c in src do
    begin
        case c of
          ' ':
          begin    
            if (not aposFound) then
                Break;
          end;
          '''':
          begin
            aposFound := not aposFound;
          end;
        end;
        operands := operands + c;
    end;
end;

{ TSymbolTable }

procedure TSymbolTable.Clear;
var
    pair: TPair<String, TSymbol>;
begin
    for pair in Self do
        pair.Value.Free;
    inherited Clear;
end;

{ TOpcodeTable }

procedure TOpcodeTable.Clear;
var
    pair: TPair<String, TOpcode>;
begin
    for pair in Self do
        pair.Value.Free;
    inherited Clear;
end;

{ TOpcode }

function TOpcode.Length: Integer;
begin
    case InstType of
      itDirective:  Result := 0;
      itRX:         Result := 4;
      itSI:         Result := 4;
      itSS1:        Result := 6;
      itSS2:        Result := 6;
      itBranch:     Result := 4;
      else          Result := 0;
    end;
end;

end.
