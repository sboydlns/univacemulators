unit U1005AsmFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections,
  U1005Types, U1005Memory, Vcl.ExtCtrls;

type
  TSymbol = class(TObject)
  public
    Name: String;
    Addr: Integer;                  // Memory address of variables
    Value: array [1..4] of Byte;    // Value of I/O intrinsic constants
    Length: Smallint;
    Sequence: String;
    NumDefines: Integer;
    LineNumber: Integer;
    IsEntry: Boolean;
    IsRelocatable: Boolean;
    constructor Create(nam: String; val: array of Byte); overload;
  end;

  TSymbolTable = class(TDictionary<String, TSymbol>)
  public
    procedure Clear; reintroduce;
  end;

  TOpcodeTable = class(TDictionary<String, T1005OpCode>)
  end;

  TSourceCard = packed record
    case Integer of
      1: (
        Sequence: array [1..5] of Char;
        Blank1: Char;
        Tag: array [1..3] of Char;
        Blank2: Char;
        Opcode: array [1..3] of Char;
        Blank3: Char;
        case Integer of
          0: (
            Operands: array [1..17] of Char;
            Comment: array [1..30] of Char;
            ID: array [1..4] of Char;
          );
          1: (
            Literal: array [1..34] of Char;
          );
      );
      2: (
        Card: array [1..80] of Char;
      );
  end;

  TSourceRec = record
  private
    Buffer: TSourceCard;
    function GetSequence: String;
    function StringOf(val: array of Char): String;
    function GetCard: String;
    function GetComment: String;
    function GetID: String;
    function GetLiteral: String;
    function GetOpcode: String;
    function GetOperands: String;
    function GetTag: String;
  public
    procedure Load(val: String);
    property Sequence: String read GetSequence;
    property Tag: String read GetTag;
    property Opcode: String read GetOpcode;
    property Operands: String read GetOperands;
    property Comment: String read GetComment;
    property Literal: String read GetLiteral;
    property ID: String read GetID;
    property Card: String read GetCard;
end;

  TU1005AsmForm = class(TForm)
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    ConvertBtn: TButton;
    CancelBtn: TButton;
    OpenDlg: TOpenDialog;
    SystemTypeBtn: TRadioGroup;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
  private
    FOpcodes: TOpcodeTable;
    FSymbols: TSymbolTable;
    FProgram: T1005Memory;
    FSourceFile: TextFile;
    FListFile: TextFile;
    FSourceRec: TSourceRec;
    FDollarSym: TSymbol;
    FBEGSeen: Boolean;
    FORGSeen: Boolean;
    FSTASeen: Boolean;
    FEndSeen: Boolean;
    FCrntAddr: Integer;
    FXferAddr: Integer;
    FLineNumber: Integer;
    FError: array [1..4] of Char;
    FPass: Integer;
    FProgramID: String;
    FLineCount: Integer;
    FSystemType: T1005SystemType;
    procedure BEG;
    procedure BF1;
    procedure BF2;
    procedure BF3;
    procedure BF4;
    procedure Character(op: T1005Opcode);
    procedure CheckBEGSeen;
    procedure CheckORGSeen;
    procedure CheckSTASeen;
    procedure Class1(op: T1005Opcode);
    procedure Class2(op: T1005Opcode);
    procedure Class3(op: T1005Opcode);
    procedure Class4(op: T1005Opcode);
    procedure Compile(fname: String);
    procedure CRD;
    procedure DASH;
    procedure ENDD;
    procedure Initialize;
    procedure Jump(op: T1005Opcode);
    procedure ORG;
    function LookupSymbol(tag: String; var sym: TSymbol): Boolean;
    procedure ParseOperand(var tag: String; var offset, len, val: Integer); overload;
    procedure ParseOperand(var tag: String; var offset, len: Integer; var val: Byte); overload;
    procedure Pass1(fname: String);
    procedure Pass2(fname: String);
    procedure PCH;
    procedure PLUS;
    procedure PrintLine; overload;
    procedure PrintLine(addr: Integer); overload;
    procedure PrintLine(addr: Integer; i1, i2, i3, i4, i5: Byte); overload;
    procedure PrintLine(addr, op1, op2: Integer; i1, i2, i3, i4, i5: Byte); overload;
    procedure PRT;
    procedure SaveInst(op, mr, mc, lr, lc: Byte);
    procedure SaveObject(fname: String);
    procedure SaveSymbol(tag: String; addr: Integer; defined: Boolean);
    procedure Shift(op: T1005Opcode);
    procedure ShowList(fname: String);
    procedure STA;
    procedure STAR;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U1005AsmForm: TU1005AsmForm;

implementation

uses EmulatorTypes, CardFile;

{$R *.dfm}

procedure TU1005AsmForm.BEG;
begin
    if ((FPass = 1) and FBEGSeen) then
        raise Exception.Create('Multiple BEG directives are not allowed.');
    FBEGSeen := True;
    FProgramID := FSourceRec.ID;
    PrintLine;
end;

procedure TU1005AsmForm.BF1;
begin
    CheckBEGSeen;
    FCrntAddr := 1;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.BF2;
begin
    CheckBEGSeen;
    FCrntAddr := 962;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.BF3;
begin
    CheckBEGSeen;
    FCrntAddr := 1923;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.BF4;
begin
    CheckBEGSeen;
    FCrntAddr := 2884;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU1005AsmForm.CancelBtnClick(Sender: TObject);
begin
    Close
end;

procedure TU1005AsmForm.Character(op: T1005Opcode);
// Compile character instructions. i.e. SHR m,l s
//
// I can't find anything that tells me how these instructions are
// encoded so I had to make some educated guesses.
var
    tag: String;
    offset, len: Integer;
    val: Byte;
    sym: TSymbol;
    m, l: I1005Addr;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    ParseOperand(tag, offset, len, val);
    if ((not LookupSymbol(tag, sym)) or (sym.NumDefines < 1)) then
        FError[2] := 'E';                   // Expression error (undefined symbol)
    // Since only the least significant byte of the operand
    // that is acted upon by character instructions I am going
    // to encode the second opcode byte and character in the M address.
    m := T1005FedSysAddr.Create;
    l := T1005FedSysAddr.Create;
    m.SetAddr(val, op.OpCode2);
    l.SetAddr(sym.Addr + offset + len - 1);
    PrintLine(FCrntAddr, m.SequentialAddr, l.SequentialAddr, op.OpCode, m.Row, m.Col, l.Row, l.Col);
    SaveInst(op.OpCode, m.Row, m.Col, l.Row, l.Col);
end;

procedure TU1005AsmForm.CheckBEGSeen;
begin
    if (not FBEGSeen) then
        raise Exception.Create('ERR NO BEG CRD');
end;

procedure TU1005AsmForm.CheckORGSeen;
begin
    if (not FORGSeen) then
        raise Exception.Create('Missing ORG directive');
end;

procedure TU1005AsmForm.CheckSTASeen;
begin
    if (not FSTASeen) then
        raise Exception.Create('ERR OP IN DATA DIV');
end;

procedure TU1005AsmForm.Class1(op: T1005Opcode);
var
    tag: String;
    offset, len, val: Integer;
    sym: TSymbol;
    m, l: I1005Addr;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    ParseOperand(tag, offset, len, val);
    m := T1005FedSysAddr.Create;
    l := T1005FedSysAddr.Create;
    if ((not LookupSymbol(tag, sym)) or (sym.NumDefines < 1)) then
    begin
        FError[2] := 'E';                   // Expression error (undefined symbol)
        m.Clear;
        l.Clear;
    end else
    begin
        m.SetAddr(sym.Addr + offset);
        l.SetAddr(sym.Addr + offset + len - 1);
    end;
    PrintLine(FCrntAddr, m.SequentialAddr, l.SequentialAddr, op.OpCode, m.Row, m.Col, l.Row, l.Col);
    SaveInst(op.OpCode, m.Row, m.Col, l.Row, l.Col);
end;

procedure TU1005AsmForm.Class2(op: T1005Opcode);
var
    tag: String;
    offset, len, val: Integer;
    sym: TSymbol;
    m, l: I1005Addr;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    ParseOperand(tag, offset, len, val);
    if ((not LookupSymbol(tag, sym)) or (sym.NumDefines < 1)) then
        FError[2] := 'E';                   // Expression error (undefined symbol)
    //
    m := T1005FedSysAddr.Create;
    l := T1005FedSysAddr.Create;
    m.SetAddr(sym.Addr + offset);
    l.SetAddr(sym.Addr + offset + 4);
    PrintLine(FCrntAddr, m.SequentialAddr, l.SequentialAddr, op.OpCode, m.Row, m.Col, l.Row, l.Col);
    SaveInst(op.OpCode, m.Row, m.Col, l.Row, l.Col);
end;

procedure TU1005AsmForm.Class3(op: T1005Opcode);
var
    operand: array [1..4] of Byte;
    val: String;
    sym: TSymbol;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    val := TrimRight(FSourceRec.Operands);
    if (LookupSymbol(val, sym)) then
    begin
        PrintLine(FCrntAddr, op.OpCode, sym.Value[1], sym.Value[2], sym.Value[3], sym.Value[4]);
        SaveInst(op.OpCode, sym.Value[1], sym.Value[2], sym.Value[3], sym.Value[4]);
    end else
    begin
        val := Copy(val + '    ', 1, 4);
        operand[1] := TCodeTranslator.AsciiToXS3(AnsiChar(val[1]));
        operand[2] := TCodeTranslator.AsciiToXS3(AnsiChar(val[2]));
        operand[3] := TCodeTranslator.AsciiToXS3(AnsiChar(val[3]));
        operand[4] := TCodeTranslator.AsciiToXS3(AnsiChar(val[4]));
        PrintLine(FCrntAddr, op.OpCode, operand[1], operand[2], operand[3], operand[4]);
        SaveInst(op.OpCode, operand[1], operand[2], operand[3], operand[4]);
    end;
end;

procedure TU1005AsmForm.Class4(op: T1005Opcode);
begin
end;

procedure TU1005AsmForm.Compile(fname: String);
var
    split: Integer;
    listFile: String;
begin
    FreeAndNil(FProgram);
    FProgram := T1005Memory.Create(FSystemType);
    AssignFile(FSourceFile, fname);
    try
        split := LastDelimiter('.', fname);
        if (split > 0) then
            fname := Copy(fname, 1, split - 1);
        listFile := fname + '.lst';
        AssignFile(FListFile, listFile);
        try
            ReWrite(FListFile);
            try
                Initialize;
                Pass1(fname);
                Write(FListFile, #12);
                WriteLn(FListFile,
                        'SEQ # LBL OP  OPERAND           COMMENTS                     IDEN LOC  OPERAND           ERR' +
                        '     C/C    INSTR   LOC    SAAL 2');
                Pass2(fname);
                split := LastDelimiter('.', fname);
                if (split > 0) then
                    fname := Copy(fname, 1, split - 1);
                fname := fname + '.h16';
                SaveObject(fname);
            except
              on E: Exception do
              begin
                WriteLn(FListFile, E.Message);
                Write(FListFile, #12);
              end;
            end;
        finally
            CloseFile(FListFile);
        end;
    finally
        CloseFile(FSourceFile);
    end;
    ShowList(listFile);
end;

procedure TU1005AsmForm.ConvertBtnClick(Sender: TObject);
begin
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the source file name');
    if (SystemTypeBtn.ItemIndex = 0) then
        FSystemType := stFedSys
    else
        FSystemType := stComm;
    Compile(FileNameEdt.Text);
end;

procedure TU1005AsmForm.CRD;
begin
    CheckBEGSeen;
    FCrntAddr := 1;
    PrintLine(FCrntAddr);
end;

constructor TU1005AsmForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FOpcodes := TOpcodeTable.Create;
    FSymbols := TSymbolTable.Create;
    FDollarSym := TSymbol.Create;
    FDollarSym.Name := '$  ';
    FDollarSym.NumDefines := 1;
    FOpcodes.Add('LA1', OpCode(X3_SPACE, 'LA1', itClass1));
    FOpcodes.Add('LA2', OpCode(X3_APOS, 'LA2', itClass1));
    FOpcodes.Add('LD1', OpCode(X3_RIGHT_SQUARE, 'LD1', itClass1));
    FOpcodes.Add('LD2', OpCode(X3_ASTERIX, 'LD2', itClass1));
    FOpcodes.Add('LPR', OpCode(X3_0, 'LPR', itClass1));
    FOpcodes.Add('SA1', OpCode(X3_4, 'SA1', itClass1));
    FOpcodes.Add('SA2', OpCode(X3_M, 'SA2', itClass1));
    FOpcodes.Add('SD1', OpCode(X3_SEMI, 'SD1', itClass1));
    FOpcodes.Add('SD2', OpCode(X3_AT, 'SD2', itClass1));
    FOpcodes.Add('SPR', OpCode(X3_I, 'SPR', itClass1));
    FOpcodes.Add('SHR', OpCode(X3_F, 'SHR', itShift));
    FOpcodes.Add('SHL', OpCode(X3_PERIOD, 'SHL', itShift));
    FOpcodes.Add('CLR', OpCode(X3_1, 'CLR', itClass1));
    FOpcodes.Add('SC ', OpCode(X3_NOT_EQUAL, 1, 'SC', itCharacter));
    FOpcodes.Add('CA1', OpCode(X3_5, 'CA1', itClass1));
    FOpcodes.Add('CA2', OpCode(X3_N, 'CA2', itClass1));
    FOpcodes.Add('CN1', OpCode(X3_COLON, 'CN1', itClass1));
    FOpcodes.Add('CN2', OpCode(X3_PERCENT, 'CN2', itClass1));
    FOpcodes.Add('IC ', OpCode(X3_MINUS, 'IC', itClass2));
    FOpcodes.Add('CCA', OpCode(X3_NOT_EQUAL, 2, 'CCA', itCharacter));
    FOpcodes.Add('J  ', OpCode(X3_2, 'J', itJump));
    FOpcodes.Add('JG ', OpCode(X3_B, 'JG', itJump));
    FOpcodes.Add('JL ', OpCode(X3_7, 'JL', itJump));
    FOpcodes.Add('JE ', OpCode(X3_8, 'JE', itJump));
    FOpcodes.Add('JEA', OpCode(X3_8, 'JEA', itJump));
    FOpcodes.Add('JUA', OpCode(X3_7, 'JUA', itJump));
    FOpcodes.Add('JP ', OpCode(X3_7, 'JP', itJump));
    FOpcodes.Add('JN ', OpCode(X3_B, 'JN', itJump));
    FOpcodes.Add('JZ ', OpCode(X3_8, 'JZ', itJump));
    FOpcodes.Add('JR ', OpCode(X3_D, 'JR', itJump));
    FOpcodes.Add('JX ', OpCode(X3_LEFT_SQUARE, 'JX', itJump));
    FOpcodes.Add('JS3', OpCode(X3_V, 1, 'JS3', itJump));
    FOpcodes.Add('JOF', OpCode(X3_V, 2, 'JOF', itJump));
    FOpcodes.Add('AM1', OpCode(X3_LESS, 'AM1', itClass1));
    FOpcodes.Add('AM2', OpCode(X3_GREATER, 'AM2', itClass1));
    FOpcodes.Add('AR1', OpCode(X3_SHARP, 'AR1', itClass1));
    FOpcodes.Add('AR2', OpCode(X3_LOZENGE, 'AR2', itClass1));
    FOpcodes.Add('SM1', OpCode(X3_H, 'SM1', itClass1));
    FOpcodes.Add('SM2', OpCode(X3_Y, 'SM2', itClass1));
    FOpcodes.Add('SR1', OpCode(X3_C, 'SR1', itClass1));
    FOpcodes.Add('SR2', OpCode(X3_T, 'SR2', itClass1));
    FOpcodes.Add('MUL', OpCode(X3_BACK_SLASH, 'MUL', itClass1));
    FOpcodes.Add('DIV', OpCode(X3_G, 'DIV', itClass1));
    FOpcodes.Add('TRL', OpCode(X3_A, 'TRL', itClass1));
    FOpcodes.Add('SZS', OpCode(X3_O, 'SZS', itClass1));
    FOpcodes.Add('LWS', OpCode(X3_QUESTION, 'LWS', itClass1));
    FOpcodes.Add('LN1', OpCode(X3_3, 'LN1', itClass1));
    FOpcodes.Add('LN2', OpCode(X3_L, 'LN2', itClass1));
    FOpcodes.Add('SED', OpCode(X3_R, 'SED', itClass1));
    FOpcodes.Add('LAN', OpCode(X3_NOT_EQUAL, 3, 'LAN', itCharacter));
    FOpcodes.Add('LOR', OpCode(X3_NOT_EQUAL, 4, 'LOR', itCharacter));
    FOpcodes.Add('BSH', OpCode(X3_NOT_EQUAL, 'BSH', itClass1));
    FOpcodes.Add('PTE', OpCode(X3_E, 'PTE', itClass2));
    FOpcodes.Add('XFC', OpCode(X3_AMP, 'XFC', itClass3));
    FOpcodes.Add('XF ', OpCode(X3_AMP, 'XF', itClass3));
    FOpcodes.Add('JPE', OpCode(X3_V, 3, 'JPE', itJump));
    FOpcodes.Add('JET', OpCode(X3_V, 4, 'JET', itJump));
    FOpcodes.Add('JI1', OpCode(X3_V, 5, 'JI1', itJump));
    FOpcodes.Add('JAL', OpCode(X3_V, 6, 'JAL', itJump));
    // Assembler directives
    FOpcodes.Add('BEG', OpCode(0, 'BEG', itProc, False, BEG));
    FOpcodes.Add('CRD', OpCode(0, 'CRD', itProc, False, CRD));
    FOpcodes.Add('PCH', OpCode(0, 'PCH', itProc, False, PCH));
    FOpcodes.Add('PRT', OpCode(0, 'PRT', itProc, False, PRT));
    FOpcodes.Add('BF1', OpCode(0, 'BF1', itProc, False, BF1));
    FOpcodes.Add('BF2', OpCode(0, 'BF2', itProc, False, BF2));
    FOpcodes.Add('BF3', OpCode(0, 'BF3', itProc, False, BF3));
    FOpcodes.Add('BF4', OpCode(0, 'BF4', itProc, False, BF4));
    FOpcodes.Add('-  ', OpCode(0, '-', itProc, False, DASH));
    FOpcodes.Add('ORG', OpCode(0, 'ORG', itProc, False, ORG));
    FOpcodes.Add('+  ', OpCode(0, '+', itProc, False, PLUS));
    FOpcodes.Add('*  ', OpCode(0, '*', itProc, False, STAR));
    FOpcodes.Add('STA', OpCode(0, 'STA', itProc, False, STA));
    FOpcodes.Add('END', OpCode(0, 'END', itProc, False, ENDD));
end;

procedure TU1005AsmForm.DASH;
var
    tag: String;
    bank, row, col: Byte;
    cbank: Byte;
    offset: Integer;
    addr: I1005Addr;
begin
    CheckBEGSeen;
    tag := FSourceRec.Tag;
    if (not TryStrToInt(TrimRight(FSourceRec.Operands), offset)) then
        FError[2] := 'E';                   // Illegal buffer offset
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(FCrntAddr);
    addr.Decode(cbank, row, col);
    addr.SetAddr(FCrntAddr + offset);
    addr.Decode(bank, row, col);
    if (bank <> cbank) then
        FError[2] := 'E';                   // Cannot span banks
    SaveSymbol(tag, FCrntAddr + offset - 1, True);
    PrintLine(FCrntAddr + offset - 1);
end;

procedure TU1005AsmForm.ENDD;
var
    tag: String;
    sym: TSymbol;
begin
    CheckBEGSeen;
    CheckSTASeen;
    FEndSeen := True;
    tag := Copy(FSourceRec.Operands, 1, 3);
    if (FSymbols.TryGetValue(tag, sym)) then
    begin
        FXferAddr := sym.Addr;
    end else
    begin
        FError[2] := 'E';                   // Expression error (undefined symbol)
    end;
    PrintLine(FXferAddr);
end;

procedure TU1005AsmForm.Initialize;
var
    bank, row, col: Byte;
begin
    FBEGSeen := False;
    FORGSeen := False;
    FPass := 1;
    FCrntAddr := 1;
    FXferAddr := 1;
    FLineCount := 999;
    FDollarSym.Addr := FCrntAddr;
    FSymbols.Clear;
    // I/O command intrinsic constants
    FSymbols.Add('REA', TSymbol.Create('REA', [X3_RIGHT_PAREN, X3_DELTA, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('PR1', TSymbol.Create('PR1', [X3_DELTA, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('PR2', TSymbol.Create('PR2', [X3_U, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('PR7', TSymbol.Create('PR7', [X3_N, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('PUN', TSymbol.Create('PUN', [X3_RIGHT_PAREN, X3_LOZENGE, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RPR', TSymbol.Create('RPR', [X3_DELTA, X3_DELTA, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RP2', TSymbol.Create('RP2', [X3_U, X3_DELTA, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RPH', TSymbol.Create('RPH', [X3_RIGHT_PAREN, X3_W, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RPP', TSymbol.Create('RPP', [X3_DELTA, X3_W, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('SK2', TSymbol.Create('SK2', [X3_LOZENGE, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('SK4', TSymbol.Create('SK4', [X3_GREATER, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('SK7', TSymbol.Create('SK7', [X3_V, X3_RIGHT_PAREN, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RCI', TSymbol.Create('RCI', [X3_RIGHT_PAREN, X3_DELTA, X3_RIGHT_PAREN, X3_U]));
    FSymbols.Add('PCI', TSymbol.Create('PCI', [X3_RIGHT_PAREN, X3_LOZENGE, X3_RIGHT_PAREN, X3_Y]));
    FSymbols.Add('RXC', TSymbol.Create('RXC', [X3_RIGHT_PAREN, X3_U, X3_RIGHT_PAREN, X3_U]));
    FSymbols.Add('RX1', TSymbol.Create('RX1', [X3_RIGHT_PAREN, X3_U, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RX2', TSymbol.Create('RX2', [X3_RIGHT_PAREN, X3_U, X3_EQUAL, X3_RIGHT_PAREN]));
    FSymbols.Add('RX3', TSymbol.Create('RX3', [X3_RIGHT_PAREN, X3_U, X3_DELTA, X3_RIGHT_PAREN]));
    FSymbols.Add('PSS', TSymbol.Create('PSS', [X3_RIGHT_PAREN, X3_LOZENGE, X3_U, X3_RIGHT_PAREN]));
    FSymbols.Add('RRP', TSymbol.Create('RRP', [X3_RIGHT_PAREN, X3_W, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    FSymbols.Add('RRS', TSymbol.Create('RRS', [X3_RIGHT_PAREN, X3_W, X3_U, X3_RIGHT_PAREN]));
    FSymbols.Add('RRC', TSymbol.Create('RRC', [X3_RIGHT_PAREN, X3_W, X3_RIGHT_PAREN, X3_Y]));
    FSymbols.Add('HLT', TSymbol.Create('HLT', [X3_RIGHT_PAREN, X3_GREATER, X3_RIGHT_PAREN, X3_RIGHT_PAREN]));
    //
    for bank := 1 to 4 do
        for row := 1 to 32 do
            for col := 1 to 32 do
                FProgram.StoreByte(bank, row, col, 255);
end;

procedure TU1005AsmForm.Jump(op: T1005Opcode);
var
    tag: String;
    offset, len, val: Integer;
    mstart: Integer;
    sym: TSymbol;
    m, l: I1005Addr;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    ParseOperand(tag, offset, len, val);
    if (FPass = 1) then
    begin
        if (not LookupSymbol(tag, sym)) then
        begin
            SaveSymbol(tag, FCrntAddr, False);
            LookupSymbol(tag, sym);
        end;
    end else
    begin
        if ((not LookupSymbol(tag, sym)) or (sym.NumDefines < 1)) then
            FError[2] := 'E';               // Expression error (undefined symbol)
    end;
    //
    m := T1005FedSysAddr.Create;
    l := T1005FedSysAddr.Create;
    m.SetAddr(sym.Addr + offset);
    mstart := (m.SequentialAddr - 1) mod 31;
    if ((mstart mod 5) <> 0) then
        FError[2] := 'E';                   // Illegal jump target. Must be multiple of 5
    // There are some jump instructions that seem to share the same opcode (X3_V). I don't
    // know how these were encoded in the original 1005 so I am encoding a second opcode
    // byte as L.Col for those instructions.
    if (op.OpCode2 <> 0) then
        l.SetAddr(0, op.OpCode2)
    else
        l.SetAddr(sym.Addr + offset + 4);
    PrintLine(FCrntAddr, m.SequentialAddr, l.SequentialAddr, op.OpCode, m.Row, m.Col, l.Row, l.Col);
    SaveInst(op.OpCode, m.Row, m.Col, l.Row, l.Col);
end;

function TU1005AsmForm.LookupSymbol(tag: String; var sym: TSymbol): Boolean;
begin
    tag := Copy(Trim(tag) + '   ', 1, 3);
    if (tag = '$  ') then
    begin
        Result := True;
        FDollarSym.Addr := FCrntAddr;
        sym := FDollarSym;
    end else
        Result := FSymbols.TryGetValue(tag, sym);
end;

procedure TU1005AsmForm.ORG;
begin
    CheckBEGSeen;
    FORGSeen := True;
    if (not TryStrToInt(Copy(FSourceRec.Operands, 1, 4), FCrntAddr)) then
        FError[2] := 'E';                   // Invalid address
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.ParseOperand(var tag: String; var offset, len, val: Integer);
var
    operand: String;
    split: Integer;
    stemp: String;
begin
    tag := '';
    offset := 0;
    len := 0;
    val := 0;
    operand := TrimRight(FSourceRec.Operands);
    // Separate the address and length from any additional value
    split := Pos(' ', operand);
    if (split <> 0) then
    begin
        stemp := Copy(operand, split + 1);
        if (not TryStrToInt(stemp, val)) then
            FError[2] := 'E';                   //  Illegal value
        operand := Copy(operand, 1, split - 1);
    end;
    // Separate the address from the length
    split := Pos(',', operand);
    if (split = 0) then
    begin
        tag := operand;
        len := 0;
    end else
    begin
        tag := Copy(operand, 1, split - 1);
        if ((not TryStrToInt(Copy(operand, split + 1), len)) or
            (len < 1) or
            (len > 999)) then
            FError[2] := 'E';                   // Illegal length
    end;
    // Separate the variable name from the offset, if present
    split := Pos('+', tag);
    if (split <> 0) then
    begin
        if (not TryStrToInt(Copy(tag, split + 1), offset)) then
            FError[2] := 'E';                   // Expression error
        tag := Copy(tag, 1, split - 1);
    end else
    begin
        split := Pos('-', tag);
        if (split > 0) then
        begin
            if (not TryStrToInt(Copy(tag, split + 1), offset)) then
                FError[2] := 'E';                   // Expression error (undefined symbol)
            offset := -offset;
            tag := Copy(tag, 1, split - 1);
        end;
    end;
    tag := Copy(tag + '   ', 1, 3);
end;

procedure TU1005AsmForm.ParseOperand(var tag: String; var offset, len: Integer; var val: Byte);
var
    operand: String;
    split: Integer;
    stemp: String;
begin
    tag := '';
    offset := 0;
    len := 0;
    val := 0;
    operand := TrimRight(FSourceRec.Operands);
    // Separate the address and length from any additional value
    split := Pos(' ', operand);
    if (split <> 0) then
    begin
        stemp := Copy(operand, split + 1);
        if (Length(stemp) > 0) then
            val := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[1]))
        else
            val := X3_SPACE;
        operand := Copy(operand, 1, split - 1);
    end;
    // Separate the address from the length
    split := Pos(',', operand);
    if (split = 0) then
    begin
        tag := operand;
        len := 0;
    end else
    begin
        tag := Copy(operand, 1, split - 1);
        if ((not TryStrToInt(Copy(operand, split + 1), len)) or
            (len < 1) or
            (len > 999)) then
            FError[2] := 'E';                   //  Illegal length
    end;
    // Separate the variable name from the offset, if present
    split := Pos('+', tag);
    if (split <> 0) then
    begin
        if (not TryStrToInt(Copy(tag, split + 1), offset)) then
            FError[2] := 'E';                   // Expression error
        tag := Copy(tag, 1, split - 1);
    end else
    begin
        split := Pos('-', tag);
        if (split > 0) then
        begin
            if (not TryStrToInt(Copy(tag, split + 1), offset)) then
                FError[2] := 'E';                   // Expression error
            offset := -offset;
            tag := Copy(tag, 1, split - 1);
        end;
    end;
end;

procedure TU1005AsmForm.Pass1(fname: String);
var
    bfr: String;
    op: T1005Opcode;
begin
    Reset(FSourceFile);
    FLineNumber := 0;
    while (not Eof(FSourceFile)) do
    begin
        ReadLn(FSourceFile, bfr);
        Inc(FLineNumber);
        FError[1] := ' ';
        FError[2] := ' ';
        FError[3] := ' ';
        Ferror[4] := ' ';
        FSourceRec.Load(bfr);
        if (Trim(bfr) = '') then
        begin
            PrintLine;
            Continue;
        end;
        if (FOpcodes.TryGetValue(FSourceRec.Opcode, op)) then
        begin
            case op.InstType of
              itClass1:     Class1(op);
              itClass2:     Class2(op);
              itClass3:     Class3(op);
              itClass4:     Class4(op);
              itShift:      Shift(op);
              itJump:       Jump(op);
              itCharacter:  Character(op);
              itProc:       op.Proc;
              itUnknown: ;
            end;
        end else  if (Copy(FSourceRec.Opcode, 1, 1) = '+') then
        begin
            // Special for + directive because literal length is encoded
            // as part of the opcode.
            if (FOpcodes.TryGetValue('+  ', op)) then
            begin
                op.Proc;
            end else
                raise Exception.Create('OOPS! + not in opcode table');
        end else  if (Copy(FSourceRec.Opcode, 1, 1) = '*') then
        begin
            // Special for * directive because the comment could start
            // in the operand field
            if (FOpcodes.TryGetValue('*  ', op)) then
            begin
                op.Proc;
            end else
                raise Exception.Create('OOPS! * not in opcode table');
        end else
        begin
            FError[1] := 'O';                   //  Invalid opcode
            PrintLine;
        end;
    end;
end;

procedure TU1005AsmForm.Pass2(fname: String);
begin
    FPass := 2;
    Pass1(fname);
end;

procedure TU1005AsmForm.PCH;
begin
    CheckBEGSeen;
    FCrntAddr := 293;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.PLUS;
var
    len: Integer;
    addr: I1005Addr;
    i: Integer;
begin
    CheckBEGSeen;
    CheckORGSeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    if (not TryStrToInt(TrimRight(Copy(FSourceRec.Opcode, 2)), len)) then
        FError[2] := 'E';                   // Invalid literal length
    if ((len < 1) or (len > 34) or ((FCrntAddr + len) > 4096)) then
        FError[2] := 'E';                   // Invalid literal length
    addr := T1005FedSysAddr.Create;
    for i := 1 to len do
    begin
        addr.SetAddr(FCrntAddr + i - 1);
        FProgram.StoreByte(addr, TCodeTranslator.AsciiToXS3(AnsiChar(FSourceRec.Buffer.Literal[i])));
    end;
    PrintLine(FCrntAddr);
    Inc(FCrntAddr, len);
end;

procedure TU1005AsmForm.PrintLine(addr: Integer; i1, i2, i3, i4, i5: Byte);
var
    inst, loc: AnsiString;
    ad: I1005Addr;
begin
    if (FPass <> 2) then
        Exit;
    inst := TCodeTranslator.XS3ToAscii(i1) +
            TCodeTranslator.XS3ToAscii(i2) +
            TCodeTranslator.XS3ToAscii(i3) +
            TCodeTranslator.XS3ToAscii(i4) +
            TCodeTranslator.XS3ToAscii(i5);
    ad := T1005FedSysAddr.Create;
    ad.SetAddr(addr);
    loc := TCodeTranslator.XS3ToAscii(ad.Row) + TCodeTranslator.XS3ToAscii(ad.Col);
    WriteLn(FListFile,
            Format('%65.65s %4.4d%-19.19s%-4.4s%-11.11s%-5.5s   %-2.2s',
                   [Copy(FSourceRec.Card, 1, 65),
                    addr,
                    ' ',
                    String(FError),
                    ' ',
                    String(inst),
                    loc]));
end;

procedure TU1005AsmForm.PrintLine(addr, op1, op2: Integer; i1, i2, i3, i4, i5: Byte);
var
    inst, loc: AnsiString;
    ad: I1005Addr;
begin
    if (FPass <> 2) then
        Exit;
    inst := TCodeTranslator.XS3ToAscii(i1) +
            TCodeTranslator.XS3ToAscii(i2) +
            TCodeTranslator.XS3ToAscii(i3) +
            TCodeTranslator.XS3ToAscii(i4) +
            TCodeTranslator.XS3ToAscii(i5);
    ad := T1005FedSysAddr.Create;
    ad.SetAddr(addr);
    loc := TCodeTranslator.XS3ToAscii(ad.Row) + TCodeTranslator.XS3ToAscii(ad.Col);
    WriteLn(FListFile,
            Format('%65.65s %4.4d %4.4d %4.4d%-9.9s%-4.4s%-11.11s%-5.5s   %-2.2s',
                   [Copy(FSourceRec.Card, 1, 65),
                    addr,
                    op1,
                    op2,
                    ' ',
                    String(FError),
                    ' ',
                    String(inst),
                    loc]));
end;

procedure TU1005AsmForm.PrintLine;
begin
    if (FPass <> 2) then
        Exit;
    WriteLn(FListFile,
            Format('%-65.65s%-24.24s%-4.4s',
                   [Copy(FSourceRec.Card, 1, 65),
                    ' ',
                    String(FError)]));
end;

procedure TU1005AsmForm.PrintLine(addr: Integer);
var
    loc: AnsiString;
    ad: I1005Addr;
begin
    if (FPass <> 2) then
        Exit;
    ad := T1005FedSysAddr.Create;
    ad.SetAddr(addr);
    loc := TCodeTranslator.XS3ToAscii(ad.Row) + TCodeTranslator.XS3ToAscii(ad.Col);
    WriteLn(FListFile,
            Format('%65.65s %4.4d%-19.19s%-4.4s%-19.19s%-2.2s',
                   [Copy(FSourceRec.Card, 1, 65),
                    addr,
                    ' ',
                    String(FError),
                    ' ',
                    loc]));
end;

procedure TU1005AsmForm.PRT;
begin
    CheckBEGSeen;
    FCrntAddr := 161;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.SaveInst(op, mr, mc, lr, lc: Byte);
var
    bank, row, col: Byte;
    addr, addr2: I1005Addr;
begin
    addr := T1005FedSysAddr.Create;
    addr2 := T1005FedSysAddr.Create;
    addr.SetAddr(FCrntAddr);
    FProgram.StoreByte(addr, op);
    Inc(FCrntAddr);
    addr.SetAddr(FCrntAddr);
    FProgram.StoreByte(addr, mr);
    Inc(FCrntAddr);
    addr.SetAddr(FCrntAddr);
    FProgram.StoreByte(addr, mc);
    Inc(FCrntAddr);
    addr.SetAddr(FCrntAddr);
    FProgram.StoreByte(addr, lr);
    Inc(FCrntAddr);
    addr.SetAddr(FCrntAddr);
    FProgram.StoreByte(addr, lc);
    Inc(FCrntAddr);
    addr.SetAddr(FCrntAddr);
    addr.Decode(bank, row, col);
    // If we just saved the last instruction in a row, save
    // row ID of the next row in column 31 and advance to
    // the next row.
    if (col = 31) then
    begin
        addr2.SetAddr(addr.Row, addr.Col);
        addr2.Increment;
        FProgram.StoreByte(addr, addr2.Row and $1f);
        FCrntAddr := addr2.SequentialAddr;
    end;
end;

procedure TU1005AsmForm.SaveObject(fname: String);
var
    done: Boolean;
    b: Byte;
    lastAddr: Integer;
    seq: Integer;
    stemp: String;
    objRecEmpty: Boolean;
    objCol: Integer;
    objStart: Integer;
    objRec: TCardRec;
    objFile: TCardFileStream;
    addr: T1005FedSysAddr;
    atemp: T1005FedSysAddr;

    procedure ClearObjRec;
    begin
        objRecEmpty := True;
        objCol := 1;
        objStart := addr.SequentialAddr;
        objRec.Clear;
        stemp := Format('%3.3d', [seq mod 1000]);
        objRec.Columns[64] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[1]));
        objRec.Columns[65] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[2]));
        objRec.Columns[66] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[3]));
        stemp := Copy(FProgramID + '    ', 1, 4);
        objRec.Columns[68] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[1]));
        objRec.Columns[69] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[2]));
        objRec.Columns[70] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[3]));
        objRec.Columns[71] := TCodeTranslator.AsciiToXS3(AnsiChar(stemp[4]));
        objRec.Count := 80;
    end;

    procedure WriteObjRec;
    begin
        objRec.Columns[74] := X3_RIGHT_SQUARE;
        atemp.SetAddr(objStart);
        objRec.Columns[77] := atemp.Row;
        objRec.Columns[78] := atemp.Col;
        atemp.SetAddr(objStart + objCol - 2);
        objRec.Columns[79] := atemp.Row;
        objRec.Columns[80] := atemp.Col;
        objFile.WriteXS3(objRec);
        Inc(seq);
    end;

    procedure WriteXferRec;
    begin
        objRec.Columns[74] := X3_SLASH;
        atemp.SetAddr(FXferAddr);
        objRec.Columns[77] := atemp.Row;
        objRec.Columns[78] := atemp.Col;
        objFile.WriteXS3(objRec);
    end;

begin
    addr.SetAddr(1);
    ClearObjRec;
    seq := 1;
    lastAddr := -1;
    objFile := TCardFileStream.Create(fname, fmCreate);
    try
        done := False;
        while (not done) do
        begin
            try
                b := FProgram.FetchByte(addr);
                if (b = 255) then
                begin
                    if (not objRecEmpty) then
                    begin
                        WriteObjRec;
                        ClearObjRec;
                    end;
                end else
                begin
                    if ((lastAddr <> (addr.SequentialAddr - 1)) or (objCol > 62)) then
                    begin
                        // start new obj rec
                        if (not objRecEmpty) then
                            WriteObjRec;
                        ClearObjRec;
                    end;
                    objRec.Columns[objCol] := b;
                    Inc(objCol);
                    lastAddr := addr.SequentialAddr;
                    objRecEmpty := False;
                end;
                addr.Increment;
            except
              on E: EMemoryError do
              begin
                done := True;
              end;
              on E: Exception do
              begin
                raise;
              end;
            end;
        end;
        if (not objRecEmpty) then
            WriteObjRec;
        ClearObjRec;
        WriteXferRec;
    finally
        objFile.Free;
    end;
end;

procedure TU1005AsmForm.SaveSymbol(tag: String; addr: Integer; defined: Boolean);
var
    sym: TSymbol;
    dup: String;
begin
    if (FPass <> 1) then
        Exit;
    //
    tag := Copy(tag + '   ', 1, 3);                         // pad to 3 characters
    if (not FSymbols.TryGetValue(tag, sym)) then
    begin
        sym := TSymbol.Create;
        sym.Name := tag;
        FSymbols.Add(sym.Name, sym);
    end;
    sym.Addr := addr;
    if (defined) then
    begin
        Inc(sym.NumDefines);
        sym.Sequence := FSourceRec.Sequence;
        sym.LineNumber := FLineNumber;
        if (FLineCount > 60) then
        begin
            WriteLn(FListFile, Format('SEQ # LBL   LOC  ERR  SAAL1         %s', [FProgramID]));
            FLineCount := 1;
        end;
        dup := '   ';
        if (sym.NumDefines > 1) then
            dup := 'DUP';
        WriteLn(FListFile, Format('%-5.5s %-3.3s  %4.4d  %-3.3s',
                                  [FSourceRec.Sequence, sym.Name, sym.Addr, dup]));
    end;
end;

procedure TU1005AsmForm.Shift(op: T1005Opcode);
// Compile shift instructions. i.e. SHR m,l s
//
// I can't find anything that tells me how these instructions are
// encoded so I had to make some educated guesses.
var
    tag: String;
    offset, len, val: Integer;
    bank, row, col: Byte;
    sym: TSymbol;
    m, l: I1005Addr;
begin
    CheckBEGSeen;
    CheckSTASeen;
    if (Trim(FSourceRec.Tag) <> '') then
        SaveSymbol(FSourceRec.Tag, FCrntAddr, True);
    ParseOperand(tag, offset, len, val);
    if ((val < 1) or (val > 30)) then
        FError[2] := 'E';                   //  Illegal shift count
    if ((not LookupSymbol(tag, sym)) or (sym.NumDefines < 1)) then
        FError[2] := 'E';                   // Expression error (undefined symbol)
    m := T1005FedSysAddr.Create;
    l := T1005FedSysAddr.Create;
    m.SetAddr(sym.Addr + offset);
    l.SetAddr(sym.Addr + offset + len - 1);
    if (op.Code = 'SHR') then
    begin
        if (((sym.Addr + offset + len - 1) mod 31) <> 0) then
            FError[2] := 'E';               //  Shift operand LSB not on row boundary (col <> 31)
        // Since the L address must end on a row boundary, there is no
        // need to encode L.Col as it must be 31. That means (at least to
        // me) that the shift count must be encoded into L.Col. I have to
        // assume that it will use the same encoding as a standard column
        // address.
        l.Decode(bank, row, col);
        l.SetAddr(bank, row, val);
    end else
    begin
        if ((sym.Addr mod 31) <> 1) then
            FError[2] := 'E';               //  Shift operand MSB not on row boundary (col <> 1)
        // Since the M address must begin on a row boundary, there is no
        // need to encode M.Col as it must be 1. That means (at least to
        // me) that the shift count must be encoded into M.Col. I have to
        // assume that it will use the same encoding as a standard column
        // address.
        m.Decode(bank, row, col);
        m.SetAddr(bank, row, val);
    end;
    PrintLine(FCrntAddr, m.SequentialAddr, l.SequentialAddr, op.OpCode, m.Row, m.Col, l.Row, l.Col);
    SaveInst(op.OpCode, m.Row, m.Col, l.Row, l.Col);
end;

procedure TU1005AsmForm.ShowList(fname: String);
var
    si: TStartupInfo;
    pi: TProcessInformation;
    stat: Integer;
    msg: Pointer;
    stemp: String;
begin
    FillChar(si, SizeOf(si), 0);
    si.cb := SizeOf(si);
    if (not CreateProcess('c:\windows\system32\notepad.exe',
                          PChar('/A ' + fname),
                          nil,
                          nil,
                          True,
                          0,
                          nil,
                          nil,
                          si,
                          pi)) then
    begin
        stat := GetLastError;
        FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER or
          FORMAT_MESSAGE_FROM_SYSTEM or
          FORMAT_MESSAGE_IGNORE_INSERTS,
          nil,
          stat,
          0, // Default language
          PChar(@msg),
          0,
          nil);
        SetString(stemp, PChar(msg), StrLen(PChar(msg)));
        LocalFree(HLOCAL(msg));
        ShowMessage(stemp);
    end;
    CloseHandle(pi.hProcess);
    CloseHandle(pi.hThread);
end;

procedure TU1005AsmForm.STA;
var
    addr: I1005Addr;
    bank, row, col: Byte;
begin
    CheckBEGSeen;
    FSTASeen := True;
    // Set current address to start of next row
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(FCrntAddr);
    addr.Decode(bank, row, col);
    if (col <> 1) then
    begin
        col := 1;
        Inc(row);
        if (row > 31) then
        begin
            Inc(bank);
            if (bank > 4) then
                FError[3] := 'P';                   //  Maximum memory capacity exceeded
        end;
        addr.SetAddr(bank, row, col);
        FCrntAddr := addr.SequentialAddr;
    end;
    PrintLine(FCrntAddr);
end;

procedure TU1005AsmForm.STAR;
begin
    PrintLine;
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

{ TSourceRec }

function TSourceRec.GetCard: String;
begin
    Result := StringOf(Buffer.Card);
end;

function TSourceRec.GetComment: String;
begin
    Result := StringOf(Buffer.Comment);
end;

function TSourceRec.GetID: String;
begin
    Result := StringOf(Buffer.ID);
end;

function TSourceRec.GetLiteral: String;
begin
    Result := StringOf(Buffer.Literal);
end;

function TSourceRec.GetOpcode: String;
begin
    Result := StringOf(Buffer.Opcode);
end;

function TSourceRec.GetOperands: String;
begin
    Result := StringOf(Buffer.Operands);
end;

function TSourceRec.GetSequence: String;
begin
    Result := StringOf(Buffer.Sequence);
end;

function TSourceRec.GetTag: String;
begin
    Result := StringOf(Buffer.Tag);
end;

procedure TSourceRec.Load(val: string);
var
    i: Integer;
begin
    for i := Low(Buffer.Card) to High(Buffer.Card) do
        Buffer.Card[i] := ' ';
    i := 1;
    while ((i <= High(Buffer.Card)) and (i <= Length(val))) do
    begin
        Buffer.Card[i] := val[i];
        Inc(i);
    end;
end;

function TSourceRec.StringOf(val: array of Char): String;
var
    c: Char;
begin
    Result := '';
    for c in val do
        Result := Result + c;
end;

{ TSymbol }

constructor TSymbol.Create(nam: String; val: array of Byte);
begin
    inherited Create;
    Name := nam;
    Value[1] := val[0];
    Value[2] := val[1];
    Value[3] := val[2];
    Value[4] := val[3];
    NumDefines := 1;
end;

end.
