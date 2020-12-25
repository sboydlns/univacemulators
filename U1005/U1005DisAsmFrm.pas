unit U1005DisAsmFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, U1005Types, U1005Files, U1005Memory, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TParamRec = record
    StartAddr, EndAddr: Integer;
  end;

  TU1005DisAsmForm = class(TForm)
    Label1: TLabel;
    OkBtn: TButton;
    CancelBtn: TButton;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    SourceMemo: TMemo;
    SaveBtn: TButton;
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    SystemTypeBtn: TRadioGroup;
    procedure BrowseBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    FFedSysOpcodes: T1005OpCodeList;
    FCommOpcodes: T1005OpCodeList;
    FMemory: T1005Memory;
    FCodeStart: I1005Addr;
    FCodeEnd: I1005Addr;
    FSystemType: T1005SystemType;
    FParams: array of TParamRec;
    FParamIndex: Integer;
    procedure Clear;
    procedure ClearMem;
    procedure DisAssemble(fin: TFedSys1005ObjectFile); overload;
    procedure DisAssemble(fin: TComm1005ObjectFile); overload;
    procedure Load(fin: TFedSys1005ObjectFile); overload;
    procedure Load(fin: TComm1005ObjectFile); overload;
    procedure LoadParams;
    function Octal(n: Integer): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  U1005DisAsmForm: TU1005DisAsmForm;

implementation

uses EmulatorTypes;

{$R *.dfm}

{ TU1005DisAsmForm }

procedure TU1005DisAsmForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU1005DisAsmForm.Clear;
begin
    ClearMem;
    FreeAndNil(FCodeStart);
    FreeAndNil(FCodeEnd);
    if (FSystemType = stFedSys) then
    begin
        FCodeStart := T1005FedSysAddr.Create;
        FCodeEnd := T1005FedSysAddr.Create;
    end else
    begin
        FCodeStart := T1005CommAddr.Create;
        FCodeEnd := T1005CommAddr.Create;
    end;
    FCodeStart.Clear;
    FCodeEnd.Clear;
    SourceMemo.Lines.Clear;
end;

procedure TU1005DisAsmForm.ClearMem;
var
    bank, row, col: Byte;
begin
    FreeAndNil(FMemory);
    FMemory := T1005Memory.Create(FSystemType);
    for bank := 1 to 4 do
        for row := 1 to 32 do
            for col := 1 to 32 do
            begin
                FMemory.StoreByte(bank, row, col, 0);
            end;
end;

constructor TU1005DisAsmForm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    // Federal Systems opcode list
    FFedSysOpcodes := T1005OpCodeList.Create;
    FFedSysOpcodes.AddOpcode(OpCode(X3_SPACE, 'LA1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_APOS, 'LA2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_RIGHT_SQUARE, 'LD1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_ASTERIX, 'LD2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_0, 'LPR', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_4, 'SA1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_M, 'SA2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_SEMI, 'SD1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_AT, 'SD2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_I, 'SPR', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_F, 'SHR', itShift));
    FFedSysOpcodes.AddOpcode(OpCode(X3_PERIOD, 'SHL', itShift));
    FFedSysOpcodes.AddOpcode(OpCode(X3_1, 'CLR', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 1, 'SC', itCharacter));
    FFedSysOpcodes.AddOpcode(OpCode(X3_5, 'CA1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_N, 'CA2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_COLON, 'CN1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_PERCENT, 'CN2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_MINUS, 'IC', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 2, 'CCA', itCharacter));
    FFedSysOpcodes.AddOpcode(OpCode(X3_2, 'J', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_B, 'J2', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_7, 'J1', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_8, 'J0', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_D, 'JR', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_LEFT_SQUARE, 'JX', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 1, 'JS3', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 2, 'JOF', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_LESS, 'AM1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_GREATER, 'AM2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_SHARP, 'AR1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_LOZENGE, 'AR2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_H, 'SM1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_Y, 'SM2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_C, 'SR1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_T, 'SR2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_BACK_SLASH, 'MUL', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_G, 'DIV', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_A, 'TRL', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_O, 'SZS', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_QUESTION, 'LWS', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_3, 'LN1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_L, 'LN2', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_R, 'SED', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 3, 'LAN', itCharacter));
    FFedSysOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 4, 'LOR', itCharacter));
    FFedSysOpcodes.AddOpcode(OpCode(X3_NOT_EQUAL, 5, 'BSH', itCharacter));
    FFedSysOpcodes.AddOpcode(OpCode(X3_E, 'PTE'));
    FFedSysOpcodes.AddOpcode(OpCode(X3_AMP, 'XFC', itClass3));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 3, 'JPE', itClass2));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 4, 'JET', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 5, 'JI1', itClass1));
    FFedSysOpcodes.AddOpcode(OpCode(X3_V, 6, 'JAL', itClass1));
    // Commercial opcode list
    FCommOpcodes := T1005OpcodeList.Create;
    FCommOpcodes.AddOpcode(OpCode(X3_0, 'TA', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_4, 'TC', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_RIGHT_SQUARE, 'TD', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_2, 'TK', it2Operand, True, otConstant, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_SEMI, 'TN', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_9, 'TR', it2Operand, True, otIgnore, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_Z, 'TX', it1Operand, False, otAddress, otAddress, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_I, 'CA', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_7, 'CK', it2Operand, True, otConstant, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_F, 'CM', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_PERIOD, 'CN', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_1, 'AD', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_5, 'AM', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_AT, 'DV', it2Operand, False));
    FCommOpcodes.AddOpcode(OpCode(X3_M, 'ML', it2Operand, False));
    FCommOpcodes.AddOpcode(OpCode(X3_ASTERIX, 'MU', it2Operand, False));
    FCommOpcodes.AddOpcode(OpCode(X3_MINUS, 'SM', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_COLON, 'SU', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_6, 'AK', it2Operand, True, otConstant, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_W, 'CC', it1Operand, False, otConstant, otAddress, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_COMMA, 'EL', itEdit, False));
    FCommOpcodes.AddOpcode(OpCode(X3_G, 'ED', it2Operand, True));
    FCommOpcodes.AddOpcode(OpCode(X3_LOZENGE, 'SC', it1Operand, False, otIgnore, otOctal, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_X, 'JC', it1Operand, False, otOctal, otAddress, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_LEFT_PAREN, 'JK', it2Operand, False, otConstant, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_J, 'JL', it2Operand, False, otConstant, otAddress, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_N, 'JR', it2Operand, False));
    FCommOpcodes.AddOpcode(OpCode(X3_Y, 'JT', it1Operand, False, otAddress, otAddress, otIgnore));
    FCommOpcodes.AddOpcode(OpCode(X3_SLASH, 'J', it1Operand, False, otIgnore, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_RIGHT_SQUARE, 'JI', it2Operand));
    FCommOpcodes.AddOpcode(OpCode(X3_PERCENT, 'GC', it2Operand, False, otOctal, otOctal, otOctal));
    FCommOpcodes.AddOpcode(OpCode(X3_DOLLAR, 'RT', it2Operand));
    FCommOpcodes.AddOpcode(OpCode(X3_K, 'WT', it2Operand));
    FCommOpcodes.AddOpcode(OpCode(X3_P, 'RD', it2Operand));
    FCommOpcodes.AddOpcode(OpCode(X3_S, 'SD', it2Operand));
    FCommOpcodes.AddOpcode(OpCode(X3_Q, 'RF', it2Operand, False, otOctal, otAddress, otAddress));
    FCommOpcodes.AddOpcode(OpCode(X3_U, 'SF', it2Operand, False, otOctal, otAddress, otAddress));
end;

destructor TU1005DisAsmForm.Destroy;
begin
    FreeAndNil(FFedSysOpcodes);
    FreeAndNil(FCommOpcodes);
    FreeAndNil(FMemory);
    inherited Destroy;
end;

procedure TU1005DisAsmForm.DisAssemble(fin: TFedSys1005ObjectFile);
var
    bank, row, col: Byte;
    op, op2: Byte;
    lastAddr, curAddr, eaddr: Integer;
    finished: Boolean;
    opcode: T1005OpCode;
    addr: I1005Addr;

    procedure SkipBlanks;
    var
        done: Boolean;
    // Skip until we find non-zero memory location
    begin
        done := False;
        while (not done) do
        begin
            if (FMemory.FetchByte(bank, row, col) <> 0) then
            begin
                done := True;
            end else
            begin
                Inc(col);
                if (col > 32) then
                begin
                    col := 1;
                    Inc(row);
                    if (row > 32) then
                    begin
                        row := 1;
                        Inc(bank);
                        if (bank > 4) then
                            done := True;
                    end;
                end;
            end;
        end;
    end;

    procedure Class1(opcode: T1005OpCode);
    var
        op: T1005FedSysOperand;
        mrow, mcol, lrow, lcol: Byte;
        laddr, maddr, len: Integer;
        stemp: String;
    begin
        mrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        mcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        op := FedSysOperand(mrow, mcol, lrow, lcol);
        laddr := op.L.SequentialAddr;
        maddr := op.M.SequentialAddr;
        len := laddr - maddr + 1;
        stemp := Format('          %-3.3s %4.4d,%d', [opcode.Code, maddr, len]);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) +
                         Char(TCodeTranslator.XS3ToAscii(opcode.OpCode)) +
                         Char(TCodeTranslator.XS3ToAscii(mrow)) +
                         Char(TCodeTranslator.XS3ToAscii(mcol)) +
                         Char(TCodeTranslator.XS3ToAscii(lrow)) +
                         Char(TCodeTranslator.XS3ToAscii(lcol));
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Class2(opcode: T1005OpCode);
    var
        l: I1005Addr;
        m: I1005Addr;
        maddr: Integer;
        stemp: String;
        nrow: Byte;
        ncol: Byte;
    begin
        l := T1005FedSysAddr.Create;
        m := T1005FedSysAddr.Create;
        nrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        ncol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        m.SetAddr(nrow, ncol);
        maddr := m.SequentialAddr;
        nrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        ncol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        l.SetAddr(nrow, ncol);
        stemp := Format('          %-3.3s %4.4d', [opcode.Code, maddr]);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) +
                         Char(TCodeTranslator.XS3ToAscii(opcode.OpCode)) +
                         Char(TCodeTranslator.XS3ToAscii(m.Row)) +
                         Char(TCodeTranslator.XS3ToAscii(m.Col)) +
                         Char(TCodeTranslator.XS3ToAscii(l.Row)) +
                         Char(TCodeTranslator.XS3ToAscii(l.Col));
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Class3(opcode: T1005OpCode);
    var
        operand: String;
        stemp: String;
    begin
        operand := Char(TCodeTranslator.XS3ToAscii(FMemory.FetchByte(bank, row, col)));
        Inc(col);
        operand := operand + Char(TCodeTranslator.XS3ToAscii(FMemory.FetchByte(bank, row, col)));
        Inc(col);
        operand := operand + Char(TCodeTranslator.XS3ToAscii(FMemory.FetchByte(bank, row, col)));
        Inc(col);
        operand := operand + Char(TCodeTranslator.XS3ToAscii(FMemory.FetchByte(bank, row, col)));
        Inc(col);
        stemp := Format('          %-3.3s %-4.4s', [opcode.Code, operand]);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) + operand;
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Shift(opcode: T1005OpCode);
    var
        op: T1005FedSysOperand;
        mrow, mcol, lrow, lcol: Byte;
        laddr, maddr, count, len: Integer;
        abank, arow, acol: Byte;
        stemp: String;
    begin
        mrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        mcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        op := FedSysOperand(mrow, mcol, lrow, lcol);
        if (opcode.Code = 'SHR') then
        begin
            maddr := op.M.SequentialAddr;
            op.L.Decode(abank, arow, acol);
            count := acol;
            op.L.SetAddr(abank, arow, 31);
            laddr := op.L.SequentialAddr;
        end else
        begin
            op.M.Decode(abank, arow, acol);
            count := acol;
            op.M.SetAddr(abank, arow, 1);
            maddr := op.M.SequentialAddr;
            laddr := op.L.SequentialAddr;
        end;
        len := laddr - maddr + 1;
        stemp := Format('          %-3.3s %4.4d,%d %d', [opcode.Code, maddr, len, count]);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) +
                         Char(TCodeTranslator.XS3ToAscii(opcode.OpCode)) +
                         Char(TCodeTranslator.XS3ToAscii(mrow)) +
                         Char(TCodeTranslator.XS3ToAscii(mcol)) +
                         Char(TCodeTranslator.XS3ToAscii(lrow)) +
                         Char(TCodeTranslator.XS3ToAscii(lcol));
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Character(opcode: T1005OpCode);
    var
        op: T1005FedSysOperand;
        mrow, mcol, lrow, lcol: Byte;
        laddr: Integer;
        stemp: String;
        val: String;
    begin
        mrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        mcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lrow := FMemory.FetchByte(bank, row, col);
        Inc(col);
        lcol := FMemory.FetchByte(bank, row, col);
        Inc(col);
        op := FedSysOperand(mrow, mcol, lrow, lcol);
        laddr := op.L.SequentialAddr;
        val := Char(TCodeTranslator.XS3ToAscii(op.M.Row));
        stemp := Format('          %-3.3s %4.4d,%d %s', [opcode.Code, laddr, 1, val]);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) +
                         Char(TCodeTranslator.XS3ToAscii(opcode.OpCode)) +
                         Char(TCodeTranslator.XS3ToAscii(mrow)) +
                         Char(TCodeTranslator.XS3ToAscii(mcol)) +
                         Char(TCodeTranslator.XS3ToAscii(lrow)) +
                         Char(TCodeTranslator.XS3ToAscii(lcol));
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Unknown(opcode: T1005OpCode);
    var
        stemp: String;
        b1, b2, b3, b4: Byte;
    begin
        b1 := FMemory.FetchByte(bank, row, col);
        Inc(col);
        b2 := FMemory.FetchByte(bank, row, col);
        Inc(col);
        b3 := FMemory.FetchByte(bank, row, col);
        Inc(col);
        b4 := FMemory.FetchByte(bank, row, col);
        Inc(col);
        stemp := stemp + StringOfChar(' ', 32 - Length(stemp)) +
                         Char(TCodeTranslator.XS3ToAscii(opcode.OpCode)) +
                         Char(TCodeTranslator.XS3ToAscii(b1)) +
                         Char(TCodeTranslator.XS3ToAscii(b2)) +
                         Char(TCodeTranslator.XS3ToAscii(b3)) +
                         Char(TCodeTranslator.XS3ToAscii(b4));
        SourceMemo.Lines.Add(stemp);
    end;

    procedure Literal;
    var
        stemp: String;
        b: Byte;
        count: Integer;
    begin
        stemp := '';
        count := 0;
        while ((count < 5) and (col <= 31)) do
        begin
            b := FMemory.FetchByte(bank, row, col);
            stemp := stemp + Char(TCodeTranslator.XS3ToAscii(b));
            Inc(col);
            Inc(count);
            Inc(curAddr);
        end;
        SourceMemo.Lines.Add(Format('          +%d  %s', [Length(stemp), stemp]));
    end;

begin
    addr := T1005FedSysAddr.Create;
    SourceMemo.Lines.Add('          BEG');
    lastAddr := -1;
    eaddr := FCodeEnd.SequentialAddr;
    FCodeStart.Decode(bank, row, col);
    finished := False;
    while (not finished) do
    begin
        addr.SetAddr(bank, row, col);
        curAddr := addr.SequentialAddr;
        if (curAddr <> lastAddr) then
            SourceMemo.Lines.Add(Format('          ORG %4.4d', [curAddr]));
        if ((col < 31) and ((col mod 5) = 1)) then
        begin
            // This could be an instruction
            op := FMemory.FetchByte(bank, row, col);
            // Certain opcodes require a second opcode byte
            if (op = X3_V) then
                op2 := FMemory.FetchByte(bank, row, col + 4)
            else if (op = X3_NOT_EQUAL) then
                op2 := FMemory.FetchByte(bank, row, col + 2)
            else
                op2 := 0;
            if (FFedSysOpcodes.IsOpcode(op, op2)) then
            begin
                Inc(col);
                opcode := FFedSysOpcodes.FindOpcode(op, op2);
                case opcode.InstType of
                  itClass1:     Class1(opcode);
                  itClass2:     Class2(opcode);
                  itClass3:     Class3(opcode);
                  itShift:      Shift(opcode);
                  itCharacter:  Character(opcode);
                  else          Unknown(opcode);
                end;
                Inc(curAddr, 5);
            end else
            begin
                Literal;
            end;
        end else
        begin
            Literal;
        end;
        lastAddr := curAddr;
        if (curAddr > eaddr) then
            finished := True;
        if (col > 31) then
        begin
            col := 1;
            Inc(row);
            if (row > 31) then
            begin
                row := 1;
                Inc(bank);
            end;
        end;
    end;
    ClearMem;
    FCodeStart.Clear;
    FCodeEnd.Clear;
end;

procedure TU1005DisAsmForm.DisAssemble(fin: TComm1005ObjectFile);
var
    curAddr, startAddr, eaddr, lastAddr: Integer;
    op: Byte;
    addr: I1005Addr;
    opcode: T1005Opcode;

    procedure Literal(len: Integer);
    var
        startAddr: Integer;
        s: AnsiString;
        count: Integer;
    begin
        startAddr := curAddr;
        s:= '';
        count := 0;
        while (len > 0) do
        begin
            if (count >= 32) then
            begin
                SourceMemo.Lines.Add(Format('%4.4d:             *     %2.2d     %s',
                                            [startAddr, count, s]));
                s := '';
                count := 0;
                startAddr := curAddr;
            end;
            addr.SetAddr(curAddr);
            s := s + TCodeTransLator.XS3ToAscii(FMemory.FetchByte(addr));
            Inc(curAddr);
            Inc(count);
            Dec(len);
        end;
        if (s <> '') then
            SourceMemo.Lines.Add(Format('%4.4d:             *     %2.2d     %s',
                                        [startAddr, count, s]));
    end;

    procedure Operand1(opcode: T1005OpCode);
    var
        b1, b2: Byte;
        code: array [1..5] of Byte;
        opAddr: Integer;
        flda, fldb: I1005Addr;
        txta, txtb: String;
    begin
        if ((FParamIndex <= High(FParams)) and
            ((curAddr + 5) > FParams[FParamIndex].StartAddr) and
            (curAddr <= FParams[FParamIndex].EndAddr)) then
        begin
            Literal(FParams[FParamIndex].EndAddr - curAddr + 1);
            Inc(FParamIndex);
            Exit;
        end;
        if ((curAddr < startAddr) and ((startAddr - curAddr) < 5)) then
        begin
            Literal(startAddr - curAddr);
            Exit;
        end;

        flda := T1005CommAddr.Create;
        fldb := T1005CommAddr.Create;
        opAddr := curAddr;
        addr.SetAddr(curAddr);
        code[1] := FMemory.FetchByte(addr);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[2] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[3] := b2;
        flda.SetAddr(b1, b2);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[4] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[5] := b2;
        fldb.SetAddr(b1, b2);
        case opcode.FldA of
          otAddress:
            txta := Format('%s%4.4d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), flda.SequentialAddr]);
          otConstant:
            txta := String(TCodeTranslator.XS3ToAscii(flda.Row) + TCodeTranslator.XS3ToAscii(flda.Col) + '   ');
          otOctal:
            txta := '#' + octal((flda.Row shl 6) + flda.Col);
          otIgnore:
            txta := '';
        end;
        case opcode.FldB of
          otAddress:
            txtb := Format('%s%4.4d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), fldb.SequentialAddr]);
          otConstant:
            txtb := String(TCodeTranslator.XS3ToAscii(fldb.Row) + TCodeTranslator.XS3ToAscii(fldb.Col) + '   ');
          otOctal:
            txtb := '#' + octal((fldb.Row shl 6) + fldb.Col);
          otIgnore:
            txtb := '';
        end;
        SourceMemo.Lines.Add(Format('%4.4d: %-7.7s     %-2.2s    %5.5s     %5.5s',
                                    [opAddr, TCodeTranslator.XS3ToAscii(code), opcode.Code, txta, txtb]));
        Inc(curAddr);
    end;

    procedure Operand2(opcode: T1005OpCode);
    var
        b1, b2: Byte;
        asterix: Char;
        code: array [1..7] of Byte;
        opAddr: Integer;
        flda, fldb, fldc: I1005Addr;
        txta, txtb, txtc: String;
        row, col, bank: Byte;
    begin
        if ((FParamIndex <= High(FParams)) and
            ((curAddr + 7) > FParams[FParamIndex].StartAddr) and
            (curAddr <= FParams[FParamIndex].EndAddr)) then
        begin
            Literal(FParams[FParamIndex].EndAddr - curAddr + 1);
            Inc(FParamIndex);
            Exit;
        end;
        if ((curAddr < startAddr) and ((startAddr - curAddr) < 7)) then
        begin
            Literal(startAddr - curAddr);
            Exit;
        end;

        flda := T1005CommAddr.Create;
        fldb := T1005CommAddr.Create;
        fldc := T1005CommAddr.Create;
        opAddr := curAddr;
        addr.SetAddr(curAddr);
        code[1] := FMemory.FetchByte(addr);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[2] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[3] := b2;
        flda.SetAddr(b1, b2);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[4] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[5] := b2;
        fldb.SetAddr(b1, b2);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[6] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[7] := b2;
        fldc.SetAddr(b1, b2);
        if (opcode.Indirect and ((fldc.Row and $20) <> 0)) then
            asterix := '*'
        else
            asterix := ' ';
        case opcode.FldA of
          otAddress:
          begin
            flda.Decode(bank, row, col);
            if ((row > 31) or (col > 31)) then
                txta := Format('%s$%2.2d%2.2dB%d', [asterix, row, col, bank])
            else
                txta := Format('%s%s%4.4d', [asterix, TCodeTranslator.XS3ToAscii(X3_LOZENGE), flda.SequentialAddr]);
          end;
          otConstant:
            txta := ' ' + String(TCodeTranslator.XS3ToAscii(flda.Row) + TCodeTranslator.XS3ToAscii(flda.Col));
          otOctal:
            txta := ' #' + octal((flda.Row shl 6) + flda.Col);
          otIgnore:
            txta := '';
        end;
        if (opcode.Indirect and ((fldc.Col and $20) <> 0)) then
            asterix := '*'
        else
            asterix := ' ';
        case opcode.FldB of
          otAddress:
          begin
            fldb.Decode(bank, row, col);
            if ((row > 31) or (col > 31)) then
                txtb := Format('%s$%2.2d%2.2dB%d', [asterix, row, col, bank])
            else
                txtb := Format('%s%s%4.4d', [asterix, TCodeTranslator.XS3ToAscii(X3_LOZENGE), fldb.SequentialAddr]);
          end;
          otConstant:
            txtb := ' ' + String(TCodeTranslator.XS3ToAscii(fldb.Row) + TCodeTranslator.XS3ToAscii(fldb.Col));
          otOctal:
            txtb := ' #' + octal((fldb.Row shl 6) + fldb.Col);
          otIgnore:
            txtb := '';
        end;
        case opcode.FldC of
          otAddress:
          begin
            if (opcode.Code <> 'JR') then
                fldc.AdjustBank(fldb);
            fldc.Decode(bank, row, col);
            if ((row > 31) or (col > 31)) then
                txtc := Format('$%2.2d%2.2dB%d', [row, col, bank])
            else
                txtc := Format('%s%4.4d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), fldc.SequentialAddr]);
          end;
          otConstant:
            txtc := String(TCodeTranslator.XS3ToAscii(fldc.Row) + TCodeTranslator.XS3ToAscii(fldc.Col));
          otOctal:
            txtc := '#' + octal((fldc.Row shl 6) + fldc.Col);
          otIgnore:
            txtc := '';
        end;
        SourceMemo.Lines.Add(Format('%4.4d: %7.7s     %-2.2s   %-8.8s  %-8.8s   %-8.8s',
                                    [opAddr, TCodeTranslator.XS3ToAscii(code), opcode.Code, txta, txtb, txtc]));
        Inc(curAddr);
    end;

    procedure Edit(opcode: T1005OpCode);
    var
        b1, b2: Byte;
        code: array [1..7] of Byte;
        opAddr: Integer;
        flda, fldb, fldc: I1005Addr;
        txta, txtb, txtc: String;
    begin
        if ((FParamIndex <= High(FParams)) and
            ((curAddr + 7) > FParams[FParamIndex].StartAddr) and
            (curAddr <= FParams[FParamIndex].EndAddr)) then
        begin
            Literal(FParams[FParamIndex].EndAddr - curAddr + 1);
            Inc(FParamIndex);
            Exit;
        end;
        if ((curAddr < startAddr) and ((startAddr - curAddr) < 7)) then
        begin
            Literal(startAddr - curAddr);
            Exit;
        end;

        flda := T1005CommAddr.Create;
        fldb := T1005CommAddr.Create;
        fldc := T1005CommAddr.Create;
        opAddr := curAddr;
        addr.SetAddr(curAddr);
        code[1] := FMemory.FetchByte(addr);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[2] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[3] := b2;
        flda.SetAddr(b1, b2);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[4] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[5] := b2;
        fldb.SetAddr(b1, b2);
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b1 := FMemory.FetchByte(addr);
        code[6] := b1;
        Inc(curAddr);
        addr.SetAddr(curAddr);
        b2 := FMemory.FetchByte(addr);
        code[7] := b2;
        fldc.SetAddr(b1, b2);
        Inc(curAddr, 1);
        txta := String(TCodeTranslator.XS3ToAscii(flda.Row) + TCodeTranslator.XS3ToAscii(flda.Col) + '   ');
        txtb := Format('%s%s%4.4d', [' ', TCodeTranslator.XS3ToAscii(X3_LOZENGE), fldb.SequentialAddr]);
        txtc := Format('%s%4.4d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), fldc.SequentialAddr]);
        SourceMemo.Lines.Add(Format('%4.4d: %7.7s     %-2.2s   %6.6s    %6.6s    %5.5s',
                                    [opAddr, TCodeTranslator.XS3ToAscii(code), 'EL', txta, txtb, txtc]));
    end;

    procedure Unknown(opcode: T1005OpCode);
    begin
        raise Exception.Create('OOPS! Unknown instruction type');
    end;

begin
    addr := T1005CommAddr.Create;
    lastAddr := -1;
    addr.Assign(fin.StartAddr);
    startAddr := addr.SequentialAddr;
    curAddr := FCodeStart.SequentialAddr;
    eaddr := FCodeEnd.SequentialAddr;
    SourceMemo.Lines.Add(Format('                  DL    %s%d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), curAddr]));
    while (curAddr <= eaddr) do
    begin
        addr.SetAddr(curAddr);
        op := FMemory.FetchByte(addr);
        if (FCommOpcodes.IsOpcode(op, 0)) then
        begin
            opcode := FCommOpcodes.FindOpcode(op, 0);
            case opcode.InstType of
              it1Operand:   Operand1(opcode);
              it2Operand:   Operand2(opcode);
              itEdit:       Edit(opcode);
              else          Unknown(opcode);
            end;
        end else
        begin
            if ((FParamIndex <= High(FParams)) and
                (curAddr >= FParams[FParamIndex].StartAddr) and
                (curAddr <= FParams[FParamIndex].EndAddr)) then
            begin
                Literal(FParams[FParamIndex].EndAddr - curAddr + 1);
                Inc(FParamIndex);
            end else
                Literal(1);
        end;
    end;
    SourceMemo.Lines.Add(Format('                  END   %s%d', [TCodeTranslator.XS3ToAscii(X3_LOZENGE), startAddr]));
    ClearMem;
    FCodeStart.Clear;
    FCodeEnd.Clear;
end;

procedure TU1005DisAsmForm.Load(fin: TFedSys1005ObjectFile);
var
    i: Integer;
    len: Integer;
    bank, row, col: Byte;
    op: T1005FedSysOperand;
begin
    i := 1;
    op := FedSysOperand(fin.StartAddr, fin.EndAddr);
    if ((FCodeEnd.Row = 0) and (FCodeEnd.Col = 0)) then
    begin
        FCodeStart.Assign(op.M);
        FCodeEnd.Assign(op.L);
    end;
    FCodeEnd.Assign(op.L);

    len := op.L.SequentialAddr - op.M.SequentialAddr;
    if (len < 0) then
    begin
        // Look like end address is in the next bank
        op.L.Decode(bank, row, col);
        Inc(bank);
        op.L.SetAddr(bank, row, col);
    end;

    while (op.M.Compare(op.L) <= 0) do
    begin
        FMemory.StoreByte(op.M, fin.Buffer.Columns[i]);
        op.M.Increment;
        Inc(i);
    end;
end;

procedure TU1005DisAsmForm.Load(fin: TComm1005ObjectFile);
var
    fromAddr, saddr, eaddr: I1005Addr;
    i: Integer;
begin
    fromAddr := fin.FromAddr;
    saddr := fin.StartAddr;
    eaddr := fin.EndAddr;
    eaddr.AdjustBank(saddr);
    if ((FCodeEnd.Row = 0) and (FCodeEnd.Col = 0)) then
    begin
        FCodeStart.Assign(saddr);
        FCodeEnd.Assign(eaddr);
    end else
    begin
        if (saddr.Compare(FCodeStart) < 0) then
            FCodeStart.Assign(saddr);
        if (eaddr.Compare(FCodeEnd) > 0) then
            FCodeEnd.Assign(eaddr);
    end;
    i := fromAddr.SequentialAddr;
    while (not saddr.Matches(eaddr)) do
    begin
        FMemory.StoreByte(saddr, fin.Buffer.Columns[i]);
        saddr.Increment;
        Inc(i);
    end;
    FMemory.StoreByte(saddr, fin.Buffer.Columns[i]);
end;

procedure TU1005DisAsmForm.LoadParams;
var
    fin: TFileStream;
    c: Char;
    s: String;
    split: Integer;
    fname: String;
    p: TStringList;
begin
    SetLength(FParams, 0);
    FParamIndex := 0;
    p := TStringList.Create;
    try
        try
            split := Pos('.', FileNameEdt.Text);
            if (split > 0) then
                fname := Copy(FileNameEdt.Text, 1, split - 1) + '.dis'
            else
                fname := FileNameEdt.Text + '.dis';
            fin := TFileStream.Create(fname, fmOpenRead or fmShareDenyNone);
            while (fin.Read(c, 1) > 0) do
            begin
                case c of
                  #13:
                    ;
                  #10:
                  begin
                    if (s <> '') then
                    begin
                        if (s[1] <> '#') then
                        begin
                            p.CommaText := s;
                            SetLength(FParams, Length(FParams) + 1);
                            if (not TryStrToInt(p[0], FParams[High(FParams)].StartAddr)) then
                                FParams[High(FParams)].StartAddr := 0;
                            if (not TryStrToInt(p[1], FParams[High(FParams)].EndAddr)) then
                                FParams[High(FParams)].EndAddr := 0;
                            s := '';
                        end;
                    end;
                  end;
                  else
                    s := s + c;
                end;
            end;
        except
            ;
        end;
    finally
        p.Free;
    end;
end;

function TU1005DisAsmForm.Octal(n: Integer): String;
var
    i: Integer;
begin
    Result := '';
    for i := 1 to 4 do
    begin
        Result := Chr((n mod 8 ) + Ord('0')) + Result;
        n := n div 8;
    end;
end;

procedure TU1005DisAsmForm.OkBtnClick(Sender: TObject);
var
    fedSysIn: TFedSys1005ObjectFile;
    commIn: TComm1005ObjectFile;
begin
    if (FileNameEdt.Text = '') then
    begin
        FileNameEdt.SetFocus;
        raise Exception.Create('Please enter the object file name');
    end;

    if (SystemTypeBtn.ItemIndex = 0) then
    begin
        // Federal Systems (military) version
        FSystemType := stFedSys;
        fedSysIn := TFedSys1005ObjectFile.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
        try
            Clear;
            LoadParams;
            while (not fedSysIn.Eof) do
            begin
                fedSysIn.Read;
                case fedSysIn.CardType of
                  X3_RIGHT_SQUARE:
                    Load(fedSysIn);
                  X3_SLASH:
                    DisAssemble(fedSysIn);
                end;
            end;
        finally
            fedSysIn.Free;
        end;
    end else
    begin
        FSystemType := stComm;
        commIn := TComm1005ObjectFile.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
        try
            Clear;
            LoadParams;
            while (not commIn.Eof) do
            begin
                commIn.Read;
                case commIn.CardType of
                  X3_RIGHT_SQUARE:
                    Load(commIn);
                  X3_SLASH:
                    DisAssemble(commIn);
                end;
            end;
        finally
            commIn.Free;
        end;
    end;
end;

end.
