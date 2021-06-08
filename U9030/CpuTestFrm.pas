unit CpuTestFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, U9030Types;

type
  TCpuTestForm = class(TForm)
    Label1: TLabel;
    ScriptEdt: TEdit;
    BrowseBtn: TButton;
    ResultsMemo: TMemo;
    OpenDlg: TOpenDialog;
    ExecBtn: TButton;
    procedure BrowseBtnClick(Sender: TObject);
    procedure ExecBtnClick(Sender: TObject);
  private
    FScriptFile: TFileStream;
    FScriptLine: String;
    FScanStart: Integer;
    procedure CloseScript;
    procedure DoAsm;
    procedure DoSet;
    procedure DoTest;
    procedure ExecScript;
    procedure GetAddress(var addr: TMemoryAddress; var len: Integer);
    function GetInteger: TDblWord;
    function GetRegister(tkn: String): Integer;
    function GetToken: String;
    procedure OpenScript;
    function ReadScript: AnsiString;
  public
    { Public declarations }
  end;

var
  CpuTestForm: TCpuTestForm;

implementation

uses Globals;

{$R *.dfm}

procedure TCpuTestForm.BrowseBtnClick(Sender: TObject);
begin
    if (not OpenDlg.Execute) then
        Exit;
    ScriptEdt.Text := OpenDlg.FileName;
end;

procedure TCpuTestForm.CloseScript;
begin
    FreeAndNil(FScriptFile);
end;

procedure TCpuTestForm.DoAsm;
var
    err, mnemonic: String;
    op: TOpcode;

    function Reg(tkn: String): Integer;
    begin
        if (tkn = '') then
            raise Exception.Create('Syntax error');
        if (tkn[1] = 'R') then
            Result := GetRegister(tkn)
        else
        begin
            if (not TryStrToInt(tkn, Result)) then
                raise Exception.Create('Invalid register number');
        end;
        if ((Result < 0) or (Result > 15)) then
            raise Exception.Create('Invalid register number');
    end;

    function Offset: Integer;
    var
        tkn: String;
    begin
        // Skip the comma, if present
        tkn := GetToken;
        if (tkn = ',') then
            tkn := GetToken;
        if (not TryStrToInt(tkn, Result)) then
            raise Exception.Create('Syntax error');
    end;

    function Immed: Byte;
    var
        tkn: String;
        i: Integer;
    begin
        tkn := GetToken;
        if (tkn = ',') then
            tkn := GetToken;
        if (not TryStrToInt(tkn, i)) then
            raise Exception.Create('Syntax error');
        Result := Byte(i);
    end;

    procedure BaseRef(var o, b: Integer);
    var
        tkn: String;
    begin
        o := 0;
        b := 0;
        o := Offset;
        tkn := GetToken;
        if (tkn = '(') then
            tkn := GetToken;
        if ((tkn = '') or (tkn = ',')) then
            Exit;
        b := Reg(tkn);
    end;

    procedure IndexRef(var o, x, b: Integer);
    var
        tkn: String;
    begin
        o := 0;
        x := 0;
        b := 0;
        o := Offset;
        tkn := GetToken;
        if (tkn = '(') then
            tkn := GetToken;
        if (tkn = '') then
            Exit;
        if (tkn = ',') then
        begin
            tkn := GetToken;
            b := Reg(tkn);
            Exit;
        end else
        begin
            x := Reg(tkn);
            tkn := GetToken;
            if (tkn = ',') then
            begin
                tkn := GetToken;
                b := Reg(tkn);
            end else if (tkn <> ')') then
                raise Exception.Create('Syntax error');
        end;
    end;

    procedure DoRX;
    // op r,o(x,b)
    var
        tkn: String;
        r, o, x, b: Integer;
    begin
        tkn := GetToken;
        r := Reg(tkn);
        IndexRef(o, x, b);
        Core.StoreByte(0, 0, op.Opcode);
        Core.StoreByte(0, 1, (r shl 4) or x);
        Core.StoreByte(0, 2, (b shr 4) or ((o shr 8) and $0f));
        Core.StoreByte(0, 3, o and $ff);
        try
            Processor.Test;
        except
          on E: Exception do
          begin
              raise Exception.CreateFmt('Failed! %s', [E.Message]);
          end;
        end;
    end;

    procedure DoSI;
    // op o(b),i
    var
        i: Byte;
        o, b: Integer;
    begin
        BaseRef(o, b);
        i := Immed;
        Core.StoreByte(0, 0, op.Opcode);
        Core.StoreByte(0, 1, i);
        Core.StoreByte(0, 2, (b shr 4) or ((o shr 8) and $0f));
        Core.StoreByte(0, 3, o and $ff);
        try
            Processor.Test;
        except
          on E: Exception do
          begin
              raise Exception.CreateFmt('Failed! %s', [E.Message]);
          end;
        end;
    end;

    procedure DoRR;
    // op r1,r2
    var
        tkn: String;
        r1, r2: Integer;
    begin
        tkn := GetToken;
        r1 := Reg(tkn);
        tkn := GetToken;
        if (tkn = ',') then
            tkn := GetToken;
        r2 := Reg(tkn);
        Core.StoreByte(0, 0, op.Opcode);
        Core.StoreByte(0, 1, (r1 shl 4) or (r2 and $0f));
        try
            Processor.Test;
        except
          on E: Exception do
          begin
              raise Exception.CreateFmt('Failed! %s', [E.Message]);
          end;
        end;
    end;

    procedure DoRS;
    // op r1,o(b)
    var
        tkn: String;
        r, o, b: Integer;
    begin
        tkn := GetToken;
        r := Reg(tkn);
        BaseRef(o, b);
        Core.StoreByte(0, 0, op.Opcode);
        Core.StoreByte(0, 1, r shl 4);
        Core.StoreByte(0, 2, (b shr 4) or ((o shr 8) and $0f));
        Core.StoreByte(0, 3, o and $ff);
        try
            Processor.Test;
        except
          on E: Exception do
          begin
              raise Exception.CreateFmt('Failed! %s', [E.Message]);
          end;
        end;
    end;

begin
    err := '';
    try
        FScanStart := 1;                            // Reset to begining of line
        mnemonic := GetToken;                       // Get the instruction mnemonic
        op := Opcodes.FindOpcode(mnemonic);         // Make sure it is valid
        case op.InstType of
          itDirective: ;
          itRR:
            DoRR;
          itRS:
            DoRS;
          itRX:
            DoRX;
          itSI:
            DoSI ;
          itSS1: ;
          itSS2: ;
          itBranch:
            raise Exception.Create('Testing of branch instructions not supported');
        end;
    except
      on E: Exception do
      begin
        err := '  ****  ' + E.Message;
      end;
    end;
    ResultsMemo.Text := ResultsMemo.Text + FScriptLine + err + #13#10;
end;

procedure TCpuTestForm.DoSet;
var
    tkn, err: String;

    procedure SetReg;
    var
        r: Integer;
    begin
        r := GetRegister(tkn);
        Processor.Registers[PSW.RegisterSet, r] := GetInteger;
    end;

    procedure SetMem;
    var
        len: Integer;
        addr: TMemoryAddress;
        val: TDblWord;
    begin
        GetAddress(addr, len);
        if (len <> 0) then
        begin
            val := GetInteger;
            case len of
              1:    Core.StoreByte(0, addr, val);
              2:    Core.StoreHalfWord(0, addr, val);
              4:    Core.StoreWord(0, addr, val);
              8:    Core.StoreDblWord(0, addr, val);
            end;
        end else
        begin
            raise Exception.Create('Arbitrary length arguments not supported yet');
        end;
    end;

begin
    err := '';
    try
        tkn := GetToken;
        if (tkn = '') then
            raise Exception.Create('Missing argument')
        else if (tkn[1] = 'R') then
            SetReg
        else if (tkn = 'MEM') then
            SetMem
        else
            raise Exception.Create('Invalid argument');
    except
      on E: Exception do
      begin
        err := '  ****  ' + E.Message;
      end;
    end;
    ResultsMemo.Text := ResultsMemo.Text + FScriptLine + err + #13#10;
end;

procedure TCpuTestForm.DoTest;
var
    tkn, err: String;

    procedure TestReg;
    var
        r: Integer;
        val, test: TWord;
    begin
        r := GetRegister(tkn);
        val := GetInteger;
        test := Processor.Registers[PSW.RegisterSet, r];
        if (test <> val) then
            raise Exception.CreateFmt('Failed! R%d = %d (%8.8x)', [r, test, test]);
    end;

    procedure TestMem;
    var
        len: Integer;
        addr: TMemoryAddress;
        val, test: TDblWord;
        testOK: Boolean;
    begin
        GetAddress(addr, len);
        if (len <> 0) then
        begin
            val := GetInteger;
            case len of
              1:
              begin
                test := Core.FetchByte(0, addr);
                testOK := Byte(val) = Byte(test);
              end;
              2:
              begin
                test := Core.FetchHalfWord(0, addr);
                testOK := THalfWord(val) = THalfWord(test);
              end;
              4:
              begin
                test := Core.FetchWord(0, addr);
                testOK := TWord(val) = TWord(test);
              end;
              8:
              begin
                test := Core.FetchDblWord(0, addr);
                testOK := val = test;
              end
              else
              begin
                test := 0;
                testOK := False;
              end;
            end;
            if (not testOk) then
            begin
                len := len * 2;
                raise Exception.CreateFmt('Failed! MEM = %d (%*.*x)', [test, len, len, test]);
            end;

        end else
        begin
            raise Exception.Create('Arbitrary length arguments not supported yet');
        end;
    end;

    procedure TestCC;
    var
        val: TDblWord;
    begin
        val := GetInteger;
        if (val <> PSW.CondCode) then
            raise Exception.CreateFmt('Failed! CC = %d', [PSW.CondCode]);
    end;

begin
    err := '  Passed';
    try
        tkn := GetToken;
        if (tkn = '') then
            raise Exception.Create('Missing argument')
        else if (tkn[1] = 'R') then
            TestReg
        else if (tkn = 'MEM') then
            TestMem
        else if (tkn = 'CC') then
            TestCC
        else
            raise Exception.Create('Invalid argument');
    except
      on E: Exception do
      begin
        err := '  ****  ' + E.Message;
      end;
    end;
    ResultsMemo.Text := ResultsMemo.Text + FScriptLine + err + #13#10;
end;

procedure TCpuTestForm.ExecBtnClick(Sender: TObject);
begin
    OpenScript;
    try
        ExecScript;
    finally
        CloseScript;
    end;
end;

procedure TCpuTestForm.ExecScript;
var
    line: AnsiString;
    tkn: String;
begin
    ResultsMemo.Clear;
    line := ReadScript;
    while (line <> '** EOF **') do
    begin
        FScriptLine := UpperCase(String(line));
        FScanStart := 1;
        tkn := GetToken;
        if (tkn = '') then
        begin
            ResultsMemo.Text := ResultsMemo.Text + FScriptLine + #13#10;
        end else if (tkn[1] = '#') then
        begin
            ResultsMemo.Text := ResultsMemo.Text + FScriptLine + #13#10;
        end else if (tkn = 'SET') then
            DoSet
        else if (tkn = 'TEST') then
            DoTest
        else
            DoAsm;
        line := ReadScript;
    end;
end;

procedure TCpuTestForm.GetAddress(var addr: TMemoryAddress; var len: Integer);
var
    tkn, s: String;
begin
    tkn := GetToken;
    // Get length code
    len := 0;
    s := Copy(tkn, Length(tkn));
    if (s = 'B') then
        len := 1
    else if (s = 'H') then
        len := 2
    else if (s = 'W') then
        len := 4
    else if (s = 'D') then
        len := 8;
    if (len <> 0) then
        tkn := Copy(tkn, 1, Length(tkn) - 1);
    if (not TryStrToInt(tkn, Integer(addr))) then
        raise Exception.Create('Invalid address');
end;

function TCpuTestForm.GetInteger: TDblWord;
var
    tkn: String;
begin
    tkn := GetToken;
    if (tkn <> '=') then
        raise Exception.Create('Missing =');

    tkn := GetToken;
    if (tkn = '') then
        raise Exception.Create('Invalid assignment')
    else if (not TryStrToInt64(tkn, Result)) then
        raise Exception.Create('Invalid numeric value');
end;

function TCpuTestForm.GetRegister(tkn: String): Integer;
begin
    tkn := Copy(tkn, 2);
    if (not TryStrToInt(tkn, Result)) then
        raise Exception.Create('Invalid register number');
    if ((Result < 0) or (Result > 15)) then
        raise Exception.Create('Invalid register number');
end;

function TCpuTestForm.GetToken: String;
// get next token from current script line. Tokens are strings of text
// separated by delimiters
const
    delim: array [0..4] of Char = ( ' ', '=', ',', '(', ')' );

    function IsDelim(c: Char): Boolean;
    var
        d: Char;
    begin
        Result := False;
        for d in delim do
        begin
            if (c = d) then
            begin
                Result := True;
                Exit;
            end;
        end;
    end;
var
    tstart, tend: Integer;
begin
    if (FScriptLine = '') then
    begin
        Result := '';
        Exit;
    end;
    // Skip leading spaces to find start of next token
    tstart := FScanStart;
    while ((tstart <= Length(FScriptLine)) and (FScriptLine[tstart] = ' ')) do
        Inc(tstart);
    if (IsDelim(FScriptLine[tstart])) then
    begin
        // Delimiter is the token
        Result := FScriptLine[tstart];
        FScanStart := tstart + 1;
        Exit;
    end;
    // Find delimiter to find end of current token
    tend := tstart;
    while ((tend <= Length(FScriptLine)) and (not IsDelim(FScriptLine[tend]))) do
        Inc(tend);
    // Extract token from line
    Result := Copy(FScriptLine, tstart, tend - tstart);
    FScanStart := tend;
end;

procedure TCpuTestForm.OpenScript;
begin
    FreeAndNil(FScriptFile);
    FScriptFile := TFileStream.Create(ScriptEdt.Text, fmOpenRead or fmShareDenyWrite);
end;

function TCpuTestForm.ReadScript: AnsiString;
var
    c: AnsiChar;
begin
    if (FScriptFile.Position >= FScriptFile.Size) then
    begin
        Result := '** EOF **';
        Exit;
    end;

    Result := '';
    while ((FScriptFile.Read(c, 1) = 1) and (c <> #10)) do
    begin
        if (c <> #13) then
            Result := Result + c;
    end;
end;

end.
