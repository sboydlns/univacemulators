unit U9030DisAsmFrm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections,
    U9030Types{, U9200Files};

type
  TParamRec = record
    StartAddr, EndAddr: Integer;
  end;

  TDisAsmState = (dasUnknown, dasConst, dasInst);

  TU9030DisAsmForm = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    FileNameEdt: TEdit;
    Label1: TLabel;
    BrowseBtn: TButton;
    SourceMemo: TMemo;
    OpenDlg: TOpenDialog;
    SaveBtn: TButton;
    SaveDlg: TSaveDialog;
    procedure CancelBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FOpcodes: TOpcodeList;
    FState: TDisAsmState;
    FParams: array of TParamRec;
    FParamIndex: Integer;
    FElementName: AnsiString;
    FStartAddr: Smallint;
    FCrntConst: array of Byte;
    FCrntInst: array [1 .. 6] of Byte;
    FCrntOpcode: TOpcode;
    FOp1Extern: String;
    FOp2Extern: String;
    FRegExtern: String;
    FCrntAddr: Smallint;
    FCrntInstLen: Integer;
    FLabel: String;
    FSkipCount: Integer;
    procedure EmitConst;
    procedure EmitInst;
    function ExtRef(Name: String; offset: Integer; len: Integer = -1): String;
    procedure LoadParams;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
  end;

var
    U9030DisAsmForm: TU9030DisAsmForm;

implementation

{$R *.dfm}


procedure TU9030DisAsmForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9030DisAsmForm.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TU9030DisAsmForm.Clear;
begin
    FState := dasUnknown;
    FElementName := '';
    FStartAddr := 0;
    FSkipCount := 0;
    SetLength(FCrntConst, 0);
    SourceMemo.Lines.Clear;
end;

constructor TU9030DisAsmForm.Create(AOwner: TComponent);
begin
    inherited;
    FState := dasUnknown;
    FOpcodes := TOpcodeList.Create;
    FOpcodes.Add(Opcode($03, 2, nil, 'STR', itRR));
    FOpcodes.Add(Opcode($04, 2, nil, 'SPM', itRR));
    FOpcodes.Add(Opcode($05, 2, nil, 'BALR', itRR));
    FOpcodes.Add(Opcode($06, 2, nil, 'BCTR', itRR));
    FOpcodes.Add(Opcode($07, 2, nil, 'BCR', itRR));
    FOpcodes.Add(Opcode($08, 2, nil, 'SSK', itRR));
    FOpcodes.Add(Opcode($09, 2, nil, 'ISK', itRR));
    FOpcodes.Add(Opcode($0A, 2, nil, 'SVC', itRR));
    FOpcodes.Add(Opcode($0D, 2, nil, 'BASR', itRR));
    FOpcodes.Add(Opcode($10, 2, nil, 'LPR', itRR));
    FOpcodes.Add(Opcode($11, 2, nil, 'LNR', itRR));
    FOpcodes.Add(Opcode($12, 2, nil, 'LTE', itRR));
    FOpcodes.Add(Opcode($13, 2, nil, 'LCR', itRR));
    FOpcodes.Add(Opcode($14, 2, nil, 'NR', itRR));
    FOpcodes.Add(Opcode($15, 2, nil, 'CLR', itRR));
    FOpcodes.Add(Opcode($16, 2, nil, 'OR', itRR));
    FOpcodes.Add(Opcode($17, 2, nil, 'XR', itRR));
    FOpcodes.Add(Opcode($18, 2, nil, 'LR', itRR));
    FOpcodes.Add(Opcode($19, 2, nil, 'CR', itRR));
    FOpcodes.Add(Opcode($1A, 2, nil, 'AR', itRR));
    FOpcodes.Add(Opcode($1B, 2, nil, 'SR', itRR));
    FOpcodes.Add(Opcode($1C, 2, nil, 'MR', itRR));
    FOpcodes.Add(Opcode($1D, 2, nil, 'DR', itRR));
    FOpcodes.Add(Opcode($1E, 2, nil, 'ALR', itRR));
    FOpcodes.Add(Opcode($1F, 2, nil, 'SLR', itRR));
    FOpcodes.Add(Opcode($20, 2, nil, 'LDPR', itRR));
    FOpcodes.Add(Opcode($21, 2, nil, 'LNDR', itRR));
    FOpcodes.Add(Opcode($22, 2, nil, 'LTDR', itRR));
    FOpcodes.Add(Opcode($23, 2, nil, 'LDCR', itRR));
    FOpcodes.Add(Opcode($24, 2, nil, 'HDR', itRR));
    FOpcodes.Add(Opcode($28, 2, nil, 'LDR', itRR));
    FOpcodes.Add(Opcode($29, 2, nil, 'CDR', itRR));
    FOpcodes.Add(Opcode($2A, 2, nil, 'ADR', itRR));
    FOpcodes.Add(Opcode($2B, 2, nil, 'SDR', itRR));
    FOpcodes.Add(Opcode($2C, 2, nil, 'MDR', itRR));
    FOpcodes.Add(Opcode($2D, 2, nil, 'DDR', itRR));
    FOpcodes.Add(Opcode($2E, 2, nil, 'AWR', itRR));
    FOpcodes.Add(Opcode($2F, 2, nil, 'SWR', itRR));
    FOpcodes.Add(Opcode($30, 2, nil, 'LPER', itRR));
    FOpcodes.Add(Opcode($31, 2, nil, 'LNER', itRR));
    FOpcodes.Add(Opcode($32, 2, nil, 'LTER', itRR));
    FOpcodes.Add(Opcode($33, 2, nil, 'LCER', itRR));
    FOpcodes.Add(Opcode($34, 2, nil, 'HER', itRR));
    FOpcodes.Add(Opcode($38, 2, nil, 'LER', itRR));
    FOpcodes.Add(Opcode($39, 2, nil, 'CER', itRR));
    FOpcodes.Add(Opcode($3A, 2, nil, 'AER', itRR));
    FOpcodes.Add(Opcode($3B, 2, nil, 'SER', itRR));
    FOpcodes.Add(Opcode($3C, 2, nil, 'MER', itRR));
    FOpcodes.Add(Opcode($3D, 2, nil, 'DER', itRR));
    FOpcodes.Add(Opcode($3E, 2, nil, 'AUR', itRR));
    FOpcodes.Add(Opcode($3F, 2, nil, 'SUR', itRR));
    FOpcodes.Add(Opcode($40, 4, nil, 'STH', itRX));
    FOpcodes.Add(Opcode($41, 4, nil, 'LA', itRX));
    FOpcodes.Add(Opcode($42, 4, nil, 'STC', itRX));
    FOpcodes.Add(Opcode($43, 4, nil, 'IC', itRX));
    FOpcodes.Add(Opcode($44, 4, nil, 'EX', itRX));
    FOpcodes.Add(Opcode($45, 4, nil, 'BAL', itRX));
    FOpcodes.Add(Opcode($46, 4, nil, 'BCT', itRX));
    FOpcodes.Add(Opcode($47, 4, nil, 'BC', itBranch));
    FOpcodes.Add(Opcode($48, 4, nil, 'LH', itRX));
    FOpcodes.Add(Opcode($49, 4, nil, 'CH', itRX));
    FOpcodes.Add(Opcode($4A, 4, nil, 'AH', itRX));
    FOpcodes.Add(Opcode($4B, 4, nil, 'SH', itRX));
    FOpcodes.Add(Opcode($4C, 4, nil, 'MH', itRX));
    FOpcodes.Add(Opcode($4D, 4, nil, 'BAS', itRX));
    FOpcodes.Add(Opcode($4E, 4, nil, 'CVD', itRX));
    FOpcodes.Add(Opcode($4F, 4, nil, 'CVB', itRX));
    FOpcodes.Add(Opcode($50, 4, nil, 'ST', itRX));
    FOpcodes.Add(Opcode($54, 4, nil, 'N', itRX));
    FOpcodes.Add(Opcode($55, 4, nil, 'CL', itRX));
    FOpcodes.Add(Opcode($56, 4, nil, 'O', itRX));
    FOpcodes.Add(Opcode($57, 4, nil, 'X', itRX));
    FOpcodes.Add(Opcode($58, 4, nil, 'L', itRX));
    FOpcodes.Add(Opcode($59, 4, nil, 'C', itRX));
    FOpcodes.Add(Opcode($5A, 4, nil, 'A', itRX));
    FOpcodes.Add(Opcode($5B, 4, nil, 'S', itRX));
    FOpcodes.Add(Opcode($5C, 4, nil, 'M', itRX));
    FOpcodes.Add(Opcode($5D, 4, nil, 'D', itRX));
    FOpcodes.Add(Opcode($5E, 4, nil, 'AL', itRX));
    FOpcodes.Add(Opcode($60, 4, nil, 'STD', itRX));
    FOpcodes.Add(Opcode($68, 4, nil, 'LD', itRX));
    FOpcodes.Add(Opcode($69, 4, nil, 'CD', itRX));
    FOpcodes.Add(Opcode($6A, 4, nil, 'AD', itRX));
    FOpcodes.Add(Opcode($6B, 4, nil, 'SD', itRX));
    FOpcodes.Add(Opcode($6C, 4, nil, 'MD', itRX));
    FOpcodes.Add(Opcode($6D, 4, nil, 'DD', itRX));
    FOpcodes.Add(Opcode($6E, 4, nil, 'AW', itRX));
    FOpcodes.Add(Opcode($6F, 4, nil, 'SW', itRX));
    FOpcodes.Add(Opcode($70, 4, nil, 'STE', itRX));
    FOpcodes.Add(Opcode($78, 4, nil, 'LE', itRX));
    FOpcodes.Add(Opcode($79, 4, nil, 'CE', itRX));
    FOpcodes.Add(Opcode($7A, 4, nil, 'AE', itRX));
    FOpcodes.Add(Opcode($7B, 4, nil, 'SE', itRX));
    FOpcodes.Add(Opcode($7C, 4, nil, 'ME', itRX));
    FOpcodes.Add(Opcode($7D, 4, nil, 'DE', itRX));
    FOpcodes.Add(Opcode($7E, 4, nil, 'AU', itRX));
    FOpcodes.Add(Opcode($7F, 4, nil, 'SU', itRX));
    FOpcodes.Add(Opcode($80, 4, nil, 'SSM', itSI));
    FOpcodes.Add(Opcode($82, 4, nil, 'LPSW', itSI));
    FOpcodes.Add(Opcode($83, 4, nil, 'DIAG', itSI));
    FOpcodes.Add(Opcode($86, 4, nil, 'BXH', itRS));
    FOpcodes.Add(Opcode($87, 4, nil, 'BXLE', itRS));
    FOpcodes.Add(Opcode($88, 4, nil, 'SRL', itRS));
    FOpcodes.Add(Opcode($89, 4, nil, 'SLL', itRS));
    FOpcodes.Add(Opcode($8A, 4, nil, 'SRA', itRS));
    FOpcodes.Add(Opcode($8B, 4, nil, 'SLA', itRS));
    FOpcodes.Add(Opcode($8C, 4, nil, 'SRDL', itRS));
    FOpcodes.Add(Opcode($8D, 4, nil, 'SLDL', itRS));
    FOpcodes.Add(Opcode($8E, 4, nil, 'SRDA', itRS));
    FOpcodes.Add(Opcode($8F, 4, nil, 'SLDA', itRS));
    FOpcodes.Add(Opcode($90, 4, nil, 'STM', itRS));
    FOpcodes.Add(Opcode($91, 4, nil, 'TM', itSI));
    FOpcodes.Add(Opcode($92, 4, nil, 'MVI', itSI));
    FOpcodes.Add(Opcode($93, 4, nil, 'TS', itSI));
    FOpcodes.Add(Opcode($94, 4, nil, 'NI', itSI));
    FOpcodes.Add(Opcode($95, 4, nil, 'CLI', itSI));
    FOpcodes.Add(Opcode($96, 4, nil, 'OI', itSI));
    FOpcodes.Add(Opcode($97, 4, nil, 'XI', itSI));
    FOpcodes.Add(Opcode($98, 4, nil, 'LM', itRS));
    FOpcodes.Add(Opcode($99, 4, nil, 'HPR', itSI));
    FOpcodes.Add(Opcode($9A, 4, nil, 'AI', itSI));
    FOpcodes.Add(Opcode($9C, 4, nil, 'SIO', itSI));
    FOpcodes.Add(Opcode($A2, 4, nil, 'SSFS', itRS));
    FOpcodes.Add(Opcode($A3, 4, nil, 'SSRS', itRS));
    FOpcodes.Add(Opcode($A6, 4, nil, 'AI', itSI));
    FOpcodes.Add(Opcode($A9, 4, nil, 'HPR', itSI));
    FOpcodes.Add(Opcode($AA, 4, nil, 'AH', itRS));
    FOpcodes.Add(Opcode($AB, 4, nil, 'SH', itRS));
    FOpcodes.Add(Opcode($B0, 4, nil, 'SSTM', itRS));
    FOpcodes.Add(Opcode($B1, 4, nil, 'LCS', itRS));
    FOpcodes.Add(Opcode($B8, 4, nil, 'SLM', itRS));
    FOpcodes.Add(Opcode($D1, 6, nil, 'MVN', itSS1));
    FOpcodes.Add(Opcode($D2, 6, nil, 'MVC', itSS1));
    FOpcodes.Add(Opcode($D3, 6, nil, 'MVZ', itSS1));
    FOpcodes.Add(Opcode($D4, 6, nil, 'NC', itSS1));
    FOpcodes.Add(Opcode($D5, 6, nil, 'CLC', itSS1));
    FOpcodes.Add(Opcode($D6, 6, nil, 'OC', itSS1));
    FOpcodes.Add(Opcode($D7, 6, nil, 'XC', itSS1));
    FOpcodes.Add(Opcode($DC, 6, nil, 'TR', itSS1));
    FOpcodes.Add(Opcode($DD, 6, nil, 'TRT', itSS1));
    FOpcodes.Add(Opcode($DE, 6, nil, 'ED', itSS1));
    FOpcodes.Add(Opcode($DF, 6, nil, 'EDMK', itSS1));
    FOpcodes.Add(Opcode($F1, 6, nil, 'MVO', itSS2));
    FOpcodes.Add(Opcode($F2, 6, nil, 'PACK', itSS2));
    FOpcodes.Add(Opcode($F3, 6, nil, 'UNPK', itSS2));
    FOpcodes.Add(Opcode($F8, 6, nil, 'ZAP', itSS2));
    FOpcodes.Add(Opcode($F9, 6, nil, 'CP', itSS2));
    FOpcodes.Add(Opcode($FA, 6, nil, 'AP', itSS2));
    FOpcodes.Add(Opcode($FB, 6, nil, 'SP', itSS2));
    FOpcodes.Add(Opcode($FC, 6, nil, 'MP', itSS2));
    FOpcodes.Add(Opcode($FD, 6, nil, 'DP', itSS2));
end;


procedure TU9030DisAsmForm.EmitConst;
var
    i, addr: Integer;
    stemp: String;
    stemp1: String;
begin
    stemp := '';
    addr := FCrntAddr - Length(FCrntConst);
    for i := Low(FCrntConst) to High(FCrntConst) do
        stemp := stemp + Format('%2.2x', [FCrntConst[i]]);
    while (stemp <> '') do
    begin
        stemp1 := Copy(stemp, 1, 32);
        stemp := Copy(stemp, 33);
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  DC    XL%d''%s''',
                                    [addr, FLabel, Length(stemp1) div 2, stemp1]));
        Inc(addr, Length(stemp1) div 2);
    end;
    SetLength(FCrntConst, 0);
    FState := dasUnknown;
    FLabel := '';
end;

procedure TU9030DisAsmForm.EmitInst;
var
    r, r2, b1, b2, x, fal: Byte;
    l1, l2: Smallint;
    fad1, fad2, off1, off2: Smallint;
    op1, op2, reg: String;
begin
    fal := FCrntInst[2];
    fad1 := (FCrntInst[3] shl 8) or FCrntInst[4];
    if (FCrntOpcode.Length = 6) then
        fad2 := (FCrntInst[5] shl 8) or FCrntInst[6]
    else
        fad2 := 0;
    case FCrntOpcode.InstType of
      itRR:
      begin
        r := (fal and $f0) shr 4;
        r2 := fal and $0f;
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s R%d,R%d',
                                    [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, r, r2]));
      end;
      itRS:
      begin
        r := (fal and $F0) shr 4;
        r2 := fal and $0f;
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1)
        else
          if (b1 = 0) then
            op1 := Format('X''%4.4x''', [fad1])
        else
            op1 := Format('X''%3.3x''(,R%d)', [off1, b1]);
        if (FRegExtern <> '') then
            reg := ExtRef(FRegExtern, r)
        else
            reg := Format('R%d', [r]);
        if ((FCrntOpcode.Code = 'STM') or
            (FCrntOpcode.Code = 'LM') or
            (FCrntOpcode.Code = 'SSTM') or
            (FCrntOpcode.Code = 'SLM')) then
            SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,R%d,%s',
                                        [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, reg, r2, op1]))
        else
            SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                        [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, reg, op1]));
      end;
      itRX:
      begin
        r := (fal and $F0) shr 4;
        x := fal and $0f;
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1)
        else if (b1 = 0) then
        begin
            if (x = 0) then
                op1 := Format('X''%4.4x''', [fad1])
            else
                op1 := Format('X''%4.4x''(R%d)', [fad1, x]);
        end else
        begin
            if (x = 0) then
                op1 := Format('X''%3.3x''(,R%d)', [off1, b1])
          else
                op1 := Format('X''%3.3x''(R%d,R%d)', [off1, x, b1]);
        end;
        if (FRegExtern <> '') then
            reg := ExtRef(FRegExtern, r)
        else
            reg := Format('%d', [r]);
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                    [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, reg, op1]))
      end;
      itSI:
      begin
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1)
        else
          if (b1 = 0) then
            op1 := Format('X''%4.4x''', [fad1])
        else
            op1 := Format('X''%3.3x''(R%d)', [off1, b1]);
        if (FRegExtern <> '') then
            reg := ExtRef(FRegExtern, fal)
        else
            reg := Format('%d', [fal]);
        if (b1 = 0) then
            SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                        [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, op1, reg]))
        else
            SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                        [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, op1, reg]));
      end;
      itSS1:
      begin
        l1 := fal + 1;
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1, l1)
        else if (b1 = 0) then
            op1 := Format('X''%4.4x''(%d)', [fad1, l1])
        else
            op1 := Format('X''%3.3x''(%d,R%d)', [off1, l1, b1]);
        b2 := (fad2 and $F000) shr 12;
        off2 := fad2 and $FFF;
        if (FOp2Extern <> '') then
            op2 := ExtRef(FOp2Extern, fad2)
        else if (b2 = 0) then
            op2 := Format('X''%4.4x''', [fad2])
        else
            op2 := Format('X''%3.3x''(R%d)', [off2, b2]);
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                    [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, op1, op2]));
      end;
      itSS2:
      begin
        l1 := ((fal and $F0) shr 4) + 1;
        l2 := (fal and $0F) + 1;
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1, l1)
        else if (b1 = 0) then
            op1 := Format('X''%4.4x''(%d)', [fad1, l1])
        else
            op1 := Format('X''%3.3x''(%d,R%d)', [off1, l1, b1]);
        b2 := (fad2 and $F000) shr 12;
        off2 := fad2 and $FFF;
        if (FOp2Extern <> '') then
            op2 := ExtRef(FOp2Extern, fad2, l2)
        else if (b2 = 0) then
            op2 := Format('X''%4.4x''(%d)', [fad2, l2])
        else
            op2 := Format('X''%3.3x''(%d,R%d)', [off2, l2, b2]);
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                    [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, op1, op2]));
      end;
      itBranch:
      begin
        r := (fal and $F0) shr 4;
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1)
        else
          if (b1 = 0) then
            op1 := Format('X''%4.4x''', [fad1])
        else
            op1 := Format('X''%3.3x''(,R%d)', [off1, b1]);
        if (FRegExtern <> '') then
            reg := ExtRef(FRegExtern, r)
        else
            reg := Format('%d', [r]);
        SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %s,%s',
                                    [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, reg, op1]))
      end;
      itUnknown:
        ;
    end;
    FOp1Extern := '';
    FOp2Extern := '';
    FRegExtern := '';
    FLabel := '';
    FState := dasUnknown;
end;

function TU9030DisAsmForm.ExtRef(Name: String; offset: Integer; len: Integer): String;
begin
    Result := Format('%s%d', [name, offset]);
    if ((Pos('+0', Result) > 0) or (Pos('-0', Result) > 0)) then
        Result := Copy(Result, 1, Length(Result) - 2);
    if (len >= 0) then
        Result := Format('%s(%d)', [Result, len]);
end;

procedure TU9030DisAsmForm.FormShow(Sender: TObject);
begin
    FileNameEdt.SetFocus;
end;

procedure TU9030DisAsmForm.LoadParams;
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

procedure TU9030DisAsmForm.OkBtnClick(Sender: TObject);
var
    fin: TFileStream;
    bfr: PByte;
    b: Byte;
    i: Integer;
    len: Integer;
begin
    if (FileNameEdt.Text = '') then
    begin
        FileNameEdt.SetFocus;
        raise Exception.Create('Please enter the object file name');
    end;

    LoadParams;
    bfr := nil;
    FCrntAddr := 0;
    FParamIndex := Low(FParams);
    if (Length(FParams) <> 0) then
    begin
        if (FParams[Low(FParams)].EndAddr = 0) then
        begin
            FCrntAddr := FParams[Low(FParams)].StartAddr;
            Inc(FParamIndex);
        end;
    end;
    fin := TFileStream.Create(FileNameEdt.Text, fmOpenRead);
    try
        GetMem(bfr, fin.Size);
        Clear;
        fin.Read(bfr^, fin.Size);
        SourceMemo.Lines.Add(Format('%4.4x:         START', [FCrntAddr]));
        SourceMemo.Lines.Add(Format('%4.4x: STRT    EQU   *', [FCrntAddr]));
        SourceMemo.Lines.Add('');
        len := fin.Size;
        i := 0;
        while (len >= 0) do
        begin
            b := bfr[i];
            if ((FParamIndex <= High(FParams)) and
                (FCrntAddr >= FParams[FParamIndex].StartAddr) and
                (FCrntAddr <= FParams[FParamIndex].EndAddr)) then
            begin
                SetLength(FCrntConst, Length(FCrntConst) + 1);
                FCrntConst[High(FCrntConst)] := b;
                FState := dasConst;
            end else
            begin
                if (FState = dasInst) then
                begin
                    Inc(FCrntInstLen);
                    FCrntInst[FCrntInstLen] := b;
                    if (FCrntOpcode.Length = FCrntInstLen) then
                        EmitInst;
                end else
                begin
                    if (((FCrntAddr mod 2) = 0) and FOpcodes.IsOpcode(b)) then
                    begin
                        if (FState = dasConst) then
                            EmitConst;
                        FState := dasInst;
                        FCrntOpcode := FOpcodes.FindOpcode(b);
                        FCrntInst[1] := b;
                        FCrntInstLen := 1;
                    end else
                    begin
                        SetLength(FCrntConst, Length(FCrntConst) + 1);
                        FCrntConst[High(FCrntConst)] := b;
                        FState := dasConst;
                    end;
                end;
            end;
            Inc(FCrntAddr);
            Inc(i);
            Dec(len);
            if ((FParamIndex <= High(FParams)) and
                (FCrntAddr > FParams[FParamIndex].EndAddr)) then
            begin
                EmitConst;
                FState := dasUnknown;
                Inc(FParamIndex);
            end;
        end;
    finally
        fin.Free;
        FreeMem(bfr);
    end;
end;

procedure TU9030DisAsmForm.SaveBtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
        SourceMemo.Lines.SaveToFile(SaveDlg.FileName);
end;

end.
