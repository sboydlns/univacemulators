unit U9200DisAsmFrm;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections,
    U9200Types, U9200Files;

type
  TEntryPoint = record
    Name: AnsiString;
    StartAddr: Smallint;
  end;

  TEntryPointList = class(TList<TEntryPoint>)
  private
    function GetEntryByAddr(addr: Smallint): TEntryPoint;
  public
    property EntryByAddr[addr: Smallint]: TEntryPoint read GetEntryByAddr;
  end;

  TExternalRef = record
    Name: AnsiString;
    ESID: Byte;
  end;

  TExternalRefList = class(TList<TExternalRef>)
  private
    function GetRefByESID(ESID: Byte): TExternalRef;
  public
    property RefByESID[ESID: Byte]: TExternalRef read GetRefByESID;
  end;

  TDisAsmState = (dasUnknown, dasConst, dasInst);

  TU9200DisAsmForm = class(TForm)
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
    FOpcodes: TU92OpcodeList;
    FEntryPoints: TEntryPointList;
    FExternalRefs: TExternalRefList;
    FState: TDisAsmState;
    FElementName: AnsiString;
    FStartAddr: Smallint;
    FElementESID: Byte;
    FCrntConst: array of Byte;
    FCrntInst: array [1 .. 6] of Byte;
    FCrntOpcode: TU92Opcode;
    FOp1Extern: String;
    FOp2Extern: String;
    FRegExtern: String;
    FCrntAddr: Smallint;
    FCrntInstLen: Integer;
    FLabel: String;
    FSkipCount: Integer;
    FHSeen: Boolean;
    FJSeen: Boolean;
    FKSeen: Boolean;
    FQSeen: Boolean;
    procedure DoARec(fin: TU92ObjectFile);
    procedure DoHRec(fin: TU92ObjectFile);
    procedure DoJRec(fin: TU92ObjectFile);
    procedure DoKRec(fin: TU92ObjectFile);
    procedure DoQRec(fin: TU92ObjectFile);
    procedure DoYRec(fin: TU92ObjectFile);
    procedure EmitConst;
    procedure EmitInst;
    function ExtRef(Name: String; offset: Integer; len: Integer = -1): String;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear;
  end;

var
    U9200DisAsmForm: TU9200DisAsmForm;

implementation

{$R *.dfm}


procedure TU9200DisAsmForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9200DisAsmForm.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TU9200DisAsmForm.Clear;
begin
    FState := dasUnknown;
    FElementName := '';
    FStartAddr := 0;
    FSkipCount := 0;
    SetLength(FCrntConst, 0);
    FHSeen := False;
    FJSeen := False;
    FKSeen := False;
    FQSeen := False;
    FEntryPoints.Clear;
    FExternalRefs.Clear;
    SourceMemo.Lines.Clear;
end;

constructor TU9200DisAsmForm.Create(AOwner: TComponent);
begin
    inherited;
    FState := dasUnknown;
    FOpcodes := TU92OpcodeList.Create;
    FEntryPoints := TEntryPointList.Create;
    FExternalRefs := TExternalRefList.Create;
    FOpcodes.Add(Opcode($40, 4, nil, 'STH', itRX));
    FOpcodes.Add(Opcode($45, 4, nil, 'BAL', itRX));
    FOpcodes.Add(Opcode($47, 4, nil, 'BC', itRX));
    FOpcodes.Add(Opcode($48, 4, nil, 'LH', itRX));
    FOpcodes.Add(Opcode($49, 4, nil, 'CH', itRX));
    FOpcodes.Add(Opcode($91, 4, nil, 'TM', itSI));
    FOpcodes.Add(Opcode($92, 4, nil, 'MVI', itSI));
    FOpcodes.Add(Opcode($94, 4, nil, 'NI', itSI));
    FOpcodes.Add(Opcode($95, 4, nil, 'CLI', itSI));
    FOpcodes.Add(Opcode($96, 4, nil, 'OI', itSI));
    FOpcodes.Add(Opcode($A0, 4, nil, 'SPSC', itSI));
    FOpcodes.Add(Opcode($A1, 4, nil, 'SRC', itSI));
    FOpcodes.Add(Opcode($A4, 4, nil, 'XIOF', itSI));
    FOpcodes.Add(Opcode($A5, 4, nil, 'TIO', itSI));
    FOpcodes.Add(Opcode($A6, 4, nil, 'AI', itSI));
    FOpcodes.Add(Opcode($A8, 4, nil, 'LPSC', itSI));
    FOpcodes.Add(Opcode($A9, 4, nil, 'HPR', itSI));
    FOpcodes.Add(Opcode($AA, 4, nil, 'AH', itRX));
    FOpcodes.Add(Opcode($AB, 4, nil, 'SH', itRX));
    FOpcodes.Add(Opcode($D1, 6, nil, 'MVN', itSS1));
    FOpcodes.Add(Opcode($D2, 6, nil, 'MVC', itSS1));
    FOpcodes.Add(Opcode($D4, 6, nil, 'NC', itSS1));
    FOpcodes.Add(Opcode($D5, 6, nil, 'CLC', itSS1));
    FOpcodes.Add(Opcode($D6, 6, nil, 'OC', itSS1));
    FOpcodes.Add(Opcode($DC, 6, nil, 'TR', itSS1));
    FOpcodes.Add(Opcode($DE, 6, nil, 'ED', itSS1));
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

procedure TU9200DisAsmForm.DoARec(fin: TU92ObjectFile);
begin
    FElementName := fin.SymName;
    FElementESID := fin.ExtSymID;
    FStartAddr := fin.StartAddr;
    FCrntAddr := FStartAddr;
    SourceMemo.Lines.Add(Format('        START %d', [FStartAddr]));
    SourceMemo.Lines.Add(Format('STRT    EQU   *', []));
    SourceMemo.Lines.Add('');
    SourceMemo.Lines.Add('        USING *,0');
    SourceMemo.Lines.Add('        USING *,1');
    SourceMemo.Lines.Add('        USING *,2');
    SourceMemo.Lines.Add('        USING *,3');
    SourceMemo.Lines.Add('        USING *,4');
    SourceMemo.Lines.Add('        USING *,5');
    SourceMemo.Lines.Add('        USING *,6');
    SourceMemo.Lines.Add('        USING *,7');
    SourceMemo.Lines.Add('');
end;

procedure TU9200DisAsmForm.DoHRec(fin: TU92ObjectFile);
var
    ep: TEntryPoint;
begin
    ep.Name := fin.SymName;
    ep.StartAddr := fin.SymAddr;
    FEntryPoints.Add(ep);
    if (not FHSeen) then
        SourceMemo.Lines.Add('');
    SourceMemo.Lines.Add(Format('        ENTRY %s', [ep.Name]));
    FHSeen := True;
end;

procedure TU9200DisAsmForm.DoJRec(fin: TU92ObjectFile);
begin
    FJSeen := True;
end;

procedure TU9200DisAsmForm.DoKRec(fin: TU92ObjectFile);
var
    ef: TExternalRef;
begin
    ef.Name := fin.SymName;
    ef.ESID := fin.ExtSymID;
    FExternalRefs.Add(ef);
    if (not FKSeen) then
        SourceMemo.Lines.Add('');
    SourceMemo.Lines.Add(Format('        EXTRN %s', [ef.Name]));
    FKSeen := True;
end;

procedure TU9200DisAsmForm.DoQRec(fin: TU92ObjectFile);
var
    b: Byte;
    offset: Smallint;
    i: Integer;
    len: Integer;
    rldLen: Integer;
    rld: TU92RLD;
    er: TExternalRef;
    ep: TEntryPoint;
    sign: String;
begin
    if (not FQSeen) then
        SourceMemo.Lines.Add('');
    FQSeen := True;
    if (fin.LoadAddr <> FCrntAddr) then
    begin
        case FState of
            dasConst:
            begin
                EmitConst;
            end;
            dasInst:
            begin
                for i := 1 to FCrntInstLen do
                begin
                    SetLength(FCrntConst, Length(FCrntConst) + 1);
                    FCrntConst[High(FCrntConst)] := FCrntInst[i];
                end;
                EmitConst;
            end;
        end;
        SourceMemo.Lines.Add('');
        if (fin.ExtSymID = 0) then
            SourceMemo.Lines.Add(Format('        ORG   X''%4.4x''', [fin.LoadAddr]))
        else
            SourceMemo.Lines.Add(Format('        ORG   STRT+%d', [fin.LoadAddr]));
        SourceMemo.Lines.Add('');
    end;
    FCrntAddr := fin.LoadAddr;
    len := fin.Length;
    i := 0;
    while (len >= 0) do
    begin
        ep := FEntryPoints.EntryByAddr[FCrntAddr];
        if (ep.Name <> '') then
        begin
            if (FState = dasConst) then
                EmitConst;
            FLabel := String(ep.Name);
        end;
        b := fin.Text[i];
        rld := fin.RLD[i];
        if (rld.ESID <> $FF) then
        begin
            rldLen := (rld.Flag and $07);
            if ((rld.Flag and $F0) = 0) then
                sign := '+'
            else
                sign := '-';
            if (rld.ESID = 1) then
                er.Name := 'STRT'
            else
                er := FExternalRefs.RefByESID[rld.ESID];
            if (er.Name <> '') then
            begin
                if (FState = dasInst) then
                begin
                    if (FCrntInstLen = 2) then
                        FOp1Extern := String(er.Name) + sign
                    else if (FCrntInstLen = 4) then
                        FOp2Extern := String(er.Name) + sign
                    else if (FCrntInstLen = 1) then
                        FRegExtern := String(er.Name) + sign
                    else
                        raise Exception.Create('Oops');
                end else if (FState = dasConst) then
                begin
                    EmitConst;
                    offset := (b shl 8) or fin.Text[i + 1];
                    if (offset = 0) then
                        SourceMemo.Lines.Add(Format('%-6.6s  DC    YL%d(%s)', [FLabel, rldLen, er.Name]))
                    else
                        SourceMemo.Lines.Add(Format('%-6.6s  DC    YL%d(%s+%d)',
                                                    [FLabel, rldLen, er.Name, offset]));
                    FSkipCount := rldLen;
                    FLabel := '';
                end else if (FState = dasUnknown) then
                begin
                    offset := (b shl 8) or fin.Text[i + 1];
                    if (offset = 0) then
                        SourceMemo.Lines.Add(Format('%-6.6s  DC    YL%d(%s)', [FLabel, rldLen, er.Name]))
                    else
                        SourceMemo.Lines.Add(Format('%-6.6s  DC    YL%d(%s+%d)',
                                                    [FLabel, rldLen, er.Name, offset]));
                    FSkipCount := rldLen;
                    FLabel := '';
                end;
            end;
        end;
        if (FSkipCount > 0) then
        begin
            Dec(FSkipCount);
        end else if (FState = dasInst) then
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
        Inc(FCrntAddr);
        Inc(i);
        Dec(len);
    end;
end;

procedure TU9200DisAsmForm.DoYRec(fin: TU92ObjectFile);
begin
    SourceMemo.Lines.Add('');
    SourceMemo.Lines.Add(Format('        END   X''%4.4x''', [fin.StartAddr]));
end;

procedure TU9200DisAsmForm.EmitConst;
var
    i: Integer;
    stemp: String;
    stemp1: String;
begin
    stemp := '';
    for i := Low(FCrntConst) to High(FCrntConst) do
        stemp := stemp + Format('%2.2x', [FCrntConst[i]]);
    while (stemp <> '') do
    begin
        stemp1 := Copy(stemp, 1, 32);
        stemp := Copy(stemp, 33);
        SourceMemo.Lines.Add(Format('%-6.6s  DC    XL%d''%s''',
                                    [FLabel, Length(stemp1) div 2, stemp1]));
    end;
    SetLength(FCrntConst, 0);
    FState := dasUnknown;
    FLabel := '';
end;

procedure TU9200DisAsmForm.EmitInst;
var
    r, b1, b2, fal: Byte;
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
        itRX:
        begin
            r := (fal and $F0) shr 4;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (FOp1Extern <> '') then
                op1 := ExtRef(FOp1Extern, fad1)
            else
              if (b1 < 8) then
                op1 := Format('X''%4.4x''', [fad1])
            else
                op1 := Format('X''%3.3x''(,%d)', [off1, b1]);
            if (FRegExtern <> '') then
                reg := ExtRef(FRegExtern, r)
            else
                reg := Format('%d', [r]);
            SourceMemo.Lines.Add(Format('%-6.6s  %-5.5s %s,%s', [FLabel, FCrntOpcode.Code, reg, op1]))
        end;
        itSI:
        begin
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (FOp1Extern <> '') then
                op1 := ExtRef(FOp1Extern, fad1)
            else
              if (b1 < 8) then
                op1 := Format('X''%4.4x''', [fad1])
            else
                op1 := Format('X''%3.3x''(%d)', [off1, b1]);
            if (FRegExtern <> '') then
                reg := ExtRef(FRegExtern, fal)
            else
                reg := Format('%d', [fal]);
            if (b1 < 8) then
                SourceMemo.Lines.Add(Format('%-6.6s  %-5.5s %s,%s', [FLabel, FCrntOpcode.Code, op1, reg]))
            else
                SourceMemo.Lines.Add(Format('%-6.6s  %-5.5s %s,%s', [FLabel, FCrntOpcode.Code, op1, reg]));
        end;
        itSS1:
        begin
            l1 := fal + 1;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (FOp1Extern <> '') then
                op1 := ExtRef(FOp1Extern, fad1, l1)
            else if (b1 < 8) then
                op1 := Format('X''%4.4x''(%d)', [fad1, l1])
            else
                op1 := Format('X''%3.3x''(%d,%d)', [off1, l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (FOp2Extern <> '') then
                op2 := ExtRef(FOp2Extern, fad2)
            else if (b2 < 8) then
                op2 := Format('X''%4.4x''', [fad2])
            else
                op2 := Format('X''%3.3x''(%d)', [off2, b2]);
            SourceMemo.Lines.Add(Format('%-6.6s  %-5.5s %s,%s', [FLabel, FCrntOpcode.Code, op1, op2]));
        end;
        itSS2:
        begin
            l1 := ((fal and $F0) shr 4) + 1;
            l2 := (fal and $0F) + 1;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (FOp1Extern <> '') then
                op1 := ExtRef(FOp1Extern, fad1, l1)
            else if (b1 < 8) then
                op1 := Format('X''%4.4x''(%d)', [fad1, l1])
            else
                op1 := Format('X''%3.3x''(%d,%d)', [off1, l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (FOp2Extern <> '') then
                op2 := ExtRef(FOp2Extern, fad2, l2)
            else if (b2 < 8) then
                op2 := Format('X''%4.4x''(%d)', [fad2, l2])
            else
                op2 := Format('X''%3.3x''(%d,%d)', [off2, l2, b2]);
            SourceMemo.Lines.Add(Format('%-6.6s  %-5.5s %s,%s', [FLabel, FCrntOpcode.Code, op1, op2]));
        end;
        itBranch:
        ;
        itUnknown:
        ;
    end;
    FOp1Extern := '';
    FOp2Extern := '';
    FRegExtern := '';
    FLabel := '';
    FState := dasUnknown;
end;

function TU9200DisAsmForm.ExtRef(Name: String; offset: Integer; len: Integer): String;
begin
    Result := Format('%s%d', [name, offset]);
    if ((Pos('+0', Result) > 0) or (Pos('-0', Result) > 0)) then
        Result := Copy(Result, 1, Length(Result) - 2);
    if (len >= 0) then
        Result := Format('%s(%d)', [Result, len]);
end;

procedure TU9200DisAsmForm.FormShow(Sender: TObject);
begin
    FileNameEdt.SetFocus;
end;

procedure TU9200DisAsmForm.OkBtnClick(Sender: TObject);
var
    fin: TU92ObjectFile;
begin
    if (FileNameEdt.Text = '') then
    begin
        FileNameEdt.SetFocus;
        raise Exception.Create('Please enter the object file name');
    end;

    fin := TU92ObjectFile.Create(FileNameEdt.Text, fmOpenRead);
    try
        Clear;
        while (not fin.Eof) do
        begin
            fin.Read;
            if (fin.Key = $D1) then
            begin
                case fin.CardType of
                  'A':
                    DoARec(fin);
                  'H':
                    DoHRec(fin);
                  'J':
                    DoJRec(fin);
                  'K':
                    DoKRec(fin);
                  'Q':
                    DoQRec(fin);
                  'Y':
                    DoYRec(fin);
                end;
            end;
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200DisAsmForm.SaveBtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
        SourceMemo.Lines.SaveToFile(SaveDlg.FileName);
end;

{ TExternalRefList }

function TExternalRefList.GetRefByESID(ESID: Byte): TExternalRef;
// Return the external reference for the given ESID.
var
    er: TExternalRef;
begin
    Result.Name := '';
    for er in Self do
    begin
        if (er.ESID = ESID) then
        begin
            Result := er;
            Exit;
        end;
    end;
end;

{ TEntryPointList }

function TEntryPointList.GetEntryByAddr(addr: Smallint): TEntryPoint;
// Return the entry point for the given address.
var
    ep: TEntryPoint;
begin
    Result.Name := '';
    for ep in Self do
    begin
        if (ep.StartAddr = addr) then
        begin
            Result := ep;
            Exit;
        end;
    end;
end;

end.
