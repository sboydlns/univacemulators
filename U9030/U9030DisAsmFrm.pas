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
        if (FCrntOpcode.Code = 'SVC') then
            SourceMemo.Lines.Add(Format('%4.4x: %-6.6s  %-5.5s %d',
                                        [FCrntAddr - FCrntOpcode.Length + 1, FLabel, FCrntOpcode.Code, fal]))
        else
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
            reg := Format('R%d', [r]);
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
        x := (fal and $0f);
        b1 := (fad1 and $F000) shr 12;
        off1 := fad1 and $FFF;
        if (FOp1Extern <> '') then
            op1 := ExtRef(FOp1Extern, fad1)
        else if (b1 = 0) then
        begin
            if (x = 0) then
                op1 := Format('X''%3.3x''', [fad1])
            else
                op1 := Format('X''%3.3x''(R%d)', [fad1, x]);
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
