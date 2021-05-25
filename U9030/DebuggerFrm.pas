unit DebuggerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Cpu, Memory, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  U9030Types, Generics.Collections, Vcl.ComCtrls;

type
  TDebuggerState = ( dsStep, dsContinue );

  TDebuggerForm = class(TForm)
    DumpMemo: TMemo;
    Bevel1: TBevel;
    SuperBox: TCheckBox;
    ProgBox: TCheckBox;
    StepBox: TCheckBox;
    HaltBox: TCheckBox;
    ErrorBox: TCheckBox;
    Label1: TLabel;
    RegGrid: TStringGrid;
    Label2: TLabel;
    ExceptLbl: TLabel;
    CommandEdt: TEdit;
    Pages: TPageControl;
    TracePage: TTabSheet;
    TraceGrid: TStringGrid;
    BrkptPage: TTabSheet;
    BrkptGrid: TStringGrid;
    TimerBox: TCheckBox;
    IOBox: TCheckBox;
    OpenDlg: TOpenDialog;
    Label3: TLabel;
    RegSuperCheck: TCheckBox;
    RegPgmCheck: TCheckBox;
    Label4: TLabel;
    Label5: TLabel;
    CondCodeLbl: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CommandEdtKeyPress(Sender: TObject; var Key: Char);
  private
    FCommand: String;
    FState: TDebuggerState;
    FTraceList: TList<TMemoryAddress>;
    FBreakPoints: TList<TMemoryAddress>;
    FWaiting: Boolean;
    FDumpAddr: TMemoryAddress;
    procedure Dump(start: TMemoryAddress);
    procedure FillForm;
    function ToHex(val: Smallint): String;
    procedure ParseCommand(s: String; var cmd: String; var param1, param2: Integer);
    function Printable(val: Cardinal): String;
    procedure SysDump(start, fin: TMemoryAddress);
    procedure Wait;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DoDebug(Sender: TObject; E: Exception);
  end;

var
  DebuggerForm: TDebuggerForm;

implementation

uses EmulatorTypes, Globals;

{$R *.dfm}

{ TU9200Debugger }

procedure TDebuggerForm.CommandEdtKeyPress(Sender: TObject; var Key: Char);
var
    cmd: String;
    addr, addr2: Integer;
    i: Integer;
begin
    Key := UpCase(Key);
    case Key of
      'C':
      begin
        if (Length(CommandEdt.Text) = 0) then
        begin
            FState := dsContinue;
            Key := #0;
            CommandEdt.Text := '';
            FWaiting := False;
        end;
      end;
      'N':
      begin
        if (Length(CommandEdt.Text) = 0) then
        begin
            FState := dsStep;
            Key := #0;
            CommandEdt.Text := '';
            FWaiting := False;
        end;
      end;
      'X':
      begin
        Key := #0;
        CommandEdt.Text := '';
        FWaiting := False;
      end;
      #13,
      #10:
      begin
        ParseCommand(CommandEdt.Text, cmd, addr, addr2);
        if (cmd = 'B') then
        begin
            if (addr <> -1) then
            begin
                i := FBreakPoints.IndexOf(addr);
                if (i = -1) then
                    FBreakPoints.Add(TMemoryAddress(addr))
                else
                    FBreakPoints.Delete(i);
                CommandEdt.Clear;
            end;
        end else if (cmd = 'D') then
        begin
            if (addr <> -1) then
            begin
                FDumpAddr := TMemoryAddress(addr);
                Dump(FDumpAddr);
                CommandEdt.Clear;
            end;
        end else if (cmd = 'SD') then
        begin
            SysDump(TMemoryAddress(addr), TMemoryAddress(addr2));
        end;
        Key := #0;
        CommandEdt.Text := '';
      end;
    end;
end;

constructor TDebuggerForm.Create(AOwner: TComponent);
begin
    inherited;
    FCommand := '';
    FState := dsStep;
    FTraceList := TList<TMemoryAddress>.Create;
    FBreakPoints := TList<TMemoryAddress>.Create;
end;

destructor TDebuggerForm.Destroy;
begin
    FreeAndNil(FTraceList);
    FreeAndNil(FBreakPoints);
    inherited;
end;

procedure TDebuggerForm.DoDebug(Sender: TObject; E: Exception);
var
    opcode: TOpcode;
    pc: TMemoryAddress;
begin
    if (Assigned(E)) then
    begin
        ExceptLbl.Caption := E.Message;
        FState := dsStep;
    end else
        ExceptLbl.Caption := '';
    opcode := Opcodes[CurInst.Opcode];
    pc := PSW.InstAddr - (PSW.InstLength * 2);
    while (FTraceList.Count >= (TraceGrid.RowCount - 1)) do
        FTraceList.Delete(0);
    FTraceList.Add(pc);
    if (Visible) then
    begin
        // If we hit an HPR, stop
        if (opcode.Opcode = $99) then
            FState := dsStep;
        if (FBreakPoints.IndexOf(pc) <> -1) then
        begin
            FState := dsStep;
            ExceptLbl.Caption := Format('Breakpoint @ %6.6x', [pc]);
        end;
        if (FState = dsStep) then
        begin
            FillForm;
            FState := dsStep;
            Wait;
        end;
    end else
    begin
        Show;
        Wait;
    end;
end;

procedure TDebuggerForm.Dump(start: TMemoryAddress);
var
    hex, text: String;
    stop: TMemoryAddress;
    mem: TWord;
    count: Smallint;

    function FetchWord(addr: TMemoryAddress): TWord;
    var
        b: PByte;
    begin
        b := PByte(@Result);
        b^ := Core.FetchByte(0, addr + 3);
        (b + 1)^ := Core.FetchByte(0, addr + 2);
        (b + 2)^ := Core.FetchByte(0, addr + 1);
        (b + 3)^ := Core.FetchByte(0, addr);
    end;

begin
    DumpMemo.Lines.BeginUpdate;
    try
        DumpMemo.Lines.Clear;
        stop := start + 4096;
        count := 0;
        hex := Format('%6.6x * ', [start]);
        text := '';
        while (start < stop) do
        begin
            mem := FetchWord(start);
            Inc(start, 4);
            hex := hex + Format('%8.8x ', [mem]);
            text := text + Printable(mem) + ' ';
            Inc(count);
            if ((count mod 4) = 0) then
            begin
                DumpMemo.Lines.Add(hex + text);
                hex := Format('%6.6x * ', [start]);
                text := '';
            end;
        end;
        DumpMemo.SelStart := DumpMemo.Perform(EM_LINEINDEX, 0, 0);
        DumpMemo.Perform(EM_SCROLLCARET, 0, 0);
        DumpMemo.Lines.EndUpdate;
    except
        DumpMemo.Lines.EndUpdate;
    end;
end;

procedure TDebuggerForm.FillForm;
var
    r, r2: Integer;
    pc: TMemoryAddress;
    opcd: Byte;
    opcode: TOpcode;
    fal: Byte;
    fad1, fad2: THalfWord;
    l1, l2: Smallint;
    b1, b2: Byte;
    off1, off2: Smallint;
    i: Integer;
    op1, op2: String;
begin
    Pages.ActivePage := TracePage;
    CommandEdt.Text := '';
    CommandEdt.SetFocus;
    SuperBox.Checked := (PSW.Mode = pmSupervisor);
    ProgBox.Checked := (PSW.Mode = pmProgram);
    RegSuperCheck.Checked := (PSW.RegisterSet = rsSupervisor);
    RegPgmCheck.Checked := (PSW.RegisterSet = rsProgram);
    StepBox.Checked := (psSingleStep in Processor.State);
    HaltBox.Checked := (psHalted in Processor.State);
    ErrorBox.Checked := (psError in Processor.State);
    TimerBox.Checked := PSW.TimerIntEnabled;
    IOBox.Checked := PSW.IOSTIntEnabled;
    CondCodeLbl.Caption := Format('Cond. Code = %d', [PSW.CondCode]);

    for i := 1 to TraceGrid.RowCount - 1 do
    begin
        TraceGrid.Cells[0, i] := '';
        TraceGrid.Cells[1, i] := '';
    end;
    for i := 0 to FTraceList.Count - 1 do
    begin
        pc := FTraceList[i];
        if (i = (FTraceList.Count - 1)) then
            opcd := CurInst.Opcode
        else
            opcd := Core.FetchByte(0, pc);
        if (Opcodes.IsOpcode(opcd)) then
            opcode := Opcodes[opcd]
        else
            Continue;
        if (i = (FTraceList.Count - 1)) then
        begin
            fal := CurInst.AsBytes[1];
            fad1 := (CurInst.AsBytes[2] shl 8) or CurInst.AsBytes[3];
            fad2 := (CurInst.AsBytes[4] shl 8) or CurInst.AsBytes[5];
        end else
        begin
            fal := Core.FetchByte(0, pc + 1);
            fad1 := Core.FetchHalfWord(0, pc + 2);
            if (opcode.Length = 6) then
                fad2 := Core.FetchHalfWord(0, pc + 4)
            else
                fad2 := 0;
        end;
        TraceGrid.Cells[0, i + 1] := Format('%6.6x', [pc]);
        case opcode.InstType of
          itRR:
          begin
            r := (fal and $f0) shr 4;
            r2 := fal and $0f;
            if (opCode.Code = 'SVC') then
                TraceGrid.Cells[1, i + 1] := Format('%s %d', [opCode.Code, fal])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%d', [opCode.Code, r, r2]);
          end;
          itRX,
          itBranch:
          begin
            r := (fal and $F0) shr 4;
            r2 := fal and $0f;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if ((b1 < 1) and (r2 < 1)) then
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%s', [opCode.Code, r, ToHex(fad1)])
            else if (b1 < 1) then
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%s(%d)', [opCode.Code, r, ToHex(fad1), r2])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%s(%d,%d)', [opCode.Code, r, ToHex(off1), r2, b1]);
          end;
          itRS:
          begin
            r := (fal and $f0) shr 4;
            r2 := fal and $0f;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 1) then
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%d,%s', [opCode.Code, r, r2, ToHex(fad1)])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%d,%s(,%d)', [opCode.Code, r, r2, ToHex(off1), b1]);
          end;
          itSI:
          begin
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 1) then
                TraceGrid.Cells[1, i + 1] := Format('%s %s,%d', [opCode.Code, ToHex(fad1), fal])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %s(,%d),%d', [opCode.Code, ToHex(off1), b1, fal]);
          end;
          itSS1:
          begin
            l1 := fal + 1;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 1) then
                op1 := Format('%s(%d)', [ToHex(fad1), l1])
            else
                op1 := Format('%s(%d,%d)', [ToHex(off1), l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (b2 < 1) then
                op2 := Format('%s', [ToHex(fad2)])
            else
                op2 := Format('%s(,%d)', [ToHex(off2), b2]);
            TraceGrid.Cells[1, i + 1] := Format('%s %s,%s', [opCode.Code, op1, op2]);
          end;
          itSS2:
          begin
            l1 := ((fal and $F0) shr 4) + 1;
            l2 := (fal and $0F) + 1;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 1) then
                op1 := Format('%s(%d)', [ToHex(fad1), l1])
            else
                op1 := Format('%s(%d,%d)', [ToHex(off1), l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (b2 < 1) then
                op2 := Format('%s(%d)', [ToHex(fad2), l2])
            else
                op2 := Format('%s(%d,%d)', [ToHex(off2), l2, b2]);
            TraceGrid.Cells[1, i + 1] := Format('%s %s,%s', [opCode.Code, op1, op2]);
          end;
          itUnknown:    ;
        end;
    end;
    if (FTraceList.Count > 0) then
        TraceGrid.Row := FTraceList.Count;

    with BrkptGrid do
    begin
        RowCount := 2;
        for i := 0 to FBreakPoints.Count - 1 do
        begin
            if (i >= (RowCount - 1)) then
                RowCount := RowCount + 1;
            Cells[1, i + 1] := Format('%6.6x', [FBreakPoints[i]]);
        end;
    end;

    with RegGrid do
    begin
        for r := 0 to 15 do
            Cells[r mod 4, (r div 4) + 1] := Format('%8.8x', [Processor.Registers[PSW.RegisterSet, r]]);
    end;

    Dump(FDumpAddr);
end;

procedure TDebuggerForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
    FWaiting := False;
    CanClose := True;
end;

procedure TDebuggerForm.FormShow(Sender: TObject);
begin
//    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    with RegGrid do
    begin
        Cells[0, 0] := '0/4/8/12';
        Cells[1, 0] := '1/5/9/13';
        Cells[2, 0] := '2/6/10/14';
        Cells[3, 0] := '3/7/11/15';
    end;
    FillForm;
end;

procedure TDebuggerForm.ParseCommand(s: String; var cmd: String; var param1, param2: Integer);
var
    split: Integer;
begin
    cmd := ' ';
    param1 := -1;
    param2 := -1;
    s := Trim(s);
    if (s = '') then
        Exit;

    split := Pos(' ', s);
    if (split > 0) then
    begin
        cmd := Copy(s, 1, split - 1);
        s := Trim(Copy(s, split + 1));
        split := Pos(',', s);
        if (split = 0) then
        begin
            if (not TryStrToInt('$' + s, param1)) then
                cmd := ' ';
        end else
        begin
            if (not TryStrToInt('$' + Trim(Copy(s, 1, split - 1)), param1)) then
                cmd := ' ';
            if (not TryStrToInt('$' + Trim(Copy(s, split + 1)), param2)) then
                cmd := ' ';
        end;
    end;
end;

function TDebuggerForm.Printable(val: Cardinal): String;
var
    c: AnsiChar;
    i: Integer;
begin
    Result := '';
    for i := 1 to 4 do
    begin
        c := TCodeTranslator.EbcdicToAscii(val and $ff);
        if ((c < ' ') or (c > '~') or (c = '`')) then
            c := '.';
        Result := Char(c) + Result;
        val := val shr 8;
    end;
end;

procedure TDebuggerForm.SysDump(start, fin: TMemoryAddress);
var
    fout: TFileStream;
    bfr: array [0..255] of Byte;
    i: UInt32;
begin
    if (not OpenDlg.Execute) then
        Exit;
    fout := TFileStream.Create(OpenDlg.FileName, fmCreate);
    try
        while (start < fin) do
        begin
            for i := 0 to 255 do
                bfr[i] := Core.FetchByte(0, start + i);
            fout.Write(bfr, 256);
            Inc(start, 256);
        end;
    finally
        fout.Free;
    end;
end;

function TDebuggerForm.ToHex(val: Smallint): String;
const
    hexChars: array [0..15] of Char = ( '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
                                        'A', 'B', 'C', 'D', 'E', 'F' );
var
    i: Integer;
begin
    Result := '';
    for i := 1 to 4 do
    begin
        Result := hexChars[val and $0F] + Result;
        val := val shr 4;
    end;
end;

procedure TDebuggerForm.Wait;
begin
    FWaiting := True;
    while (FWaiting) do
    begin
        Application.ProcessMessages;
        Sleep(10);
    end;
end;

end.
