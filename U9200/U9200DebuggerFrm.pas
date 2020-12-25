unit U9200DebuggerFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, U9200CPU, U9200Memory, Vcl.StdCtrls, Vcl.Grids, Vcl.ExtCtrls,
  U9200Types, Generics.Collections, Vcl.ComCtrls;

type
  TU92DebuggerState = ( udsStep, udsContinue );

  TU92Debugger = class(TForm)
    DumpMemo: TMemo;
    Bevel1: TBevel;
    ProcBox: TCheckBox;
    IOBox: TCheckBox;
    StepBox: TCheckBox;
    HaltBox: TCheckBox;
    StallBox: TCheckBox;
    ErrorBox: TCheckBox;
    FetchedBox: TCheckBox;
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
    procedure FormShow(Sender: TObject);
    procedure CommandEdtChange(Sender: TObject);
  private
    FCPU: TU92CPU;
    FMemory: TU92Memory;
    FOpcodes: TU92OpcodeList;
    FCommand: String;
    FState: TU92DebuggerState;
    FCrntOpCode: Byte;
    FTraceList: TList<Smallint>;
    FBreakPoints: TList<Smallint>;
    procedure Dump(start: Smallint);
    procedure FillForm;
    function ToHex(val: Smallint): String;
    procedure ParseCommand(s: String; var cmd: String; var param: Integer);
    function Printable(val: Cardinal): String;
    procedure Wait;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Initialize(cpu: TU92CPU; mem: TU92Memory);
    procedure DoDebug(Sender: TObject; E: Exception);
  end;

var
  U92Debugger: TU92Debugger;

implementation

uses EmulatorTypes;

{$R *.dfm}

{ TU9200Debugger }

procedure TU92Debugger.CommandEdtChange(Sender: TObject);
begin
    FCommand := UpperCase(CommandEdt.Text);
end;

constructor TU92Debugger.Create(AOwner: TComponent);
begin
    inherited;
    FCommand := '';
    FState := udsStep;
    FTraceList := TList<Smallint>.Create;
    FBreakPoints := TList<Smallint>.Create;
    FOpcodes := TU92OpcodeList.Create;
    FOpcodes.Add(Opcode($00, 4, nil, 'NOP', itRX));
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

destructor TU92Debugger.Destroy;
begin
    FreeAndNil(FOpcodes);
    FreeAndNil(FTraceList);
    FreeAndNil(FBreakPoints);
    inherited;
end;

procedure TU92Debugger.DoDebug(Sender: TObject; E: Exception);
var
    opcode: TU92Opcode;
    pc: Smallint;
begin
    if (Assigned(E)) then
    begin
        ExceptLbl.Caption := E.Message;
        FState := udsStep;
    end else
        ExceptLbl.Caption := '';
    FCrntOpCode := FMemory.FAF[[ucsProcessor]];
    opcode := FOpcodes.FindOpcode(FCrntOpCode);
    pc := FMemory.FAP[FCPU.State] - opcode.Length;
    while (FTraceList.Count >= (TraceGrid.RowCount - 1)) do
        FTraceList.Delete(0);
    FTraceList.Add(pc);
    if (Visible) then
    begin
        // If we hit an HPR, stop
        if (FCrntOpCode = $A9) then
            FState := udsStep;
        if (FBreakPoints.IndexOf(pc) <> -1) then
        begin
            FState := udsStep;
            ExceptLbl.Caption := Format('Breakpoint @ %4.4x', [pc]);
        end;
        if (FState = udsStep) then
        begin
            FillForm;
            FState := udsStep;
            Wait;
        end;
    end else
    begin
        Show;
        Wait;
    end;
end;

procedure TU92Debugger.Dump(start: Smallint);
var
    hex, text: String;
    stop: Smallint;
    mem1, mem2: Smallint;
    count: Smallint;
begin
    DumpMemo.Lines.BeginUpdate;
    try
        DumpMemo.Lines.Clear;
        start := start and $7ffe;
        stop := start + 4096;
        count := 0;
        hex := Format('%4.4x * ', [start]);
        text := '';
        while (start < stop) do
        begin
            mem1 := FMemory.FetchHalfWord(start);
            mem2 := FMemory.FetchHalfWord(start + 2);
            Inc(start, 4);
            hex := hex + ToHex(mem1) + ToHex(mem2) + ' ';
            text := text + Printable(mem1) + Printable(mem2) + ' ';
            Inc(count);
            if ((count mod 4) = 0) then
            begin
                DumpMemo.Lines.Add(hex + text);
                hex := Format('%4.4x * ', [start]);
                text := '';
            end;
        end;
        DumpMemo.SelStart := DumpMemo.Perform(EM_LINEINDEX, 0, 0);
        DumpMemo.Perform(EM_SCROLLCARET, 0, 0);
    finally
        DumpMemo.Lines.EndUpdate;
    end;
end;

procedure TU92Debugger.FillForm;
var
    r: Integer;
    pc: Smallint;
    opcd: Byte;
    opcode: TU92Opcode;
    fal: Byte;
    fad1, fad2: Smallint;
    l1, l2: Smallint;
    b1, b2: Byte;
    off1, off2: Smallint;
    i: Integer;
    op1, op2: String;
begin
    CommandEdt.Text := '';
    CommandEdt.SetFocus;
    ProcBox.Checked := (ucsProcessor in FCPU.State);
    IOBox.Checked := (ucsIO in FCPU.State);
    StepBox.Checked := (ucsSingleStep in FCPU.State);
    HaltBox.Checked := (ucsHalted in FCPU.State);
    StallBox.Checked := (ucsStalled in FCPU.State);
    ErrorBox.Checked := (ucsError in FCPU.State);
    FetchedBox.Checked := (ucsInstructionFetched in FCPU.State);

    for i := 1 to TraceGrid.RowCount - 1 do
    begin
        TraceGrid.Cells[0, i] := '';
        TraceGrid.Cells[1, i] := '';
    end;
    for i := 0 to FTraceList.Count - 1 do
    begin
        pc := FTraceList[i];
        opcd := FMemory.FetchByte(pc);
        if (FOpcodes.IsOpcode(opcd)) then
            opcode := FOpcodes.FindOpcode(opcd)
        else
            Continue;
        fal := FMemory.FetchByte(pc + 1);
        fad1 := FMemory.FetchHalfWord(pc + 2);
        if (opcode.Length = 6) then
            fad2 := FMemory.FetchHalfWord(pc + 4)
        else
            fad2 := 0;
        TraceGrid.Cells[0, i + 1] := ToHex(pc);
        case opcode.InstType of
          itRX:
          begin
            r := (fal and $F0) shr 4;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 8) then
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%s', [opCode.Code, r, ToHex(fad1)])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %d,%s(,%d)', [opCode.Code, r, ToHex(off1), b1]);
          end;
          itSI:
          begin
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 8) then
                TraceGrid.Cells[1, i + 1] := Format('%s %s,%d', [opCode.Code, ToHex(fad1), fal])
            else
                TraceGrid.Cells[1, i + 1] := Format('%s %s(,%d),%d', [opCode.Code, ToHex(off1), b1, fal]);
          end;
          itSS1:
          begin
            l1 := fal + 1;
            b1 := (fad1 and $F000) shr 12;
            off1 := fad1 and $FFF;
            if (b1 < 8) then
                op1 := Format('%s(%d)', [ToHex(fad1), l1])
            else
                op1 := Format('%s(%d,%d)', [ToHex(off1), l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (b2 < 8) then
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
            if (b1 < 8) then
                op1 := Format('%s(%d)', [ToHex(fad1), l1])
            else
                op1 := Format('%s(%d,%d)', [ToHex(off1), l1, b1]);
            b2 := (fad2 and $F000) shr 12;
            off2 := fad2 and $FFF;
            if (b2 < 8) then
                op2 := Format('%s(%d)', [ToHex(fad2), l2])
            else
                op2 := Format('%s(%d,%d)', [ToHex(off2), l2, b2]);
            TraceGrid.Cells[1, i + 1] := Format('%s %s,%s', [opCode.Code, op1, op2]);
          end;
          itBranch: ;
          itUnknown: ;
        end;
    end;
    TraceGrid.Row := FTraceList.Count;

    with BrkptGrid do
    begin
        RowCount := 2;
        for i := 0 to FBreakPoints.Count - 1 do
        begin
            if (i >= (RowCount - 1)) then
                RowCount := RowCount + 1;
            Cells[1, i + 1] := ToHex(FBreakPoints[i]);
        end;
    end;

    with RegGrid do
    begin
        Cells[1, 1] := IntToStr(FMemory.CC[[ucsProcessor]]);
        pc := FMemory.FAP[[ucsProcessor]];
        if (ucsProcessor in FCPU.State) then
            Cells[2, 1] := ToHex(pc - opcode.Length)
        else
            Cells[2, 1] := ToHex(pc);
        for r := 8 to 15 do
            Cells[3 + r - 8, 1] := ToHex(FMemory.R[[ucsProcessor], r]);
        Cells[1, 2] := IntToStr(FMemory.CC[[ucsIO]]);
        pc := FMemory.FAP[[ucsIO]];
        if (ucsIO in FCPU.State) then
            Cells[2, 2] := ToHex(pc - opcode.Length)
        else
            Cells[2, 2] := ToHex(pc);
        for r := 8 to 15 do
            Cells[3 + r - 8, 2] := ToHex(FMemory.R[[ucsIO], r]);
    end;

    Dump(0);
end;

procedure TU92Debugger.FormShow(Sender: TObject);
var
    r: Integer;
begin
    SetWindowPos(Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE);
    with RegGrid do
    begin
        Cells[1, 0] := 'CC';
        Cells[2, 0] := 'PC';
        for r := 8 to 15 do
            Cells[3 + r - 8, 0] := IntToStr(r);
        Cells[0, 1] := 'Proc';
        Cells[0, 2] := 'I/O';
    end;
    FillForm;
end;

procedure TU92Debugger.Initialize(cpu: TU92CPU; mem: TU92Memory);
begin
    FCPU := cpu;
    FMemory := mem;
end;

procedure TU92Debugger.ParseCommand(s: String; var cmd: String; var param: Integer);
var
    split: Integer;
begin
    cmd := Copy(Copy(s, 1, 1) + ' ', 1, 1);
    param := -1;
    split := Pos(' ', s);
    if (split > 0) then
    begin
        cmd := Copy(Trim(Copy(s, 1, split - 1)) + ' ', 1, 1);
        s := Trim(Copy(s, split + 1));
        if (Length(s) >= 4) then
        begin
            if (not TryStrToInt('$' + s, param)) then
            begin
                param := 0;
                cmd := ' ';
            end;
        end else
            cmd := ' ';
    end;
end;

function TU92Debugger.Printable(val: Cardinal): String;
var
    c: AnsiChar;
    i: Integer;
begin
    Result := '';
    for i := 1 to 2 do
    begin
        c := TCodeTranslator.EbcdicToAscii(val and $ff);
        if ((c < ' ') or (c > '~') or (c = '`')) then
            c := '.';
        Result := Char(c) + Result;
        val := val shr 8;
    end;
end;

function TU92Debugger.ToHex(val: Smallint): String;
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

procedure TU92Debugger.Wait;
var
    cmd: String;
    addr: Integer;
    done: Boolean;
    i: Integer;
begin
    done := False;
    while (not done) do
    begin
        while (FCommand = '') do
        begin
            Application.ProcessMessages;
            Sleep(10);
        end;
        ParseCommand(FCommand, cmd, addr);
        FCommand := '';
        case cmd[1] of
          'B':
          begin
            if (addr <> -1) then
            begin
                i := FBreakPoints.IndexOf(addr);
                if (i = -1) then
                    FBreakPoints.Add(addr)
                else
                    FBreakPoints.Delete(i);
                CommandEdt.Clear;
            end;
          end;
          'C':
          begin
            FState := udsContinue;
            done := True;
          end;
          'D':
          begin
            if (addr <> -1) then
            begin
                Dump(Smallint(addr));
                CommandEdt.Clear;
            end;
          end;
          'N':
          begin
            FState := udsStep;
            done := True;
          end;
        end;
    end;
end;

end.
