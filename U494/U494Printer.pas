unit U494Printer;

interface

uses SysUtils, Classes, SyncObjs, U494Memory, U494Cpu, Generics.Collections,
    Graphics, System.Types, Printers, Forms, EmulatorTypes, U494Interrupts;

type
  T494PrinterEventRec = record
    Command: Char;
    lineNum: Integer;
    text: AnsiString;
  end;

  T494Printer = class(T494Device)
  private
    FLPI: Byte;
    FPPI: Integer;
    FCarriageTape: TCarriageControlTape;
    FCurrentLine: Byte;
    FCurrentText: AnsiString;
    FOnHome: THomeNotifyEvent;
    FOnPrint: TPrintNotifyEvent;
    FSingleSpace: Boolean;
    FEventQueue: TQueue<T494PrinterEventRec>;
    FFuncCode: Byte;
    FWithExtInterrupt: Boolean;
    FSpaceBefore: Integer;
    function DoFeed(setStatus: Boolean): Byte;
    function DoHome: Byte;
    procedure DoOnHome;
    procedure DoOnPrint;
    function GetSendToPrint: Boolean;
    procedure Print;
    procedure SetLPI(const Value: Byte);
    procedure SetSendToPrint(const Value: Boolean);
    procedure SpaceBefore;
    procedure Term;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure ActivateOutput(withMon: Boolean); override;
    procedure Clear; override;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    procedure Feed;
    procedure Home;
    property LPI: Byte read FLPI write SetLPI;
    property OnHome: THomeNotifyEvent read FOnHome write FOnHome;
    property OnPrint: TPrintNotifyEvent read FOnPrint write FOnPrint;
    property SendToPrint: Boolean read GetSendToPrint write SetSendToPrint;
    property SingleSpace: Boolean read FSingleSpace write FSingleSpace;
    end;

implementation

const
    // Function codes
    CPRINT = $02;
    CTERMINATE = $13;
    CINTERRUPT = $08;
    // Status codes
    CNORMAL_COMPLETION = $20;
    COUT_OF_FORMS = $24;
    CINVALID_FUNCTION = $28;
    CINERLOCK = $3C;

{ TU92Printer }

procedure T494Printer.ActivateInput(withMon: Boolean);
// Meaingless for card punch, ignore
begin
    ;
end;

procedure T494Printer.ActivateOutput(withMon: Boolean);
begin
    Lock;
    try
        FOutputMonitor := withMon;
        FOutputActive := True;
    finally
        Unlock;
    end;
end;

procedure T494Printer.Clear;
begin
end;

constructor T494Printer.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited Create(cpu, mem, chan);
    FLPI := 6;
    FCurrentLine := 0;
    FCarriageTape := TCarriageControlTape.Create(FLPI);
    FEventQueue := TQueue<T494PrinterEventRec>.Create;
end;

destructor T494Printer.Destroy;
begin
    FreeAndNil(FCarriageTape);
    FreeAndNil(FEventQueue);
    inherited;
end;

function T494Printer.DoFeed(setStatus: Boolean): Byte;
begin
    Result := 0;
    Inc(FCurrentLine);
end;

function T494Printer.DoHome: Byte;
var
    i: Integer;
    per: T494PrinterEventRec;
begin
    Result := 0;
    if (Printer.Printing) then
        Printer.NewPage;
    Lock;
    try
        per.Command := 'H';
        FEventQueue.Enqueue(per);
    finally
        Unlock;
    end;
    Queue(DoOnHome);
    i := FCurrentLine + 1;
    if (i >= FCarriageTape.Count) then
        i := 0;
    repeat
        if ((FCarriageTape[i] and CHANNEL7) <> 0) then
        begin
            FCurrentLine := i;
            Exit;
        end;
        Inc(i);
        if (i >= FCarriageTape.count) then
            i := 0;
    until i = FCurrentLine + 1;
    raise Exception.Create('Form runaway');
end;

procedure T494Printer.DoOnHome;
begin
    Lock;
    try
        FEventQueue.Dequeue;
    finally
        Unlock;
    end;
    if (Assigned(FOnHome)) then
        FOnHome(Self);
end;

procedure T494Printer.DoOnPrint;
var
    per: T494PrinterEventRec;
begin
    Lock;
    try
        per := FEventQueue.Dequeue;
    finally
        Unlock;
    end;
    if (Assigned(FOnPrint)) then
        FOnPrint(Self, per.lineNum, per.text);
end;

procedure T494Printer.Execute;
begin
    try
        while (not Terminated) do
        begin
            if (FEvent.WaitFor(100) = wrSignaled) then
            begin
                Lock;
                try
                case FFuncCode of
                  0:
                    Continue;
                  CPRINT:
                    Print;
                  CTERMINATE:
                    Term;
                  else
                  begin
                    FFuncCode := 0;
                    QueueInterrupt(intIO, IIsiExternal(FChannel), CINVALID_FUNCTION);
                  end;
                end;
                finally
                    Unlock;
                end;
            end;
        end;
    except
        on E: Exception do
        begin
            Application.ShowException(E);
        end;
    end;
end;

procedure T494Printer.ExternalFunction(func: T494Word);
begin
    Lock;
    try
        if (FFuncCode <> 0) then
        begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), CINVALID_FUNCTION);
            Exit;
        end;
        FFuncCode := (func.Value shr 24) and (not CINTERRUPT);
        FWithExtInterrupt := ((func.Value shr 24) and CINTERRUPT) <> 0;
        FSpaceBefore := (func.Value shr 18) and $3f;
        if (FFuncCode = 0) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CINVALID_FUNCTION)
        else
            FEvent.SetEvent;
    finally
        Unlock;
    end;
end;

procedure T494Printer.Feed;
begin
//    if (udsBusy in FState) then
//        Exit;
    DoFeed(False);
end;

function T494Printer.GetSendToPrint: Boolean;
begin
    Result := Printer.Printing;
end;

procedure T494Printer.Home;
begin
    // if (udsBusy in FState) then
    // Exit;
    DoHome;
end;

procedure T494Printer.Print;
var
    text: AnsiString;
    i: Integer;
    margin: Integer;
    r: TRect;
    h: Integer;
    s: String;
    per: T494PrinterEventRec;
    bcr: T494Bcr;
    word: T494Word;
begin
    // Get the text to be printed and convert to ASCII.
    bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
    i := 1;
    text := '';
    while (FOutputActive and (i <= 132) and (bcr.Count > 0)) do
    begin
        word := FMemory.Fetch(bcr.Address, True);
        text := text + AnsiChar(Chr((word.Value shr 24) and $3f));
        text := text + AnsiChar(Chr((word.Value shr 18) and $3f));
        text := text + AnsiChar(Chr((word.Value shr 12) and $3f));
        text := text + AnsiChar(Chr((word.Value shr 6) and $3f));
        text := text + AnsiChar(Chr(word.Value and $3f));
        bcr.Address := bcr.Address + 1;
        bcr.Count := bcr.Count - 1;
        FMemory.StoreBcr(BcrOut(FChannel), bcr, True);
        Inc(i, 5);
    end;
    text := TCodeTranslator.FieldataToAscii(text);
    text := AnsiString(Copy(String(text), 1, 132));
    
    SpaceBefore;
    // If the user is trying to print beyond the end of the page, we need
    // to do a form feed here, even if the user hasn't asked for one, to
    // prevent data loss.
    if (FCurrentLine >= FCarriageTape.Count) then
    begin
        if (Printer.Printing) then
        begin
            Printer.Canvas.Lock;
            try
                Printer.NewPage;
            finally
                Printer.Canvas.Unlock;
            end;
        end;
        Lock;
        try
            per.Command := 'H';
            FEventQueue.Enqueue(per);
        finally
            Unlock;
        end;
        Queue(DoOnHome);
        FSpaceBefore := FCurrentLine - FCarriageTape.Count;
        FCurrentLine := 0;
        if (FSpaceBefore <> 0) then
            SpaceBefore;
    end;
    //
    if (Printer.Printing) then
    begin
        Printer.Canvas.Lock;
        try
            h := FPPI div FLPI;
            margin := FPPI div 2;
            r := Rect(margin,
                      FCurrentLine * h,
                      Printer.PageWidth - margin,
                      ((FCurrentLine + 1) * h) - 1);
            s := String(text);
            Printer.Canvas.Font.Height := FPPI div FLPI;
            Printer.Canvas.TextRect(r, s, [tfNoPrefix]);
        finally
            Printer.Canvas.Unlock;
        end;
    end;
    FCurrentText := text;
    Lock;
    try
        per.Command := 'P';
        per.lineNum := FCurrentLine;
        per.text := text;
        FEventQueue.Enqueue(per);
    finally
        Unlock;
    end;
    FFuncCode := 0;
    Queue(DoOnPrint);
    if (FOutputMonitor and (bcr.Count = 0)) then
        QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
    if (FWithExtInterrupt) then
        QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
end;

procedure T494Printer.SetLPI(const Value: Byte);
begin
    FLPI := Value;
    FCarriageTape.lpi := FLPI;
end;

procedure T494Printer.SetSendToPrint(const Value: Boolean);
var
    per: T494PrinterEventRec;
begin
    if (Value and (not Printer.Printing)) then
    begin
        DoHome;
        Printer.Orientation := poLandscape;
        Printer.BeginDoc;
        FPPI := Printer.PageHeight div 11;
        Printer.Canvas.Brush.Style := bsClear;
        Printer.Canvas.Font.Name := 'Courier New';
        Printer.Canvas.Font.Style := [fsBold];
        Printer.Canvas.Font.Height := FPPI div FLPI;
    end else if ((not Value) and Printer.Printing) then
    begin
        Printer.EndDoc;
        Lock;
        try
            per.Command := 'H';
            FEventQueue.Enqueue(per);
        finally
            Unlock;
        end;
        Queue(DoOnHome);
    end;
end;

procedure T494Printer.SpaceBefore;
var
    count: Integer;
begin
    Sleep(10);
    // advance n lines
    count := FSpaceBefore;
    if (FSingleSpace and (count > 0)) then
        count := 1;
    Inc(FCurrentLine, count);
end;

procedure T494Printer.Term;
begin
    Lock;
    try
        TerminateOutput;
        FFuncCode := 0;
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    finally
        Unlock;
    end;
end;

end.
