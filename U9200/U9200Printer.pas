unit U9200Printer;

interface

uses SysUtils, Classes, SyncObjs, U9200Types, U9200Memory, U9200Device, Generics.Collections,
    Graphics, System.Types, Printers, Forms, EmulatorTypes;

type
    TU92PrinterEventRec = record
        Command: Char;
        lineNum: Integer;
        text: AnsiString;
    end;

    TU92Printer = class(TU92Device)
    private
        FNextCommand: Byte;
        FPrintBar: Byte;
        FPrintNumeric: Boolean;
        FLPI: Byte;
        FPPI: Integer;
        FCarriageTape: TCarriageControlTape;
        FCurrentLine: Byte;
        FCurrentText: AnsiString;
        FOnHome: THomeNotifyEvent;
        FOnPrint: TPrintNotifyEvent;
        FSingleSpace: Boolean;
        FEventQueue: TQueue<TU92PrinterEventRec>;
        procedure Control;
        function DoFeed(setStatus: Boolean): Byte;
        function DoHome: Byte;
        procedure DoOnHome;
        procedure DoOnPrint;
        function GetSendToPrint: Boolean;
        procedure Print;
        procedure SetLPI(const Value: Byte);
        procedure SetSendToPrint(const Value: Boolean);
    public
        constructor Create(mem: TU92Memory); override;
        destructor Destroy; override;
        procedure Clear; override;
        procedure Execute; override;
        procedure Feed;
        procedure Home;
        function StartIO(dev, func: Byte): Byte; override;
        procedure TestIO(dev: Byte; var status, cc: Byte); override;
        property LPI: Byte read FLPI write SetLPI;
        property OnHome: THomeNotifyEvent read FOnHome write FOnHome;
        property OnPrint: TPrintNotifyEvent read FOnPrint write FOnPrint;
        property SendToPrint: Boolean read GetSendToPrint write SetSendToPrint;
        property SingleSpace: Boolean read FSingleSpace write FSingleSpace;
    end;

implementation

{ TU92Printer }

procedure TU92Printer.Clear;
begin
    Lock;
    try
        FStatusQueue.Clear;
        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

procedure TU92Printer.Control;
var
    count: Integer;
    channel: Integer;
    i: Integer;
    bcw: Cardinal;
    ovfl: Boolean;
    per: TU92PrinterEventRec;
begin
    // Grab the BCW before clearing BUSY status, otherwise
    // the processor has a chance to change the BCW before
    // we get our hands on it.
    bcw := FMemory.BCW[4];
    // Post successful status before starting to advance
    // paper, allowing the processor to issue the next
    // print command while we are busy.
    Lock;
    try
        PostStatus(0);
    finally
        Unlock;
    end;
    // Sleep(100); // Simulate 600 lines per minute
    Sleep(10);
    if ((bcw and $8000000) = 0) then
    begin
        // advance n lines
        ovfl := False;
        count := (bcw and $03000000) shr 24;
        if (FSingleSpace and (count > 0)) then
            count := 1;
        while (count > 0) do
        begin
            Inc(FCurrentLine);
            if ((FCurrentLine < FCarriageTape.Count) and ((FCarriageTape[FCurrentLine] and CHANNEL1) <> 0)) then
                ovfl := True;
            Dec(count);
        end;
        if (ovfl) then
        begin
            Lock;
            try
                FNextCommand := PRINTER_NOOP;
                PostStatus(PRINTER_OVERFLOW);
            finally
                Unlock;
            end;
        end;
    end else
    begin
        // skip to channel
        channel := $01 shl ((bcw and $07000000) shr 24);
        if (channel = CHANNEL7) then
        begin
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
            FCurrentLine := 0;
        end else
        begin
            i := FCurrentLine + 1;
            while (i <> FCurrentLine) do
            begin
                if (i >= FCarriageTape.count) then
                    i := 0;
                if ((FCarriageTape[i] and channel) <> 0) then
                begin
                    FCurrentLine := i;
                    Exit;
                end;
                Inc(i);
            end;
            Lock;
            try
                FNextCommand := PRINTER_NOOP;
                PostStatus(PRINTER_RUNAWAY);
            finally
                Unlock;
            end;
        end;
    end;
end;

constructor TU92Printer.Create(mem: TU92Memory);
begin
    inherited;
    Address := 3;
    FLPI := 6;
    FCurrentLine := 0;
    FCarriageTape := TCarriageControlTape.Create(FLPI);
    FEventQueue := TQueue<TU92PrinterEventRec>.Create;
end;

destructor TU92Printer.Destroy;
begin
    FreeAndNil(FCarriageTape);
    FreeAndNil(FEventQueue);
    inherited;
end;

function TU92Printer.DoFeed(setStatus: Boolean): Byte;
begin
    Result := 0;
    Inc(FCurrentLine);
    if (setStatus) then
    begin
        if ((FCarriageTape[FCurrentLine] and CHANNEL1) <> 0) then
            Result := Result or PRINTER_OVERFLOW
        else
            Result := Result and ($FF xor PRINTER_OVERFLOW);
    end;
end;

function TU92Printer.DoHome: Byte;
var
    i: Integer;
    per: TU92PrinterEventRec;
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
    Result := Result or PRINTER_RUNAWAY;
    FState := FState + [udsError];
end;

procedure TU92Printer.DoOnHome;
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

procedure TU92Printer.DoOnPrint;
var
    per: TU92PrinterEventRec;
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

procedure TU92Printer.Execute;
begin
    try
        while (not Terminated) do
        begin
            if (FEvent.WaitFor(100) = wrSignaled) then
            begin
                while (FCommand <> PRINTER_NOOP) do
                begin
                    Lock;
                    try
                        if ((FCommand and $80) = 0) then
                            FPrintBar := 63
                        else
                            FPrintBar := 48;
                        FPrintNumeric := ((FCommand and $40) <> 0);
                        FInhibitInt := ((FCommand and INHIBIT_INTERRUPT) <> 0);
                        FCommand := (FCommand and $07);
                    finally
                        Unlock;
                    end;
                    case FCommand of
                      PRINTER_PRINT:
                        Print;
                      PRINTER_CONTROL:
                        Control;
                      else
                        raise Exception.Create('Internal error. Unknown printer command.');
                    end;
                    Lock;
                    try
                        FCommand := PRINTER_NOOP;
                        if (FNextCommand <> PRINTER_NOOP) then
                        begin
                            FCommand := FNextCommand;
                            FNextCommand := PRINTER_NOOP;
                        end;
                    finally
                        Unlock;
                    end;
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

procedure TU92Printer.Feed;
begin
    if (udsBusy in FState) then
        Exit;
    DoFeed(False);
end;

function TU92Printer.GetSendToPrint: Boolean;
begin
    Result := Printer.Printing;
end;

procedure TU92Printer.Home;
begin
    // if (udsBusy in FState) then
    // Exit;
    DoHome;
end;

procedure TU92Printer.Print;
var
    text: AnsiString;
    i: Integer;
    margin: Integer;
    r: TRect;
    h: Integer;
    s: String;
    per: TU92PrinterEventRec;
begin
    text := '';
    if (FPrintBar = 63) then
    begin
        for i := 0 to 131 do
            text := text + TCodeTranslator.Printer63ToAscii(FMemory.PrinterImage[i]);
    end else
    begin
        for i := 0 to 131 do
            text := text + TCodeTranslator.Printer48ToAscii(FMemory.PrinterImage[i]);
    end;
    // If the user is trying to print beyond the end of the page, we need
    // to do a form feed here, even if the user hasn't asked for one, to
    // prevent data loss. If this is happening, then presumably the user
    // has chosen to ignore a prior overflow condition and just print off
    // the end of one page and onto the beginning of the next. This might
    // cause a blank page if the user suddenly decides to react to the overflow
    // condition but I guess that's just TFB.
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
        FCurrentLine := 0;
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
    Queue(DoOnPrint);
    Control;
end;

procedure TU92Printer.SetLPI(const Value: Byte);
begin
    FLPI := Value;
    FCarriageTape.lpi := FLPI;
end;

procedure TU92Printer.SetSendToPrint(const Value: Boolean);
var
    per: TU92PrinterEventRec;
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

function TU92Printer.StartIO(dev, func: Byte): Byte;
var
    cmd: Byte;
    i: Integer;
    stemp1, stemp2: String;
begin
    Lock;
    try
        // Trace StartIO request
        cmd := func and $07;
        stemp2 := '';
        if (cmd = 1) then
        begin
            for i := 0 to 20 do
                stemp2 := stemp2 + Char(TCodeTranslator.Printer63ToAscii(FMemory.PrinterImage[i]));
        end;
        stemp1 := Format('XIOF func = %2.2x bcw = %8.8x %s', [func, FMemory.BCW[4], stemp2]);
        Trace(stemp1);
        //
        if ((udsBusy in FState) or (not Ready) or (not OnLine)) then
        begin
            Result := 2;
            Trace('**Busy');
            Exit;
        end;
        if ((FStatusQueue.count > 0) and ((FStatusQueue[0].status and PRINTER_OVERFLOW) <> 0)) then
        begin
            Trace('**Overflow');
            Result := 0;
            Exit;
        end;
        if ((cmd <> 1) and (cmd <> 3)) then
        begin
            Trace('**Invalid command');
            Result := 2;
            Exit;
        end;
        FState := FState + [udsBusy];
        if (FCommand = PRINTER_NOOP) then
        begin
            FCommand := func;
            FEvent.SetEvent;
        end else
            FNextCommand := func;
        Result := 0;
    finally
        Unlock;
    end;
end;

procedure TU92Printer.TestIO(dev: Byte; var status, cc: Byte);
var
    statRec: TU92DeviceStatus;
    stemp: String;
begin
    Lock;
    try
        cc := 0;
        status := 0;
        if (FStatusQueue.count > 0) then
        begin
            statRec := FStatusQueue[0];
            if (statRec.IntPending or ((statRec.status and DEV_INT_PENDING) <> 0)) then
            begin
                status := statRec.status or DEV_INT_PENDING;
                FStatusQueue.Delete(0);
                FState := FState - [udsBusy];
                cc := 1;
            end;
        end else if (udsBusy in FState) then
        begin
            cc := 2;
        end;
        stemp := Format('TIO status=%2.2x cc=%d', [status, cc]);
        Trace(stemp);
    finally
        Unlock;
    end;
end;

end.
