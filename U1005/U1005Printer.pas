unit U1005Printer;

interface

uses SysUtils, Types, Printers, Graphics, EmulatorTypes, U1005Memory, U1005Device;

type
  T1005Printer = class(T1005Device)
  private
    FCarriageTape: TCarriageControlTape;
    FCurrentLine: Byte;
    FLPI: Byte;
    FPPI: Integer;
    FSingleSpace: Boolean;
    FOnHome: THomeNotifyEvent;
    FOnPrint: TPrintNotifyEvent;
    procedure DoHome;
    procedure DoPrint(lineNum: Integer; text: AnsiString);
    procedure SetLPI(const Value: Byte);
    function GetSendToPrint: Boolean;
    procedure SetSendToPrint(const Value: Boolean);
  public
    constructor Create(mem: T1005Memory); override;
    destructor Destroy; override;
    procedure Clear;
    procedure Print(maxColumns: Smallint = 132);
    procedure Skip(channel: Integer);
    function Space(count: Integer): Boolean;
    property LPI: Byte read FLPI write SetLPI;
    property OnHome: THomeNotifyEvent read FOnHome write FOnHome;
    property OnPrint: TPrintNotifyEvent read FOnPrint write FOnPrint;
    property SingleSpace: Boolean read FSingleSpace write FSingleSpace;
    property SendToPrint: Boolean read GetSendToPrint write SetSendToPrint;
  end;

implementation

uses U1005Types;

{ T1005Printer }

procedure T1005Printer.Clear;
begin
    ;
end;

constructor T1005Printer.Create(mem: T1005Memory);
begin
    inherited;
    FLPI := 6;
    FCurrentLine := 0;
    FCarriageTape := TCarriageControlTape.Create(FLPI);
end;

destructor T1005Printer.Destroy;
begin
    FreeAndNil(FCarriageTape);
    inherited;
end;

procedure T1005Printer.DoHome;
begin
    if (Assigned(FOnHome)) then
        FOnHome(Self);
end;

procedure T1005Printer.DoPrint(lineNum: Integer; text: AnsiString);
begin
    if (Assigned(FOnPrint)) then
        FOnPrint(Self, lineNum, text);
end;

function T1005Printer.GetSendToPrint: Boolean;
begin
    Result := Printer.Printing;
end;

procedure T1005Printer.Print(maxColumns: Smallint);
var
    text: AnsiString;
    count: Integer;
    margin: Integer;
    r: TRect;
    h: Integer;
    s: String;
    addr: I1005Addr;
begin
    text := '';
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(PRINTER_BUFFER);
    for count := 1 to maxColumns do
    begin
        text := text + TCodeTranslator.XS3ToAscii(FMemory.FetchByte(addr));
        addr.Increment;
    end;
    // If the user is trying to print beyond the end of the page, we need
    // to do a form feed here, even if the user hasn't asked for one, to
    // prevent data loss. If this is happening, then presumably the user
    // has chosen to ignore a prior overflow condition and just print off
    // the end of one page and onto the beginning of the next.
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
        DoHome;
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
    DoPrint(FCurrentLine, text);
end;

procedure T1005Printer.SetLPI(const Value: Byte);
begin
    FLPI := Value;
    FCarriageTape.lpi := FLPI;
end;

procedure T1005Printer.SetSendToPrint(const Value: Boolean);
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
        DoHome;
    end;
end;

procedure T1005Printer.Skip(channel: Integer);
var
    i: Integer;
begin
    channel := 1 shl channel;               // cvt channel # to carriage tape bit position
    if (channel = CHANNEL7) then
    begin
        if (Printer.Printing) then
            Printer.NewPage;
        DoHome;
        FCurrentLine := 0;
    end else
    begin
        i := FCurrentLine + 1;
        while (i <> FCurrentLine) do
        begin
            if (i >= FCarriageTape.Count) then
                i := 0;
            if ((FCarriageTape[i] and channel) <> 0) then
            begin
                FCurrentLine := i;
                Exit;
            end;
            Inc(i);
        end;
    end;
    FMemory.StoreIndicator(IND_GREATER);
end;

function T1005Printer.Space(count: Integer): Boolean;
// Space count lines and return True if overflow.
var
    ovfl: Boolean;
begin
    ovfl := False;
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
        if (FMemory.SystemType = stFedSys)  then
            FMemory.StoreIndicator(IND_LESS);
        Result := True;
    end else
    begin
        if (FMemory.SystemType = stFedSys)  then
            FMemory.StoreIndicator(IND_GREATER);
        Result := False;
    end;
end;

end.
