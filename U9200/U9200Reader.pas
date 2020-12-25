unit U9200Reader;

interface

uses SysUtils, Classes, SyncObjs, U9200Types, U9200Memory, U9200Device, CardFile;

type
  TU92Reader = class(TU92CardDevice)
  private
    FOutputCount: Integer;
    FReadStationLoaded: Boolean;
    FReadStation: TCardRec;
    FOnFeed: TNotifyEvent;
    procedure DoFeed;
    procedure DoOnFeed;
    procedure ReadImage;
    procedure ReadTranslate;
    procedure Store(bfr: TCardRec);
  public
    constructor Create(mem: TU92Memory); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure EmptyHopper;
    procedure Feed;
    function HasCardToFeed: Boolean;
    procedure Execute; override;
    function StartIO(dev, func: Byte): Byte; override;
    procedure TestIO(dev: Byte; var status, cc: Byte); override;
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property OutputCount: Integer read FOutputCount;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

implementation

uses Math, EmulatorTypes;

{ TU92Reader }

procedure TU92Reader.Clear;
begin
    Lock;
    try
        FStatusQueue.Clear;
        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

constructor TU92Reader.Create(mem: TU92Memory);
begin
    inherited;
    Address := 1;
    FHopperEmpty := True;
end;

destructor TU92Reader.Destroy;
begin
    FreeAndNil(FCurrentFile);
    inherited;
end;

procedure TU92Reader.DoFeed;
begin
    Lock;
    try
        if (FReadStationLoaded) then
            Inc(FOutputCount);
        if ((not Assigned(FCurrentFile)) or (FCurrentFile.Eof)) then
        begin
            if (not OpenNextFile) then
            begin
                FHopperEmpty := True;
                FReadStationLoaded := False;
                Exit;
            end;
        end;
        FReadStationLoaded := True;                 // mark read station as loaded
        FCurrentFile.ReadRaw(FReadStation);
        Dec(FInputCount);                           // adjust hopper counts
        if (FInputCount < 0) then
            FInputCount := 0;
    finally
        Unlock;
        if (Assigned(FOnFeed)) then
            Queue(DoOnFeed);
    end;
//    Sleep(100);                                     // Sleep 0.1 secs to simulate 600 CPM
    Sleep(20);
end;

procedure TU92Reader.DoOnFeed;
begin
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

procedure TU92Reader.EmptyHopper;
begin
    Lock;
    try
        FFiles.Clear;
        FreeAndNil(FCurrentFile);
        FInputCount := 0;
        FOutputCount := 0;
    finally
        Unlock;
    end;
end;

procedure TU92Reader.Execute;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            case FCommand of
              READER_READ_TRANSLATE:
                ReadTranslate;
              READER_READ_IMAGE:
                ReadImage;
              READER_NOOP:
                ;
              else
                raise Exception.Create('Internal error. Unknown reader command.');
            end;
        end;
    end;
end;

procedure TU92Reader.Feed;
begin
    if (udsBusy in FState) then
        Exit;
    DoFeed;
end;

function TU92Reader.HasCardToFeed: Boolean;
begin
    Result := ((not FHopperEmpty) or FReadStationLoaded);
end;

procedure TU92Reader.ReadImage;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadImage(bfr, FReadStation);
        Store(bfr);
        DoFeed;
        PostStatus(0);
    end else
        PostStatus(READER_STACKER);
end;

procedure TU92Reader.ReadTranslate;
var
    bfr: TCardRec;
    stemp: String;
    i: Integer;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadTranslate(bfr, FReadStation);
        // Trace buffer just read
        stemp := Format('Recnum=%d ', [FCurrentFile.RecNumber]);
        for i := 1 to 20 do
            stemp := stemp + Char(TCodeTranslator.Hollerith8ToAscii(bfr.Columns[i]));
        stemp := stemp + '...';
        for i := 70 to 80 do
            stemp := stemp + Char(TCodeTranslator.Hollerith8ToAscii(bfr.Columns[i]));
        Trace(stemp);
        //
        Store(bfr);
        DoFeed;
        PostStatus(0);
    end else
        PostStatus(READER_STACKER);
end;

function TU92Reader.StartIO(dev, func: Byte): Byte;
var
    stemp: String;
begin
    Lock;
    try
        // Trace StartIO request
        stemp := Format('XIOF func = %2.2x bcw = %8.8x', [func, FMemory.BCW[1]]);
        Trace(stemp);
        //
        if ((udsBusy in FState) or (not Ready) or (not OnLine) or (not FReadStationLoaded)) then
        begin
            Result := 2;
            Trace('**Busy');
            Exit;
        end;
        FInhibitInt := ((func and INHIBIT_INTERRUPT) <> 0);
        FCommand := (func and $07);
        if ((FCommand <> 2) and (FCommand <> 6)) then
        begin
            Result := 2;
            Trace('**Invalid command');
            Exit;
        end;
        FState := FState + [udsBusy];
        FEvent.SetEvent;
        Result := 0;
    finally
        Unlock;
    end;
end;

procedure TU92Reader.Store(bfr: TCardRec);
var
    i: Integer;
    toRead, read: Cardinal;
    adr: Smallint;
    bcw: Cardinal;
begin
    bcw := FMemory.BCW[1];
    i := Low(bfr.Columns);
    toRead := Min((bcw and $FF0000) shr 16, Length(bfr.Columns));
    read := 0;
    adr := (bcw and $FFFF);
    while (read < toRead) do
    begin
        FMemory.StoreByte(adr, bfr.Columns[i]);
        Inc(i);
        Inc(adr);
        Inc(read);
    end;
    FMemory.BCW[1] := ((((bcw and $FF0000) shr 16) - read) shl 16) or adr;
end;

procedure TU92Reader.TestIO(dev: Byte; var status, cc: Byte);
var
    statRec: TU92DeviceStatus;
    stemp: String;
begin
    Lock;
    try
        cc := 0;
        status := 0;
        if (FStatusQueue.Count > 0) then
        begin
            statRec := FStatusQueue[0];
            if (statRec.IntPending or ((statRec.Status and DEV_INT_PENDING) <> 0)) then
            begin
                status := statRec.Status or DEV_INT_PENDING;
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
