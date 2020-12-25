unit U9200Punch;

interface

uses SysUtils, Classes, SyncObjs, U9200Types, U9200Memory, U9200Device, CardFile,
     Generics.Collections;

type
  TU92Punch = class(TU92CardDevice)
  private
    FOutputFile1: TCardFileStream;
    FOutputFile2: TCardFileStream;
    FCardFileDir: String;
    FOutputCount1: Integer;
    FOutputCount2: Integer;
    FReadStationLoaded: Boolean;
    FReadStation: TCardRec;
    FPunchStationLoaded: Boolean;
    FPunchStation: TCardRec;
    FImageMode: Boolean;
    FHopper: Integer;
    FOnFeed: TNotifyEvent;
    procedure DoFeed;
    procedure DoOnFeed;
    procedure Fetch(var bfr: TCardRec);
    function PunchImage: Byte;
    function PunchTranslate: Byte;
    function ReadImage: Byte;
    function ReadTranslate: Byte;
    procedure SavePunchStation;
    procedure Store(bfr: TCardRec);
  public
    constructor Create(mem: TU92Memory); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure EmptyHopper(num: Integer);
    procedure EmptyHoppers;
    procedure Feed;
    function HasCardToFeed: Boolean;
    procedure Execute; override;
    procedure SaveHopper(num: Integer; fname: String);
    function StartIO(dev, func: Byte): Byte; override;
    procedure TestIO(dev: Byte; var status, cc: Byte); override;
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property OutputCount1: Integer read FOutputCount1;
    property OutputCount2: Integer read FOutputCount2;
    property PunchStationLoaded: Boolean read FPunchStationLoaded;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

implementation

uses Math, EmulatorTypes;

{ TU92Punch }

procedure TU92Punch.Clear;
begin
    Lock;
    try
        FStatusQueue.Clear;
        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

constructor TU92Punch.Create(mem: TU92Memory);
begin
    inherited;
    FCardFileDir := UserDataDir;
    FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
    FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    Address := 2;
    FHopperEmpty := True;
end;

destructor TU92Punch.Destroy;
var
    fname: String;
begin
    fname := FOutputFile1.FileName;
    FreeAndNil(FOutputFile1);
    DeleteFile(fname);
    fname := FOutputFile2.FileName;
    FreeAndNil(FOutputFile2);
    DeleteFile(fname);
    FreeAndNil(FCurrentFile);
    inherited;
end;

procedure TU92Punch.DoFeed;
begin
    Lock;
    try
        if (FPunchStationLoaded) then
        begin
            if (FHopper = 1) then
            begin
                Inc(FOutputCount1);
            end else
            begin
                Inc(FOutputCount2);
            end;
            FPunchStationLoaded := False;
        end;
        if (FReadStationLoaded) then
        begin
            FPunchStation := FReadStation;
            FPunchStationLoaded := True;
        end;
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
    Sleep(10);
end;

procedure TU92Punch.DoOnFeed;
begin
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

procedure TU92Punch.EmptyHopper(num: Integer);
begin
    Lock;
    try
        if (num = 1) then
        begin
            FOutputCount1 := 0;
            FOutputFile1.Free;
            FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end else
        begin
            FOutputCount2 := 0;
            FOutputFile2.Free;
            FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end;
    finally
        Unlock;
    end;
end;

procedure TU92Punch.EmptyHoppers;
begin
    Lock;
    try
        FFiles.Clear;
        FreeAndNil(FCurrentFile);
        FInputCount := 0;
        FOutputCount1 := 0;
        FOutputCount2 := 0;
        FOutputFile1.Free;
        FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        FOutputFile2.Free;
        FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    finally
        Unlock;
    end;
end;

procedure TU92Punch.Execute;
var
    stat: Byte;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            case FCommand of
              PUNCH_READ:
              begin
                if (FImageMode) then
                    stat := ReadImage
                else
                    stat := ReadTranslate;
                if (stat = 0) then
                    SavePunchStation;
                DoFeed;
                PostStatus(stat);
              end;
              PUNCH_PUNCH:
              begin
                if (FImageMode) then
                    stat := PunchImage
                else
                    stat := PunchTranslate;
                DoFeed;
                PostStatus(stat);
              end;
              PUNCH_READ_PUNCH:
              begin
                if (FImageMode) then
                begin
                    stat := PunchImage;
                    if (stat = 0) then
                        stat := ReadImage;
                end else
                begin
                    stat := PunchTranslate;
                    if (stat = 0) then
                        stat := ReadTranslate;
                end;
                DoFeed;
                PostStatus(stat);
              end;
              PUNCH_NOOP:
                ;
              else
                raise Exception.Create('Internal error. Unknown punch command.');
            end;
        end;
    end;
end;

procedure TU92Punch.Feed;
begin
    if (udsBusy in FState) then
        Exit;
    FHopper := 1;
    DoFeed;
end;

procedure TU92Punch.Fetch(var bfr: TCardRec);
var
    bcw: Cardinal;
    i: Integer;
    toRead, read: Cardinal;
    adr: Smallint;
begin
    bfr.Clear;
    bcw := FMemory.BCW[3];
    i := Low(bfr.Columns);
    toRead := Min((bcw and $FF0000) shr 16, Length(bfr.Columns));
    read := 0;
    adr := (bcw and $FFFF);
    while (read < toRead) do
    begin
        bfr.Columns[i] := FMemory.FetchByte(adr);
        Inc(i);
        Inc(adr);
        Inc(read);
    end;
    bfr.Count := read;
    FMemory.BCW[3] := ((((bcw and $FF0000) shr 16) - read) shl 16) or adr;
end;

function TU92Punch.HasCardToFeed: Boolean;
begin
    Result := ((not FHopperEmpty) or FReadStationLoaded or FPunchStationLoaded);
end;

function TU92Punch.PunchImage: Byte;
var
    bfr: TCardRec;
    rawBfr: TCardRec;
begin
    if (FPunchStationLoaded) then
    begin
        Fetch(bfr);
        TCardFileStream.WriteImage(rawBfr, bfr);
        if (FHopper = 1) then
            FOutputFile1.Merge(rawBfr, FPunchStation)
        else
            FOutputFile2.Merge(rawBfr, FPunchStation);
        Result := 0;
    end else
        Result := PUNCH_STACKER;
end;

function TU92Punch.PunchTranslate: Byte;
var
    bfr: TCardRec;
    rawBfr: TCardRec;
    stemp: String;
    i: Integer;
begin
    if (FPunchStationLoaded) then
    begin
        Fetch(bfr);
        // Trace buffer about to be punched
        if (FHopper = 1) then
            stemp := Format('Recnum=%d ', [FOutputFile1.RecNumber])
        else
            stemp := Format('Recnum=%d ', [FOutputFile2.RecNumber]);
        for i := 1 to 20 do
            stemp := stemp + Char(TCodeTranslator.Hollerith8ToAscii(bfr.Columns[i]));
        stemp := stemp + '...';
        for i := 70 to 80 do
            stemp := stemp + Char(TCodeTranslator.Hollerith8ToAscii(bfr.Columns[i]));
        Trace(stemp);
        //
        TCardFileStream.WriteTranslate(rawBfr, bfr);
        if (FHopper = 1) then
            FOutputFile1.Merge(rawBfr, FPunchStation)
        else
            FOutputFile2.Merge(rawBfr, FPunchStation);
        Result := 0;
    end else
        Result := PUNCH_STACKER;
end;

function TU92Punch.ReadImage: Byte;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadImage(bfr, FReadStation);
        Store(bfr);
        Result := 0;
    end else
        Result := PUNCH_STACKER;
end;

function TU92Punch.ReadTranslate: Byte;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadTranslate(bfr, FReadStation);
        Store(bfr);
        Result := 0;
    end else
        Result := PUNCH_STACKER;
end;

procedure TU92Punch.SaveHopper(num: Integer; fname: String);
var
    split: Integer;
begin
    split := LastDelimiter('.', fname);
    if (split = 0) then
        fname := fname + '.h16';
    if (num = 1) then
        FOutputFile1.SaveToFile(fname)
    else
        FOutputFile2.SaveToFile(fname);
end;

procedure TU92Punch.SavePunchStation;
begin
    if (FPunchStationLoaded) then
    begin
        if (FHopper = 1) then
            FOutputFile1.WriteRaw(FPunchStation)
        else
            FOutputFile2.WriteRaw(FPunchStation);
    end;
end;

function TU92Punch.StartIO(dev, func: Byte): Byte;
var
    stemp: String;
begin
    Lock;
    try
        // Trace StartIO request
        FInhibitInt := ((func and INHIBIT_INTERRUPT) <> 0);
        FImageMode := ((func and PUNCH_IMAGE_MODE) <> 0);
        if ((func and PUNCH_HOPPER2) = 0) then
            FHopper := 1
        else
            FHopper := 2;
        FCommand := (func and $03);
        stemp := Format('XIOF func = %2.2x hopper = %d bcw = %8.8x', [func, FHopper, FMemory.BCW[3]]);
        Trace(stemp);
        //
        if ((udsBusy in FState) or (not Ready) or (not OnLine) or (not FReadStationLoaded)) then
        begin
            Result := 2;
            Trace('**Busy');
            Exit;
        end;
        if (FCommand = 0) then
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

procedure TU92Punch.Store(bfr: TCardRec);
var
    i: Integer;
    toRead, read: Cardinal;
    adr: Smallint;
    bcw: Cardinal;
begin
    bcw := FMemory.BCW[3];
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
    FMemory.BCW[3] := ((((bcw and $FF0000) shr 16) - read) shl 16) or adr;
end;

procedure TU92Punch.TestIO(dev: Byte; var status, cc: Byte);
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
