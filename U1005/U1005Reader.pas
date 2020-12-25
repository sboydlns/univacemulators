unit U1005Reader;

interface

uses SysUtils, Classes, CardFile, U1005Memory, U1005Device;

type
  T1005Reader = class(T1005CardDevice)
  private
    FReadStation: TCardRec;
    FOnFeed: TNotifyEvent;
    FReadStationLoaded: Boolean;
    FOutputCount: Integer;
  public
    procedure Clear;
    procedure EmptyHopper;
    procedure Feed;
    function Read(stopAllOnes: Boolean = False): Boolean; overload;
    function Read(var bfr: TCardRec; stopAllOnes: Boolean = False): Boolean; overload;
    function ReadImage(stopAllOnes: Boolean = False): Boolean; overload;
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property OutputCount: Integer read FOutputCount;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

implementation

uses U1005Types;

{ T1005Reader }

procedure T1005Reader.Clear;
begin
    ;
end;

procedure T1005Reader.EmptyHopper;
begin
    FReadStationLoaded := False;
    FFiles.Clear;
    FreeAndNil(FCurrentFile);
    FInputCount := 0;
    FOutputCount := 0;
    FHopperEmpty := True;
end;

procedure T1005Reader.Feed;
begin
    if (FReadStationLoaded) then
        Inc(FOutputCount);
    if ((not Assigned(FCurrentFile)) or (FCurrentFile.Eof)) then
    begin
        if (not OpenNextFile) then
        begin
            FHopperEmpty := True;
            FReadStationLoaded := False;
            if (Assigned(FOnFeed)) then
                FOnFeed(Self);
            Exit;
        end;
    end;
    FReadStationLoaded := True;                 // mark read station as loaded
    FCurrentFile.ReadRaw(FReadStation);
    Dec(FInputCount);                           // adjust hopper counts
    if (FInputCount < 0) then
        FInputCount := 0;
    FHopperEmpty := (FInputCount = 0);
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

function T1005Reader.Read(var bfr: TCardRec; stopAllOnes: Boolean): Boolean;
var
    i: Integer;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadXS3(bfr, FReadStation);
        if (stopAllOnes) then
        begin
            for i := 1 to bfr.Count do
            begin
                if (bfr.Columns[i] = $3f) then
                    bfr.Count := i - 1;
            end;
        end;
        Feed;
        Result := True;
    end else
        Result := False;
end;

function T1005Reader.Read(stopAllOnes: Boolean): Boolean;
var
    bfr: TCardRec;
    addr: I1005Addr;
    i: Integer;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadXS3(bfr, FReadStation);
        addr := T1005FedSysAddr.Create;
        addr.SetAddr(READER_BUFFER);
        for i := 1 to bfr.Count do
        begin
            if (stopAllOnes and (bfr.Columns[i] = $3f)) then
                Break;
            FMemory.StoreByte(addr, bfr.Columns[i]);
            addr.Increment;
        end;
        Feed;
        Result := True;
    end else
        Result := False;
end;

function T1005Reader.ReadImage(stopAllOnes: Boolean = False): Boolean;
var
    bfr: TCardRec;
    addr: I1005Addr;
    i: Integer;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadImage(bfr, FReadStation);
        addr := T1005FedSysAddr.Create;
        addr.SetAddr(READER_BUFFER);
        for i := 1 to bfr.Count do
        begin
            if (stopAllOnes and (bfr.Columns[i] = $3f)) then
                Break;
            FMemory.StoreByte(addr, bfr.Columns[i]);
            addr.Increment;
        end;
        Feed;
        Result := True;
    end else
        Result := False;
end;

end.
