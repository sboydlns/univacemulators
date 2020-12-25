unit U1005Punch;

interface

uses SysUtils, Classes, CardFile, U1005Memory, U1005Device, EmulatorTypes;

type
  T1005Punch = class(T1005CardDevice)
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
    FHopper: Integer;
    FOnFeed: TNotifyEvent;
    procedure Feed;
    function PunchInit(stacker: Integer): Boolean;
  public
    constructor Create(mem: T1005Memory); override;
    destructor Destroy; override;
    procedure Clear;
    procedure EmptyHopper(num: Integer);
    procedure EmptyHoppers;
    function Punch(stacker: Integer): Boolean;
    function PunchImage(stacker: Integer): Boolean;
    procedure SaveHopper(num: Integer; fname: String);
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property OutputCount1: Integer read FOutputCount1;
    property OutputCount2: Integer read FOutputCount2;
    property PunchStationLoaded: Boolean read FPunchStationLoaded;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

implementation

uses U1005Types;

{ T1005Punch }

procedure T1005Punch.Clear;
begin
    ;
end;

constructor T1005Punch.Create(mem: T1005Memory);
begin
    inherited Create(mem);
    FCardFileDir := UserDataDir;
    FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
    FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    FHopperEmpty := True;
end;

destructor T1005Punch.Destroy;
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
    inherited Destroy;
end;

procedure T1005Punch.EmptyHopper(num: Integer);
begin
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
end;

procedure T1005Punch.EmptyHoppers;
begin
    FReadStationLoaded := False;
    FPunchStationLoaded := False;
    FFiles.Clear;
    FreeAndNil(FCurrentFile);
    FInputCount := 0;
    FOutputCount1 := 0;
    FOutputCount2 := 0;
    FOutputFile1.Free;
    FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
    FOutputFile2.Free;
    FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
end;

procedure T1005Punch.Feed;
begin
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
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

function T1005Punch.Punch(stacker: Integer): Boolean;
var
    i: Integer;
    bfr: TCardRec;
    rawBfr: TCardRec;
    addr: I1005Addr;
begin
    Result := PunchInit(stacker);
    if (not Result) then
        Exit;
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(PUNCH_BUFFER);
    bfr.Count := 80;
    for i := 1 to 80 do
    begin
        bfr.Columns[i] := FMemory.FetchByte(addr);
        addr.Increment;
    end;
    TCardFileStream.WriteXS3(rawBfr, bfr);
    if (FHopper = 1) then
        FOutputFile1.Merge(rawBfr, FPunchStation)
    else
        FOutputFile2.Merge(rawBfr, FPunchStation);
    Feed;
end;

function T1005Punch.PunchImage(stacker: Integer): Boolean;
var
    i: Integer;
    bfr: TCardRec;
    rawBfr: TCardRec;
    addr: I1005Addr;
begin
    Result := PunchInit(stacker);
    if (not Result) then
        Exit;
    addr := T1005FedSysAddr.Create;
    addr.SetAddr(PUNCH_BUFFER);
    bfr.Count := 160;
    for i := 1 to 160 do
    begin
        bfr.Columns[i] := FMemory.FetchByte(addr);
        addr.Increment;
    end;
    TCardFileStream.WriteImage(rawBfr, bfr);
    if (FHopper = 1) then
        FOutputFile1.Merge(rawBfr, FPunchStation)
    else
        FOutputFile2.Merge(rawBfr, FPunchStation);
    Feed;
end;

function T1005Punch.PunchInit(stacker: Integer): Boolean;
begin
    if (stacker = 0) then
        FHopper := 2
    else
        FHopper := 1;
    // Feed as many cards as needed to get one into the punch station
    if (not FPunchStationLoaded) then
        Feed;
    if (not FPunchStationLoaded) then
        Feed;
    Result := FPunchStationLoaded;
end;

procedure T1005Punch.SaveHopper(num: Integer; fname: String);
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

end.
