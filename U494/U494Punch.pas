unit U494Punch;

interface

uses SysUtils, Classes, SyncObjs,U494Memory, U494Cpu, CardFile,
     Generics.Collections;

type
  T494Punch = class(T494CardDevice)
  private
    FOutputFile1: TCardFileStream;
    FOutputFile2: TCardFileStream;
    FCardFileDir: String;
    FStacker0Count: Integer;
    FStacker1Count: Integer;
    FPunchStationLoaded: Boolean;
    FFuncCode: Byte;
    FWithExtInterrupt: Boolean;
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
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure Clear; override;
    procedure EmptyHopper(num: Integer);
    procedure EmptyHoppers;
    procedure Feed;
    function HasCardToFeed: Boolean;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    procedure SaveHopper(num: Integer; fname: String);
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property Stacker0Count: Integer read FStacker0Count;
    property Stacker1Count: Integer read FStacker1Count;
    property PunchStationLoaded: Boolean read FPunchStationLoaded;
  end;

implementation

uses Math, EmulatorTypes, U494Interrupts;

{ TU92Punch }

procedure T494Punch.ActivateInput(withMon: Boolean);
// Meaingless for card punch, ignore
begin
    ;
end;

procedure T494Punch.Clear;
begin
    Lock;
    try
//        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

constructor T494Punch.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited;
    FCardFileDir := UserDataDir;
    FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
    FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    FHopperEmpty := True;
end;

destructor T494Punch.Destroy;
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

procedure T494Punch.DoFeed;
begin
    Lock;
    try
        if (FPunchStationLoaded) then
        begin
            if (FHopper = 1) then
            begin
                Inc(FStacker0Count);
            end else
            begin
                Inc(FStacker1Count);
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

procedure T494Punch.DoOnFeed;
begin
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

procedure T494Punch.EmptyHopper(num: Integer);
begin
    Lock;
    try
        if (num = 1) then
        begin
            FStacker0Count := 0;
            FOutputFile1.Free;
            FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end else
        begin
            FStacker1Count := 0;
            FOutputFile2.Free;
            FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end;
    finally
        Unlock;
    end;
end;

procedure T494Punch.EmptyHoppers;
begin
    Lock;
    try
        FFiles.Clear;
        FreeAndNil(FCurrentFile);
        FInputCount := 0;
        FStacker0Count := 0;
        FStacker1Count := 0;
        FOutputFile1.Free;
        FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        FOutputFile2.Free;
        FOutputFile2 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    finally
        Unlock;
    end;
end;

procedure T494Punch.Execute;
var
    stat: Byte;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
//            case FCommand of
//              PUNCH_READ:
//              begin
//                if (FImageMode) then
//                    stat := ReadImage
//                else
//                    stat := ReadTranslate;
//                if (stat = 0) then
//                    SavePunchStation;
//                DoFeed;
//                PostStatus(stat);
//              end;
//              PUNCH_PUNCH:
//              begin
//                if (FImageMode) then
//                    stat := PunchImage
//                else
//                    stat := PunchTranslate;
//                DoFeed;
//                PostStatus(stat);
//              end;
//              PUNCH_READ_PUNCH:
//              begin
//                if (FImageMode) then
//                begin
//                    stat := PunchImage;
//                    if (stat = 0) then
//                        stat := ReadImage;
//                end else
//                begin
//                    stat := PunchTranslate;
//                    if (stat = 0) then
//                        stat := ReadTranslate;
//                end;
//                DoFeed;
//                PostStatus(stat);
//              end;
//              PUNCH_NOOP:
//                ;
//              else
//                raise Exception.Create('Internal error. Unknown punch command.');
//            end;
        end;
    end;
end;

procedure T494Punch.ExternalFunction(func: T494Word);
begin
    Lock;
    try
        if (FFuncCode <> 0) then
        begin
            QueueInterrupt(intIO, IIsiExternal, CINAPPROPRAITE_FUNCTION);
            Exit;
        end;
        FFuncCode := (func.Value shr 24) and (not CINTERRUPT);
        FWithExtInterrupt := ((func.Value shr 24) and CINTERRUPT) <> 0;
        case FFuncCode of
          0:
          begin
            QueueInterrupt(intIO, IIsiExternal, CILLEGAL_FUNCTION);
            Exit;
          end;
          CREAD_TRANSLATE:
          begin
            FReadType := rtTranslate;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal, CNORMAL_COMPLETION);
          end;
          CREAD_IMAGE_COL:
          begin
            FReadType := rtImageCol;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal, CNORMAL_COMPLETION);
          end;
          CREAD_IMAGE_ROW:
          begin
            FReadType := rtImageRow;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal, CNORMAL_COMPLETION);
          end;
          else
          begin
            FEvent.SetEvent;
          end;
        end;
    finally
        Unlock;
    end;
end;

procedure T494Punch.Feed;
begin
//    if (udsBusy in FState) then
//        Exit;
    FHopper := 1;
    DoFeed;
end;

procedure T494Punch.Fetch(var bfr: TCardRec);
var
    bcw: Cardinal;
    i: Integer;
    toRead, read: Cardinal;
    adr: Smallint;
begin
    bfr.Clear;
//    bcw := FMemory.BCW[3];
    i := Low(bfr.Columns);
    toRead := Min((bcw and $FF0000) shr 16, Length(bfr.Columns));
    read := 0;
    adr := (bcw and $FFFF);
    while (read < toRead) do
    begin
//        bfr.Columns[i] := FMemory.FetchByte(adr);
        Inc(i);
        Inc(adr);
        Inc(read);
    end;
    bfr.Count := read;
//    FMemory.BCW[3] := ((((bcw and $FF0000) shr 16) - read) shl 16) or adr;
end;

function T494Punch.HasCardToFeed: Boolean;
begin
    Result := ((not FHopperEmpty) or FReadStationLoaded or FPunchStationLoaded);
end;

function T494Punch.PunchImage: Byte;
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
//        Result := PUNCH_STACKER;
end;

function T494Punch.PunchTranslate: Byte;
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
//        Trace(stemp);
        //
        TCardFileStream.WriteTranslate(rawBfr, bfr);
        if (FHopper = 1) then
            FOutputFile1.Merge(rawBfr, FPunchStation)
        else
            FOutputFile2.Merge(rawBfr, FPunchStation);
        Result := 0;
    end else
//        Result := PUNCH_STACKER;
end;

function T494Punch.ReadImage: Byte;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadImage(bfr, FReadStation);
        Store(bfr);
        Result := 0;
    end else
//        Result := PUNCH_STACKER;
end;

function T494Punch.ReadTranslate: Byte;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadTranslate(bfr, FReadStation);
        Store(bfr);
        Result := 0;
    end else
//        Result := PUNCH_STACKER;
end;

procedure T494Punch.SaveHopper(num: Integer; fname: String);
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

procedure T494Punch.SavePunchStation;
begin
    if (FPunchStationLoaded) then
    begin
        if (FHopper = 1) then
            FOutputFile1.WriteRaw(FPunchStation)
        else
            FOutputFile2.WriteRaw(FPunchStation);
    end;
end;

procedure T494Punch.Store(bfr: TCardRec);
var
    i: Integer;
    toRead, read: Cardinal;
    adr: Smallint;
    bcw: Cardinal;
begin
//    bcw := FMemory.BCW[3];
    i := Low(bfr.Columns);
    toRead := Min((bcw and $FF0000) shr 16, Length(bfr.Columns));
    read := 0;
    adr := (bcw and $FFFF);
    while (read < toRead) do
    begin
//        FMemory.StoreByte(adr, bfr.Columns[i]);
        Inc(i);
        Inc(adr);
        Inc(read);
    end;
//    FMemory.BCW[3] := ((((bcw and $FF0000) shr 16) - read) shl 16) or adr;
end;

end.
