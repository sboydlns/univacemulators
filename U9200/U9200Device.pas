unit U9200Device;

interface

uses SysUtils, Classes, SyncObjs, Generics.Collections, U9200Memory, CardFile;

const
    INHIBIT_INTERRUPT = $10;
    DEV_INT_PENDING = $04;

    // Reader status bits
    READER_JAM = $80;
    READER_STACKER = $40;
    READER_INT_PENDING = DEV_INT_PENDING;

    // Punch status bits
    PUNCH_JAM = $80;
    PUNCH_CHECK = $20;
    PUNCH_PARITY = $10;
    PUNCH_PHOTOCELL_CHECK = $08;
    PUNCH_INT_PENDING = DEV_INT_PENDING;
    PUNCH_STACKER = $02;

    // Printer status bits
    PRINTER_NOT_READY = $80;
    PRINTER_RUNAWAY = $40;
    PRINTER_MEM_OVERLOAD = $20;
    PRINTER_PARITY = $10;
    PRINTER_BAR_MISMATCH = $08;
    PRINTER_INT_PENDING = $04;
    PRINTER_OVERFLOW = $02;
    PRINTER_PAPER_LOW = $01;

    // Reader commands
    READER_NOOP = $00;
    READER_READ_TRANSLATE = $02;
    READER_READ_IMAGE = $06;

    // Punch commands
    PUNCH_NOOP = $00;
    PUNCH_PUNCH = $01;
    PUNCH_READ = $02;
    PUNCH_READ_PUNCH = $03;
    PUNCH_IMAGE_MODE = $04;
    PUNCH_HOPPER2 = $08;

    // Printer commands
    PRINTER_NOOP = $00;
    PRINTER_PRINT = $01;
    PRINTER_CONTROL = $03;

type
  TU92DeviceStates = ( udsReady, udsOnLine, udsBusy, udsError );
  TU92DeviceState = set of TU92DeviceStates;

  TU92DeviceStatus = record
    Status: Byte;
    Device: Byte;
    IntPending: Boolean;
  end;

  TU92DeviceStatusQueue = class(TList<TU92DeviceStatus>)
  procedure Delete(idx: Integer); reintroduce;
  end;

  // An abstract class use as the base class for all I/O devices
  TU92Device = class(TThread)
  private
    procedure SetAddress(const Value: Byte);
  protected
    FMemory: TU92Memory;
    FAddress: Byte;
    FLock: TCriticalSection;
    FState: TU92DeviceState;
    FEvent: TEvent;
    FCommand: Byte;
    FInhibitInt: Boolean;
    FStatusQueue: TU92DeviceStatusQueue;
    FIOTrace: TFileStream;
    FLastTraceMsg: String;
    function GetIntPending: Boolean; virtual;
    function GetOnLine: Boolean; virtual;
    function GetReady: Boolean; virtual;
    function GetStatus: TU92DeviceStatus; virtual;
    procedure Lock;
    procedure PostStatus(stat: Byte); virtual;
    procedure SetOnLine(Value: Boolean); virtual;
    procedure Trace(msg: String);
    Procedure Unlock;
  public
    constructor Create(mem: TU92Memory); reintroduce; virtual;
    destructor Destroy; override;
    procedure Clear; virtual; abstract;
    procedure IntClear; virtual;
    function StartIO(dev, func: Byte): Byte; virtual; abstract;
    procedure TestIO(dev: Byte; var status, cc: Byte); virtual; abstract;
    property Address: Byte read FAddress write SetAddress;
    property IntPending: Boolean read GetIntPending;
    property OnLine: Boolean read GetOnLine write SetOnLine;
    property Ready: Boolean read GetReady;
    property Status: TU92DeviceStatus read GetStatus;
  end;

  TU92DeviceList = class(TObjectList<TU92Device>)
  private
    function GetDevices(dev: Byte): TU92Device;
  public
    constructor Create;
    property Devices[dev: Byte]: TU92Device read GetDevices;
  end;

  TU92CardDevice = class(TU92Device)
  protected
    FFiles: TCardFileList;
    FCurrentFile: TCardFileStream;
    FInputCount: Integer;
    FHopperEmpty: Boolean;
    function OpenNextFile: Boolean; virtual;
  public
    constructor Create(mem: TU92Memory); override;
    destructor Destroy; override;
    procedure AddFile(fname: String; rpgType: String = ''); virtual;
    procedure AddBlankCards(count: Integer); virtual;
    property HopperEmpty: Boolean read FHopperEmpty;
    property InputCount: Integer read FInputCount;
  end;

implementation

{ TU92DeviceList }

uses U9200Config;

constructor TU92DeviceList.Create;
begin
    inherited;
    OwnsObjects := False;
end;

function TU92DeviceList.GetDevices(dev: Byte): TU92Device;
begin
    for Result in Self do
    begin
        if (Result.Address = dev) then
            Exit;
    end;
    Result := nil;
end;

{ TU92Device }

constructor TU92Device.Create(mem: TU92Memory);
begin
    inherited Create(False);
    FMemory := mem;
    FLock := TCriticalSection.Create;
    FEvent := TEvent.Create(nil, False, False, '');
    FStatusQueue := TU92DeviceStatusQueue.Create;
    FState := [udsReady];
end;

destructor TU92Device.Destroy;
begin
    FreeAndNil(FLock);
    FreeAndNil(FEvent);
    FreeAndNil(FStatusQueue);
    FreeAndNil(FIOTrace);
    inherited;
end;

function TU92Device.GetIntPending: Boolean;
begin
    Result := ((FStatusQueue.Count > 0) and FStatusQueue[0].IntPending);
end;

function TU92Device.GetOnLine: Boolean;
begin
    Result := ((udsReady in FState) and (udsOnLine in FState));
end;

function TU92Device.GetReady: Boolean;
begin
    Result := (udsReady in FState);
end;

function TU92Device.GetStatus: TU92DeviceStatus;
begin
    Result.Device := 0;
    Result.Status := 0;
    Result.IntPending := False;
    Lock;
    try
        if (FStatusQueue.Count > 0) then
            Result := FStatusQueue[0];
    finally
        Unlock;
    end;
end;

procedure TU92Device.IntClear;
begin
    Lock;
    try
        if (FStatusQueue.Count > 0) then
            FStatusQueue.Delete(0);
    finally
        Unlock;
    end;
end;

procedure TU92Device.Lock;
begin
    FLock.Enter;
end;

procedure TU92Device.PostStatus(stat: Byte);
var
    statRec: TU92DeviceStatus;
begin
    Lock;
    try
        statRec.Status := stat;
        statRec.IntPending := False;
        if (FInhibitInt) then
            statRec.Status := statRec.Status or DEV_INT_PENDING
        else
            statRec.IntPending := True;
        FStatusQueue.Add(statRec);
    finally
        Unlock;
    end;
end;

procedure TU92Device.SetAddress(const Value: Byte);
begin
    FAddress := Value;
    if (gConfig.IOTraceEnabled) then
        FIOTrace := TFileStream.Create(Format('%s\IOTrace_%2.2x.txt', [gConfig.TraceDir, FAddress]), fmCreate);
end;

procedure TU92Device.SetOnLine(Value: Boolean);
begin
    if (Value) then
        FState := FState + [udsOnLine]
    else
        FState := FState - [udsOnLine];
end;

procedure TU92Device.Trace(msg: String);
begin
    if (Assigned(FIOTrace)) then
    begin
        if (msg <> FLastTraceMsg) then
        begin
            FIOTrace.Write(PAnsiChar(AnsiString(msg))^, Length(msg));
            FIOTrace.Write(PAnsiChar(AnsiString(#13#10))^, 2);
            FLastTraceMsg := msg;
        end;
    end;
end;

procedure TU92Device.Unlock;
begin
    FLock.Leave;
end;

{ TU92DeviceStatusQueue }

procedure TU92DeviceStatusQueue.Delete(idx: Integer);
begin
  inherited Delete(idx);
end;

{ TU92CardDevice }

procedure TU92CardDevice.AddBlankCards(count: Integer);
var
    cfr: TCardFileRec;
begin
    Lock;
    try
        Inc(FInputCount, count);
        FHopperEmpty := False;
        cfr.FileName := '';
        cfr.BlankCards := count;
        FFiles.Add(cfr);
    finally
        Unlock;
    end;
end;

procedure TU92CardDevice.AddFile(fname: String; rpgType: String);
var
    fin: TCardFileStream;
    cfr: TCardFileRec;
    cclIn: TCCLStream;
    cclr: TCCLRec;
    extn: String;
    rootDir: String;
    itemp: Integer;
begin
    extn := LowerCase(ExtractFileExt(fname));
    if (extn = '.ccl') then
    begin
        rootDir := '.';
        cclIn := TCCLStream.Create(fname, fmOpenRead);
        try
            while (cclIn.Read(cclr)) do
            begin
                case cclr.FileType of
                  ctRootDir:
                  begin
                    rootDir := cclr.Name;
                  end;
                  ctData:
                  begin
                    if ((Pos(':', cclr.Name) <> 2) and (Pos('\', cclr.Name) <> 1)) then
                        cclr.Name := rootDir + '\' + cclr.Name;
                    AddFile(cclr.Name, cclr.RPGType);
                  end;
                  ctBlanks:
                  begin
                    if (TryStrToInt(cclr.Name, itemp)) then
                        AddBlankCards(itemp)
                    else
                        raise Exception.CreateFmt('Invalid # of blank cards in /BLANKS command (%s)',
                                                  [cclr.Name]);
                  end;
                end;
            end;
        finally
            cclIn.Free;
        end;
        Exit;
    end;
    Lock;
    try
        extn := LowerCase(ExtractFileExt(fname));
        if (extn = '.rpg') then
            fin := TRPGCardStream.Create(fname, fmOpenRead, rpgType)
        else
            fin := TCardFileStream.Create(fname, fmOpenRead);
        try
            FInputCount := FInputCount + fin.RecordCount;
            FHopperEmpty := False;
        finally
            fin.Free;
        end;
        cfr.FileName := fname;
        cfr.RPGType := rpgType;
        cfr.BlankCards := 0;
        FFiles.Add(cfr);
    finally
        Unlock;
    end;
end;

constructor TU92CardDevice.Create(mem: TU92Memory);
begin
    inherited Create(mem);
    FFiles := TCardFileList.Create;
end;

destructor TU92CardDevice.Destroy;
begin
    FreeAndNil(FFiles);
    inherited Destroy;
end;

function TU92CardDevice.OpenNextFile: Boolean;
var
    cfr: TCardFileRec;
    ext: String;
begin
    Result := False;
    if (Assigned(FCurrentFile)) then
    begin
        FFiles.Delete(0);
        FreeAndNil(FCurrentFile);
    end;
    if (FFiles.Count > 0) then
    begin
        cfr := FFiles[0];
        if (cfr.FileName <> '') then
        begin
            ext := LowerCase(ExtractFileExt(cfr.FileName));
            if (ext = '.rpg') then
                FCurrentFile := TRPGCardStream.Create(cfr.FileName, fmOpenRead, cfr.RPGType)
            else
                FCurrentFile := TCardFileStream.Create(cfr.FileName, fmOpenRead)
        end else
            FCurrentFile := TBlankCardStream.Create(cfr.BlankCards);
        Result := True;
    end;
end;

end.
