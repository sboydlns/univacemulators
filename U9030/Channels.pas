unit Channels;

interface

uses SysUtils, Classes, System.Contnrs, SyncObjs, Generics.Collections,
     U9030Types;

const
  MAX_CHANNEL = 7;
  STAT_ATTENTION = $80;
  // sense bits
  //
  // SB0
  SENSE_CMD_REJECT = $80;
  SENSE_INTERVENTION = $40;
  SENSE_OTPT_PARITY = $20;
  SENSE_EQUIP_CHECK = $10;
  SENSE_DATA_CHECK = $08;
  SENSE_OVERRUN = $04;
  SENSE_STOP_STATE = $02;
  SENSE_DVC_CHECK = $01;
  // DEVICE STATUS BITS
  ATTENTION = $80;
  BUSY = $10;
  CHANNEL_END = $08;
  DEVICE_END = $04;
  UNIT_CHECK = $02;
  UNIT_EXCEPTION = $01;
  // CHANNEL STATUS BITS
  INVALID_ADDRESS = $10;
  DATA_CHECK = $08;
  CONTROL_CHECK = $02;

type
  TChannel = class;

  TBCW = class
  public
    Address: TMemoryAddress;
    Command: Byte;
    Count: THalfWord;
    Key: Byte;
    procedure Fetch(addr: TMemoryAddress); virtual;
  end;

  TDevice = class(TThread)
  protected
    FChannel: TChannel;
    FDeviceNum: Byte;
    FCmdRecvd: TEvent;
    FResetDone: TEvent;
    FReset: Boolean;
    FBusy: Boolean;
    procedure DoReset; virtual;
    procedure ProcessCommand; virtual; abstract;
  public
    constructor Create(num: Byte); virtual;
    destructor Destroy; override;
    procedure Execute; override;
    procedure Reset;
    property Busy: Boolean read FBusy;
  end;

  TStatus = class
  private
    function GetChannel: Byte;
    procedure SetChannel(const Value: Byte);
    function GetDevice: Byte;
    procedure SetDevice(const Value: Byte);
  public
    Device: Byte;
    Length: Byte;                           // # of status words in Status
    Status: array [0..15] of Byte;
    property ChannelNum: Byte read GetChannel write SetChannel;
    property DeviceNum: Byte read GetDevice write SetDevice;
    property DeviceStatus: Byte read Status[2] write Status[2];
    property ChannelStatus: Byte read Status[3] write Status[3];
  end;

  TStatusQueue = class(TQueue<TStatus>)
  private
    FLock: TCriticalSection;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Dequeue: TStatus; reintroduce;
    procedure Enqueue(s: TStatus); reintroduce;
  end;

  TChannel = class
  protected
    FBusy: Boolean;
    FMaxDevice: Byte;
    FChannelNum: Byte;
    FStatusQueue: TStatusQueue;
    FDevices: array [0..31] of TDevice;
    FSense: array [0..31, 0..4] of Byte;
    function BcwAddress(chan, dvc: Byte): TMemoryAddress;
    procedure ClearSense(dvc: Byte);
  public
    constructor Create(chan: Byte); virtual;
    destructor Destroy; override;
    procedure AddDevice(dvc: TDevice); virtual;
    procedure ForceAttn;
    function GetStatus: TStatus;
    function IntPending: Boolean;
    procedure QueueStatus(stat: TStatus); virtual;
    procedure Reset;
    function SIO(addr: TWord): Byte; virtual;
    property ChannelNum: Byte read FChannelNum;
  end;

  TChannelList = class(TObjectList)
  private
    function GetChannel(chan: Integer): TChannel;
    procedure SetChannel(chan: Integer; const Value: TChannel);
  public
    constructor Create; reintroduce;
    property Channel[chan: Integer]: TChannel read GetChannel write SetChannel;
  end;

implementation

uses Dialogs, Globals, Memory;
{ TChannelList }

constructor TChannelList.Create;
var
    i: Integer;
begin
    inherited Create(True);
    // Create an empty list
    for i := 0 to MAX_CHANNEL do
        Add(nil);
end;

function TChannelList.GetChannel(chan: Integer): TChannel;
begin
    Result := TChannel(Items[chan]);
end;

procedure TChannelList.SetChannel(chan: Integer; const Value: TChannel);
begin
    Items[chan].Free;
    Items[chan] := Value;
end;

{ TStatusQueue }

constructor TStatusQueue.Create;
begin
    inherited Create;
    FLock := TCriticalSection.Create;
end;

function TStatusQueue.Dequeue: TStatus;
begin
    FLock.Acquire;
    try
        Result := inherited Dequeue;
    finally
        FLock.Release;
    end;
end;

destructor TStatusQueue.Destroy;
begin
    FreeAndNil(FLock);
    inherited Destroy;
end;

procedure TStatusQueue.Enqueue(s: TStatus);
begin
    FLock.Acquire;
    try
        inherited Enqueue(s);
    finally
        FLock.Release;
    end;
    Processor.IOST.IntRequest;
end;

{ TChannel }

procedure TChannel.AddDevice(dvc: TDevice);
begin
    if (dvc.FDeviceNum > FMaxDevice) then
        raise Exception.Create('Device number must be 0 - 15');

    if (Assigned(FDevices[dvc.FDeviceNum])) then
    begin
        FDevices[dvc.FDeviceNum].Terminate;
        FDevices[dvc.FDeviceNum].WaitFor;
        FreeAndNil(FDevices[dvc.FDeviceNum]);
    end;
    dvc.FChannel := Self;
    FDevices[dvc.FDeviceNum] := dvc;
end;

function TChannel.BcwAddress(chan, dvc: Byte): TMemoryAddress;
begin
    if (chan = 0) then
        Result := CONS_BCW0 + (dvc shl 4)
    else if (chan = 1) then
        Result := MUX_BCW0 + (dvc and $f0)
    else if (chan = 3) then
        Result := IDA_BCW0
    else if ((chan = 4) or (chan = 6)) then
        // For selector channels we need to read the CAW to get the pointer
        // to the CCW which is the same as a BCW for our purposes here.
        Result := Core.FetchWord(0, CAW) and $3ffff
    else
        raise Exception.Create('Invalid channel #');
end;

procedure TChannel.ClearSense(dvc: Byte);
begin
    FillChar(FSense[dvc], SizeOf(FSense[dvc]), 0);
end;

constructor TChannel.Create(chan: Byte);
begin
    FMaxDevice := 15;
    FChannelNum := chan;
    FStatusQueue := TStatusQueue.Create;
end;

destructor TChannel.Destroy;
var
    i: Integer;
begin
    for i := Low(FDevices) to High(FDevices) do
    begin
        if (Assigned(FDevices[i])) then
        begin
            FDevices[i].Terminate;
            FDevices[i].WaitFor;
            FreeAndNil(FDevices[i]);
        end;
    end;
    FreeAndNil(FStatusQueue);
    inherited;
end;

procedure TChannel.ForceAttn;
// Queue an attention interrupt for each device on the channel
var
    dvc: TDevice;
    stat: TStatus;
begin
    for dvc in FDevices do
    begin
        if (Assigned(dvc)) then
        begin
            stat := TStatus.Create;
            stat.ChannelNum := FChannelNum;
            stat.DeviceNum := dvc.FDeviceNum;
            stat.DeviceStatus := STAT_ATTENTION;
            stat.Length := 1;
            QueueStatus(stat);
        end;
    end;
end;

function TChannel.GetStatus: TStatus;
begin
    Result := FStatusQueue.Dequeue;
end;

function TChannel.IntPending: Boolean;
begin
    Result := FStatusQueue.Count <> 0;
end;

procedure TChannel.QueueStatus(stat: TStatus);
begin
    FStatusQueue.Enqueue(stat);
end;

procedure TChannel.Reset;
var
    i: Integer;
begin
    for i := Low(FDevices) to High(FDevices) do
    begin
        if (Assigned(FDevices[i])) then
            FDevices[i].Reset;
    end;
    FStatusQueue.Clear;
end;

function TChannel.SIO(addr: TWord): Byte;
// This default SIO method is only invoked for channels or devices that don't
// have an implementation. It checks the command code in the BCW. If it
// is a sense command we post reply with CHANNEL_END / DEVICE_END with
// sense bytes of CMD_REJECT.
//
// Otherwise we set the condition code to 3 and post CHANNEL_END / UNIT_CHECK.
var
    chan, dvc: Byte;
    stat: TStatus;
    bcw: TBCW;
    bfr: TMemoryAddress;
    count: THalfword;
begin
    chan := (addr shr 8) and $7;
    dvc := addr and $ff;

    bcw := TBCW.Create;
    try
        stat := TStatus.Create;
        stat.Device := dvc;
        stat.Length := 1;
        stat.ChannelNum := FChannelNum;
        stat.DeviceNum := dvc;

        bcw.Fetch(BcwAddress(chan, dvc));

        if ((bcw.Command and $0f) = 4) then
        begin
            // If sense command, return CMD_REJECT
            bfr := bcw.Address;
            Core.StoreByte(bcw.Key, bfr, SENSE_CMD_REJECT);
            Inc(bfr, 1);
            count := bcw.Count - 1;
            while (count > 0) do
            begin
                Core.StoreByte(bcw.Key, bfr, 0);
                Inc(bfr, 1);
                Dec(count);
            end;
            stat.DeviceStatus := CHANNEL_END or DEVICE_END;
            QueueStatus(stat);
            Result := 0;
        end else
        begin
            // Otherweise return UNIT_CHECK
            stat.DeviceStatus := CHANNEL_END or UNIT_CHECK;
            QueueStatus(stat);
            Result := 3;
        end;
    finally
        bcw.Free;
    end;
end;

{ TDevice }

constructor TDevice.Create(num: Byte);
begin
    inherited Create(False);
    FDeviceNum := num;
    FCmdRecvd := TEvent.Create(nil, False, False, '');
    FResetDone := TEvent.Create(nil, False, False, '');
end;

destructor TDevice.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FCmdRecvd);
    FreeAndNil(FResetDone);
    inherited Destroy;
end;

procedure TDevice.DoReset;
begin
    FReset := False;
    FCmdRecvd.ResetEvent;
    FResetDone.SetEvent;
end;

procedure TDevice.Execute;
var
    stat: TWaitResult;
begin
    while (not Terminated) do
    begin
        stat := FCmdRecvd.WaitFor(100);
        case stat of
          wrSignaled,
          wrTimeout:
          begin
            if (FReset) then
                DoReset
            else
                ProcessCommand;
          end;
          else
          begin
            // Event was abandonded or there was some other error.
            // This should never happen. Quit!
            ShowMessageFmt('Event error chan = %d dvc = %d' ,[FChannel.FChannelNum, FDeviceNum]);
            Terminate;
          end;
        end;
    end;
end;

procedure TDevice.Reset;
begin
    FReset := True;
    FCmdRecvd.SetEvent;
    FResetDone.WaitFor(100);
end;

{ TStatus }

function TStatus.GetChannel: Byte;
begin
    Result := Status[0] and $7;
end;

function TStatus.GetDevice: Byte;
begin
    Result := Status[1];
end;

procedure TStatus.SetChannel(const Value: Byte);
begin
    Status[0] := Status[0] or (Value and $7);
end;

procedure TStatus.SetDevice(const Value: Byte);
begin
    Status[1] := Value;
end;

{ TBCW }

procedure TBCW.Fetch(addr: TMemoryAddress);
var
    w: TWord;
begin
    w := Core.FetchWord(0, addr);
    Command := w shr 24;
    Key := (w shr 20) and $07;
    Address := w and $7ffff;
    w := Core.FetchWord(0, addr + 4);
    Count := w and $1ff;
end;

end.
