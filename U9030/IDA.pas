unit IDA;

interface

uses SysUtils, Classes,
     U9030Types, Channels, U8418;

const
  // Command codes
  IDA_FORMAT_WRITE = $01;
  IDA_WRITE = $05;
  IDA_SEARCH_EQ = $09;
  IDA_SEARCH_GE = $0D;
  IDA_READ_ID = $0E;
  IDA_READ = $02;
  IDA_SEEK = $08;
  IDA_SENSE = $04;
  IDA_ECC_SENSE = $03;
  IDA_DIAG = $06;
  IDA_ECC_DIAG = $07;
  // IDA sense bits
  //
  // SB0
  IDA_SENSE_CMD_REJECT = $80;
  IDA_SENSE_INTERVENTION = $40;
  IDA_SENSE_OTPT_PARITY = $20;
  IDA_SENSE_EQUIP_CHECK = $10;
  IDA_SENSE_DATA_CHECK = $08;
  IDA_SENSE_OVERRUN = $04;
  IDA_SENSE_STOP_STATE = $02;
  IDA_SENSE_DVC_CHECK = $01;
  // SB1
  IDA_SENSE_ID_CHECK = $80;
  IDA_SENSE_TRK_OVERRUN = $40;
  IDA_SENSE_CYLINDER_END = $20;
  IDA_SENSE_HIGH_DENSITY = $10;
  IDA_SENSE_NOT_FOUND = $08;
  IDA_SENSE_FILE_PROTECT = $04;
  IDA_SENSE_SYNC_ERROR = $02;
  IDA_SENSE_DATA_FIELD_CHECK = $01;
  // SB2
  IDA_SENSE_SEEK_INCMPLT = $80;
  IDA_SENSE_COMPARE_PARITY = $40;
  IDA_SENSE_CYL_HEAD_MISCOMPARE = $20;
  IDA_SENSE_RECORD_MISCOMPARE = $10;
  IDA_SENSE_FLAG_MISCOMPARE = $08;
  IDA_SENSE_UNSELECTED = $04;
  IDA_SENSE_ECC_CHECK = $02;
  IDA_SENSE_NO_CLOCKS = $01;
  // IDA DEVICE STATUS BITS
  IDA_ATTENTION = $80;
  IDA_BUSY = $10;
  IDA_CHANNEL_END = $08;
  IDA_DEVICE_END = $04;
  IDA_UNIT_CHECK = $02;
  IDA_UNIT_EXCEPTION = $01;
  // IDA CHANNEL STATUS BITS
  IDA_INVALID_ADDRESS = $10;
  IDA_DATA_CHECK = $08;
  IDA_CONTROL_CHECK = $02;

type
  TIDAType = ( it8416, it8418 );

  // IDA BCW wrapper
  TIDABCW = class(TBCW)
  public
    Cylinder: THalfWord;
    Direction: Boolean;
    Head: Byte;
    MultiTrack: Boolean;
    ProgOffset: Byte;
    Recal: Boolean;
    RecordNum: Byte;
    SeekDiff: THalfWord;
    Skip: Boolean;
    TrackCond: Byte;
    Xit: Boolean;
    procedure Assign(src: TIDABCW);
    procedure Fetch; reintroduce;
  end;

  TIDADisk = class(TDevice)
  protected
    FSense: array [0..4] of Byte;
    FDiskType: TIDAType;
    FDisk: T8418Disk;
    FBCW: TIDABCW;
    FCylinder: THalfWord;
    FHead: Byte;
    FRecord: Byte;
    procedure ClearSense;
    procedure DoRead;
    procedure DoReset; override;
    procedure DoSearchEQ;
    procedure DoSeek;
    procedure DoSense;
    function MakeStatus(dstat, cstat: Byte): TStatus;
    procedure NotImplemented;
    procedure ProcessCommand; override;
    function StoreBuffer(bfr: PByte; len: Integer): Boolean;
  public
    constructor Create(num: Byte; typ: TIDAType; fname: String); reintroduce;
    destructor Destroy; override;
    procedure SIO(bcw: TIDABCW);
  end;

  TIDA = class(TChannel)
  private
    FBCW: TIDABCW;
    procedure DeviceBusy(dvc: Byte);
    procedure DoSense(dvc: Byte);
    procedure CommandReject(dvc: Byte);
    procedure IllegalDevice(dvc: Byte);
  public
    constructor Create(chan: Byte); override;
    destructor Destroy; override;
    procedure QueueStatus(stat: TStatus); override;
    function SIO(addr: TWord): Byte; override;
  end;

implementation

uses Memory, Globals;

{ TIDA }

procedure TIDA.CommandReject(dvc: Byte);
var
    stat: TStatus;
begin
    ClearSense(dvc);
    FSense[dvc, 0] := IDA_SENSE_CMD_REJECT;
    stat := TStatus.Create;
    stat.Device := dvc;
    stat.Length := 1;
    stat.ChannelNum := FChannelNum;
    stat.DeviceNum := dvc;
    stat.DeviceStatus := IDA_CHANNEL_END or IDA_UNIT_CHECK;
    QueueStatus(stat);
end;

constructor TIDA.Create(chan: Byte);
begin
    inherited Create(chan);
    FBCW := TIDABCW.Create;
    FMaxDevice := 7;
end;

destructor TIDA.Destroy;
begin
    FreeAndNil(FBCW);
    inherited Destroy;
end;

procedure TIDA.DeviceBusy(dvc: Byte);
var
    stat: TStatus;
begin
    stat := TStatus.Create;
    stat.Device := dvc;
    stat.Length := 1;
    stat.ChannelNum := FChannelNum;
    stat.DeviceNum := dvc;
    stat.DeviceStatus := IDA_CHANNEL_END or IDA_BUSY;
    QueueStatus(stat);
end;

procedure TIDA.DoSense(dvc: Byte);
var
    i: Integer;
    stat: TStatus;
begin
    stat := TStatus.Create;
    stat.Device := dvc;
    stat.Length := 1;
    stat.ChannelNum := FChannelNum;
    stat.DeviceNum := dvc;
    for i := 0 to 4 do
    begin
        try
            Core.StoreByte(0, FBCW.Address, FSense[dvc, i]);
        except
            stat.DeviceStatus := IDA_CHANNEL_END or IDA_DEVICE_END;
            stat.ChannelStatus := IDA_INVALID_ADDRESS;
            QueueStatus(stat);
            Exit;
        end;
    end;
    stat.DeviceStatus := IDA_CHANNEL_END or IDA_CHANNEL_END;
    QueueStatus(stat);
    Exit;
end;

procedure TIDA.IllegalDevice(dvc: Byte);
var
    stat: TStatus;
begin
    stat := TStatus.Create;
    stat.Device := dvc;
    stat.Length := 1;
    stat.ChannelNum := FChannelNum;
    stat.DeviceNum := dvc;
    stat.DeviceStatus := IDA_CHANNEL_END or IDA_UNIT_CHECK;
    QueueStatus(stat);
end;

procedure TIDA.QueueStatus(stat: TStatus);
begin
    if ((stat.DeviceStatus and IDA_CHANNEL_END) <> 0) then
        FBusy := False;
    inherited QueueStatus(stat);
end;

function TIDA.SIO(addr: TWord): Byte;
var
    dvcNum: Byte;
    dvc: TDevice;
begin
    if (FBusy) then                                         // Channel busy
    begin
        Result := 2;
        Exit;
    end;
    FBusy := True;

    dvcNum := addr and $f;
    FBCW.Fetch;
    dvc := FDevices[dvcNum];

    if (not Assigned(dvc)) then
    begin
        Result := inherited SIO(addr);
        Exit;
    end;

    if (dvc.Busy and (FBCW.Command <> IDA_SENSE) and (FBCW.Command <> IDA_DIAG)) then
    begin
        DeviceBusy(dvcNum);
        Result := 1;
        Exit;
    end;

    case FBCW.Command of
      IDA_FORMAT_WRITE,
      IDA_WRITE,
      IDA_SEARCH_EQ,
      IDA_SEARCH_GE,
      IDA_READ_ID,
      IDA_READ,
      IDA_SEEK,
      IDA_SENSE,
      IDA_ECC_SENSE,
      IDA_DIAG,
      IDA_ECC_DIAG:
      begin
        TIDADisk(dvc).SIO(FBCW);
        Result := 0;
      end;
      else
      begin
        CommandReject(dvcNum);
        Result := 1;
      end;
    end;
end;

{ TIDADisk }

procedure TIDADisk.ClearSense;
begin
    FillChar(FSense, SizeOf(FSense), 0);
end;

constructor TIDADisk.Create(num: Byte; typ: TIDAType; fname: String);
begin
    inherited Create(num);
    FBCW := TIDABCW.Create;
    FDiskType := typ;
    if (FDiskType = it8416) then
        FDisk := T8416Disk.Create(fname, fmOpenReadWrite or fmShareDenyWrite)
    else
        FDisk := T8418Disk.Create(fname, fmOpenReadWrite or fmShareDenyWrite);
end;

destructor TIDADisk.Destroy;
begin
    FreeAndNil(FBCW);
    FreeAndNil(FDisk);
    inherited Destroy;
end;

procedure TIDADisk.DoRead;
var
    bfr: array [0..255] of Byte;
begin
    ClearSense;
    FCylinder := FBCW.Cylinder;
    FHead := FBCW.Head;
    FRecord := FBCW.RecordNum;
    FSense[3] := FHead;
    FSense[4] := FRecord;
    while (FBCW.Count > 0) do
    begin
        try
            FDisk.SeekSector(FCylinder, FHead, FRecord);
        except
            FSense[0] := IDA_SENSE_CMD_REJECT;
            FSense[1] := IDA_SENSE_NOT_FOUND;
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
            Exit;
        end;
        try
            FDisk.ReadSector(FCylinder, FHead, FRecord, @bfr);
        except
            FSense[0] := IDA_SENSE_DATA_CHECK;
            FSense[1] := IDA_SENSE_DATA_FIELD_CHECK;
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
            Exit;
        end;
        if (not FBCW.Skip) then
            if (not StoreBuffer(@bfr, 256)) then
                Exit;
        Dec(FBCW.Count);
        Inc(FRecord);
        if (FRecord > 40) then
        begin
            FRecord := 1;
            Inc(Fhead);
            if (Fhead > 6) then
            begin
                FSense[1] := IDA_SENSE_CYLINDER_END;
                FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
                Exit;
            end;
        end;
        FSense[3] := FHead;
        FSense[4] := FRecord;
    end;
    FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_DEVICE_END, 0));
end;

procedure TIDADisk.DoReset;
begin
    inherited DoReset;
    FBCW.Command := 0;
end;

procedure TIDADisk.DoSearchEQ;
var
    i: Integer;
    found: Boolean;
    bfr: array [0..255] of Byte;
    match: array of Byte;
    addr: TMemoryAddress;
begin
    ClearSense;
    // Fetch bytes to match from memory
    SetLength(match, FBCW.Count);
    addr := FBCW.Address;
    for i := 0 to FBCW.Count - 1 do
    begin
        match[i] := Core.FetchByte(FBCW.Key, addr);
        Inc(addr);
    end;
    //
    FCylinder := FBCW.Cylinder;
    FHead := FBCW.Head;
    FRecord := 1;
    FSense[3] := FHead;
    FSense[4] := FRecord;
    while (True) do
    begin
        // Read next record
        try
            FDisk.SeekSector(FCylinder, FHead, FRecord);
        except
            FSense[0] := IDA_SENSE_CMD_REJECT;
            FSense[1] := IDA_SENSE_NOT_FOUND;
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
            Exit;
        end;
        try
            FDisk.ReadSector(FCylinder, FHead, FRecord, @bfr);
        except
            FSense[0] := IDA_SENSE_DATA_CHECK;
            FSense[1] := IDA_SENSE_DATA_FIELD_CHECK;
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
            Exit;
        end;
        // Check for key match
        found := True;
        for i := 0 to FBCW.Count - 1 do
        begin
            if ((match[i] <> $ff) and (match[i] <> bfr[i])) then
            begin
                found := False;
                Break;
            end;
        end;
        // If match found copy bfr to memory and return successful
        // status
        if (found) then
        begin
            if (not FBCW.Skip) then
            begin
                if (not StoreBuffer(PByte(@bfr), 256)) then
                    Exit;
            end;
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_DEVICE_END, 0));
            Exit;
        end;
        // No find, try next record
        Inc(FRecord);
        if (FRecord > 40) then
        begin
            if (not FBCW.MultiTrack) then
            begin
                // Track exhausted without a find
                FSense[1] := IDA_SENSE_NOT_FOUND;
                FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
                Exit;
            end;
            FRecord := 1;
            Inc(FHead);
            if (FHead > 6) then
            begin
                FSense[1] := IDA_SENSE_CYLINDER_END or IDA_SENSE_NOT_FOUND;
                FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_DEVICE_END, 0));
                Exit;
            end;
        end;
        FSense[3] := FHead;
        FSense[4] := FRecord;
    end;
    FSense[1] := IDA_SENSE_NOT_FOUND;
    FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, 0));
end;

procedure TIDADisk.DoSeek;
begin
    FCylinder := FBCW.Cylinder;
    FHead := FBCW.Head;
    FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_DEVICE_END, 0));
end;

procedure TIDADisk.DoSense;
begin
    if (StoreBuffer(@FSense, 4)) then
        FChannel.QueueStatus(MakeStatus(IDA_DEVICE_END or IDA_CHANNEL_END, 0));
end;

function TIDADisk.MakeStatus(dstat, cstat: Byte): TStatus;
begin
    Result := TStatus.Create;
    Result.Device := FDeviceNum;
    Result.Length := 1;
    Result.ChannelNum := FChannel.ChannelNum;
    Result.DeviceNum := FDeviceNum;
    Result.DeviceStatus := dstat;
    Result.ChannelStatus := cstat;
end;

procedure TIDADisk.NotImplemented;
begin
    raise Exception.Create('IDA command not implemented');
end;

procedure TIDADisk.ProcessCommand;
begin
    if (FBCW.Recal) then
        FCylinder := 0;
    case FBCW.Command of
      IDA_FORMAT_WRITE:     NotImplemented;
      IDA_WRITE:            NotImplemented;
      IDA_SEARCH_EQ:        DoSearchEQ;
      IDA_SEARCH_GE:        NotImplemented;
      IDA_READ_ID:          NotImplemented;
      IDA_READ:             DoRead;
      IDA_SEEK:             DoSeek;
      IDA_SENSE:            DoSense;
      IDA_ECC_SENSE:        NotImplemented;
      IDA_DIAG:             NotImplemented;
      IDA_ECC_DIAG:         NotImplemented;
    end;
    FBCW.Command := 0;
    FBusy := False;
end;

procedure TIDADisk.SIO(bcw: TIDABCW);
begin
    FBCW.Assign(bcw);
    if (FBCW.Command = 0) then
        raise Exception.Create('IDA command = 0');
    if (FBusy and (FBCW.Command <> IDA_SENSE) and (FBCW.Command <> IDA_DIAG)) then
        // Since this is checked at the channel level,
        // this should never happer.
        Exit;
    FBusy := True;
    FCmdRecvd.SetEvent;
end;

function TIDADisk.StoreBuffer(bfr: PByte; len: Integer): Boolean;
var
    i: Integer;
    addr: TMemoryAddress;
begin
    Result := True;
    addr := FBCW.Address;
    for i := 0 to len - 1 do
    begin
        try
            Core.StoreByte(0, addr, (bfr + i)^);
        except
            Result := False;
            Inc(FBCW.Address, i);
            FChannel.QueueStatus(MakeStatus(IDA_CHANNEL_END or IDA_UNIT_CHECK, IDA_INVALID_ADDRESS));
            Exit;
        end;
        Inc(addr);
    end;
    Inc(FBCW.Address, len);
end;

{ TIDABCW }

procedure TIDABCW.Assign(src: TIDABCW);
begin
    Address := src.Address;
    Command := src.Command;
    Count := src.Count;
    Cylinder := src.Cylinder;
    Direction := src.Direction;
    Head := src.Head;
    Key := src.Key;
    MultiTrack := src.MultiTrack;
    ProgOffset := src.ProgOffset;
    Recal := src.Recal;
    RecordNum := src.RecordNum;
    SeekDiff := src.SeekDiff;
    Skip := src.Skip;
    TrackCond := src.TrackCond;
    Xit := src.Xit;
end;

procedure TIDABCW.Fetch;
var
    w: TWord;
begin
    inherited Fetch(IDA_BCW0);
    w := Core.FetchWord(0, IDA_BCW1);
    Skip := (w and $8000) <> 0;
    MultiTrack := (w and $4000) <> 0;
    Direction := (w and $2000) <> 0;
    Xit := (w and $1000) <> 0;
    w := Core.FetchWord(0, IDA_BCW2);
    SeekDiff := w shr 16;
    Head := (w shr 8) and $f;
    TrackCond := (w shr 6) and $3;
    w := Core.FetchWord(0, IDA_BCW3);
    Recal := (w and $80000000) <> 0;
    Cylinder := (w shr 16) and $fff;
    RecordNum := (w shr 8) and $ff;
    ProgOffset := w and $7;
end;

end.