unit U9200TapeController;

interface

uses SysUtils, SyncObjs, U9200Device, U9200Memory, U9200Tape;

type
  TU92TapeCommand = ( tcTest, tcInhibitStatus, tcResetInhibitStatus,
                      tcSense, tcWrite, tcRead, tcReadBackward,
                      tcSetMode, tcInvalid, tcRewind, tcRewindWithLock,
                      tcErase, tcWriteTapeMark, tcBackSpaceBlock,
                      tcBackSpaceFile, tcForwardSpaceBlock, tcForwardSpaceFile );

  TU92TapeController = class(TU92Device)
  protected
    FCommand: TU92TapeCommand;
    FDevice: Byte;
    FDevices: TU92TapeList;
    FBuffer: TU92TapeBlock;
    FSelectedTape: TU92Tape;
    FLastStatusBusy: Boolean;
    FLastStatus: Byte;
    function DoStartIO(dev: Byte; tape: TU92Tape; func: Byte): Byte;
    procedure Fetch(var bfr: TU92TapeBlock);
    function GetIntPending: Boolean; override;
    function GetStatus: TU92DeviceStatus; override;
    procedure ParseCommand(func: Byte;
                           var cmd: TU92TapeCommand;
                           var ctrl, mode, density: Byte);
    procedure SetBusy(busy: Boolean);
    procedure Store(bfr: TU92TapeBlock);
  public
    constructor Create(mem: TU92Memory); override;
    destructor Destroy; override;
    procedure AddDevice(dev: TU92Tape);
    procedure Clear; override;
    procedure Execute; override;
    procedure IntClear; override;
    function StartIO(dev, func: Byte): Byte; override;
    procedure TestIO(dev: Byte; var status, cc: Byte); override;
  end;

implementation

uses U9200Types, U9200Mux, U9200Config;

{ TU92TapeController }

procedure TU92TapeController.AddDevice(dev: TU92Tape);
begin
    FDevices.Add(dev);
end;

procedure TU92TapeController.Clear;
begin
end;

constructor TU92TapeController.Create(mem: TU92Memory);
begin
    inherited;
    FDevices := TU92TapeList.Create;
    Address := gConfig.TapeChannel;
end;

destructor TU92TapeController.Destroy;
begin
    FreeAndNil(FDevices);
    inherited;
end;

function TU92TapeController.DoStartIO(dev: Byte; tape: TU92tape; func: Byte): Byte;
// Do the Start I/O logic, returning a MUX status byte (the initial status), that
// will be translated to a condition code in StartIO.
var
    cmd: TU92TapeCommand;
    ctrl, mode, density: Byte;
begin
    if (udsBusy in FState) then
    begin
        Result := MUX_STATUS_MODIFIER or MUX_BUSY;
        FLastStatusBusy := True;
        Exit;
    end;
    ParseCommand(func, cmd, ctrl, mode, density);
    if (tape.StatusPending) then
    begin
        if ((cmd <> tcTest) and (cmd <> tcInhibitStatus) and (cmd <> tcResetInhibitStatus)) then
        begin
            Result := MUX_BUSY;
            Exit;
        end;
    end;
    if (cmd = tcInvalid) then
    begin
        tape.InvalidFunction := True;
        Result := MUX_UNIT_CHECK;
        Exit;
    end;
    if ((cmd = tcReadBackward) or (cmd = tcBackSpaceBlock) or (cmd = tcBackSpaceFile)) then
    begin
        if (tape.LoadPoint) then
        begin
            Result := MUX_UNIT_CHECK;
            Exit;
        end;
    end;
    case cmd of
      tcTest:
      begin
        Result := tape.Test;
      end;
      tcInhibitStatus:
      begin
        FInhibitInt := True;
        Result := tape.Test;
      end;
      tcResetInhibitStatus:
      begin
        FInhibitInt := False;
        Result := tape.Test;
      end;
      tcSetMode:
      begin
        Result := tape.ModeSet(mode, density);
      end;
      else
      begin
        if (tape.OnLine or (cmd = tcSense)) then
        begin
            SetBusy(True);
            FCommand := cmd;
            FDevice := dev;
            FSelectedTape := tape;
            FEvent.SetEvent;
            case cmd of
              tcRewind,
              tcRewindWithLock,
              tcErase,
              tcWriteTapeMark,
              tcBackSpaceBlock,
              tcBackSpaceFile,
              tcForwardSpaceBlock,
              tcForwardSpaceFile:
                Result := MUX_CHANNEL_END;
              else
                Result := 0;
            end;
        end else
        begin
            Result := MUX_UNIT_CHECK;
        end;
      end;
    end;
end;

procedure TU92TapeController.Execute;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            try
                case FCommand of
                  tcSense:
                  begin
                    FBuffer.Length := 5;
                    FSelectedTape.Sense(FBuffer);
                    Store(FBuffer);
                  end;
                  tcWrite:
                  begin
                    Fetch(FBuffer);
                    FSelectedTape.Write(FBuffer);
                  end;
                  tcRead:
                  begin
                    FBuffer.Length := 8192;
                    FSelectedTape.ReadForward(FBuffer);
                    Store(FBuffer);
                  end;
                  tcReadBackward:
                  begin
                    FBuffer.Length := 8192;
                    FSelectedTape.ReadBackward(FBuffer);
                    Store(FBuffer);
                  end;
                  tcRewind:
                  begin
                    FSelectedTape.Rewind;
                  end;
                  tcRewindWithLock:
                  begin
                    FSelectedTape.RewindWithLock;
                  end;
                  tcErase:
                  begin
                    FSelectedTape.Erase;
                  end;
                  tcWriteTapeMark:
                  begin
                    FSelectedTape.WriteTapemark;
                  end;
                  tcBackSpaceBlock:
                  begin
                    FSelectedTape.BackSpaceBlock;
                  end;
                  tcBackSpaceFile:
                  begin
                    FSelectedTape.BackSpaceFile;
                  end;
                  tcForwardSpaceBlock:
                  begin
                    FSelectedTape.ForwardSpaceBlock;
                  end;
                  tcForwardSpaceFile:
                  begin
                    FSelectedTape.ForwardSpaceFile;
                  end;
                else
                  raise Exception.Create('Internal error. Unknown tape command.');
                end;
            finally
                SetBusy(False);
            end;
        end;
    end;
end;

procedure TU92TapeController.Fetch(var bfr: TU92TapeBlock);
var
    w, m, t: Byte;
    count: Integer;
    addr: Integer;
    i: Integer;
begin
    TU92Mux.FetchBcw(FMemory, FAddress, w, m, t, count, addr);
    bfr.Length := 0;
    if (t = 1) then
        Exit;
    i := Low(bfr.Buffer);
    while ((count > 0) and (i <= High(bfr.Buffer))) do
    begin
        bfr.Buffer[i] := FMemory.FetchByte(addr);
        if (m = 0) then
            Inc(addr)
        else
            Dec(addr);
        Inc(i);
        Dec(count);
        Inc(bfr.Length);
    end;
    if (count = 0) then
        t := 1;
    TU92Mux.StoreBcw(FMemory, FAddress, w, m, t, count, addr);
end;

function TU92TapeController.GetIntPending: Boolean;
var
    dev: TU92Tape;
begin
    if (not FInhibitInt) then
    begin
        for dev in FDevices do
        begin
            if (dev.StatusPending) then
            begin
                Result := True;
                Exit;
            end;
        end;
    end;
    Result := False;
end;

function TU92TapeController.GetStatus: TU92DeviceStatus;
var
    dev: TU92Tape;
begin
    Result.Device := 0;
    Result.Status := 0;
    Result.IntPending := False;
    for dev in FDevices do
    begin
        if (dev.StatusPending) then
        begin
            Result.Status := dev.Status;
            Result.Device := $80 or (FAddress shl 3) or dev.Address;
            Result.IntPending := True;
            Exit;
        end;
    end;
end;

procedure TU92TapeController.IntClear;
begin
    ;
end;

procedure TU92TapeController.ParseCommand(func: Byte; var cmd: TU92TapeCommand; var ctrl, mode, density: Byte);
var
    f: Byte;
begin
    ctrl := 0;
    mode := 0;
    density := 0;
    f := func and $3f;
    if ((f = $0) or (f = $30)) then
        cmd := tcTest
    else if (f = $10) then
        cmd := tcInhibitStatus
    else if (f = $20) then
        cmd := tcResetInhibitStatus
    else if (f = $04) then
        cmd := tcSense
    else if (f = $01) then
        cmd := tcWrite
    else if ((f and $2f) = $02) then
        cmd := tcRead
    else if ((f and $2f) = $0c) then
        cmd := tcReadBackward
    else if ((f and $07) = $07) then
    begin
        ctrl := (f and $38) shr 3;
        case ctrl of
          0:    cmd := tcRewind;
          1:    cmd := tcRewindWithLock;
          2:    cmd := tcErase;
          3:    cmd := tcWriteTapeMark;
          4:    cmd := tcBackSpaceBlock;
          5:    cmd := tcBackSpaceFile;
          6:    cmd := tcForwardSpaceBlock;
          7:    cmd := tcForwardSpaceFile;
        end;
    end else if ((f and $07) = $03) then
    begin
        cmd := tcSetMode;
        mode := f and $38;
        density := func and $c0;
    end else
        cmd := tcInvalid;
end;

procedure TU92TapeController.SetBusy(busy: Boolean);
begin
    Lock;
    try
        if (busy) then
            FState := FState + [udsBusy]
        else
            FState := FState - [udsBusy];
    finally
        Unlock;
    end;
end;

function TU92TapeController.StartIO(dev, func: Byte): Byte;
var
    tape: TU92Tape;
begin
    tape := FDevices.Devices[dev and $07];
    if (not Assigned(tape)) then
    begin
        Result := 3;                        // invalid address
        Exit;
    end;
    FLastStatus := DoStartIO(dev, tape, func);
    if ((FLastStatus and $f3) = 0) then
        Result := 0                         // OK
    else if ((FLastStatus and $b0) <> MUX_BUSY) then
        Result := 1                         // command rejected
    else if ((FLastStatus and $b0) = MUX_BUSY) then
        Result := 2                         // busy
    else
        Result := 3;                        // invalid device
end;

procedure TU92TapeController.Store(bfr: TU92TapeBlock);
var
    w, m, t: Byte;
    count: Integer;
    addr: Integer;
    i: Integer;
begin
    TU92Mux.FetchBcw(FMemory, FAddress, w, m, t, count, addr);
    if (t = 1) then
        Exit;
    i := Low(bfr.Buffer);
    while ((count > 0) and (bfr.Length > 0)) do
    begin
        FMemory.StoreByte(addr, bfr.Buffer[i]);
        if (m = 0) then
            Inc(addr)
        else
            Dec(addr);
        Inc(i);
        Dec(count);
        Dec(bfr.Length);
    end;
    if (count = 0) then
        t := 1;
    TU92Mux.StoreBcw(FMemory, FAddress, w, m, t, count, addr);
end;

procedure TU92TapeController.TestIO(dev: Byte; var status, cc: Byte);
var
    tape: TU92Tape;
begin
    tape := FDevices.Devices[dev and $07];
    if (not Assigned(tape)) then
    begin
        cc := 3;                            // invalid device
        FLastStatus := 0;
    end else if (udsBusy in FState) then
    begin
        cc := 2;
        FLastStatus := FLastStatus or MUX_STATUS_MODIFIER or MUX_BUSY;
        FLastStatusBusy := True;
    end else
    begin
        FLastStatus := FLastStatus or tape.Status;
        if ((FLastStatus and $50) <> 0) then
            cc := 1
        else
            cc := 0;
        if (FLastStatusBusy) then
        begin
            FLastStatus := FLastStatus or MUX_CONTROL_UNIT_END;
            FLastStatusBusy := False;
        end;
    end;
    status := FLastStatus;
    FLastStatus := 0;
end;

end.
