unit U9200Timer;

interface

uses SysUtils, SyncObjs, U9200Memory, U9200Device;

type
  TU92Timer = class(TU92Device)
  public
    constructor Create(mem: TU92Memory); override;
    procedure Clear; override;
    procedure Execute; override;
    function StartIO(dev, func: Byte): Byte; override;
    procedure TestIO(dev: Byte; var status, cc: Byte); override;
  end;

implementation

{ TU92Timer }

procedure TU92Timer.Clear;
begin
    Lock;
    try
        FStatusQueue.Clear;
        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

constructor TU92Timer.Create(mem: TU92Memory);
begin
    inherited;
    Address := $90;
end;

procedure TU92Timer.Execute;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            Sleep(1000);
            PostStatus($0c);
            Lock;
            try
                FState := FState - [udsBusy];
            finally
                Unlock;
            end;
        end;
    end;
end;

function TU92Timer.StartIO(dev, func: Byte): Byte;
var
    stemp: String;
begin
    Lock;
    try
        // Trace StartIO request
        stemp := Format('XIOF func = %2.2x', [func]);
        Trace(stemp);
        //
        if ((udsBusy in FState)) then
        begin
            Result := 2;
            Trace('**Busy');
            Exit;
        end;
        FCommand := func;
        if (FCommand <> $23) then
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

procedure TU92Timer.TestIO(dev: Byte; var status, cc: Byte);
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
            status := statRec.Status;
            FStatusQueue.Delete(0);
            FState := FState - [udsBusy];
            cc := 1;
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
