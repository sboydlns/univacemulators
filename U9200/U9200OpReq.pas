unit U9200OpReq;

interface

uses SysUtils, U9200Memory, U9200Device;

type
  TU92OpReq = class(TU92Device)
  private
    FEnabled: Boolean;
  public
    constructor Create(mem: TU92Memory); override;
    procedure Clear; override;
    procedure Execute; override;
    procedure OpReqPressed;
    function StartIO(dev, func: Byte): Byte; override;
    procedure TestIO(dev: Byte; var status, cc: Byte); override;
    property Enabled: Boolean read FEnabled;
  end;

implementation

{ TU92OpReq }

procedure TU92OpReq.Clear;
begin
    Lock;
    try
        FEnabled := False;
        FStatusQueue.Clear;
        FState := FState + [udsReady] - [udsBusy, udsError];
    finally
        Unlock;
    end;
end;

constructor TU92OpReq.Create(mem: TU92Memory);
begin
    inherited;
    Address := $80;
end;

procedure TU92OpReq.Execute;
begin
    while (not Terminated) do
        Sleep(100);
end;

procedure TU92OpReq.OpReqPressed;
begin
    Lock;
    try
        FEnabled := False;
        PostStatus(0);
    finally
        Unlock;
    end;
end;

function TU92OpReq.StartIO(dev, func: Byte): Byte;
begin
    Lock;
    try
        Result := 0;
        case func of
          $13:  FEnabled := False;
          $23:  FEnabled := True;
          else  Result := 2;
        end;
    finally
        Unlock;
    end;

end;

procedure TU92OpReq.TestIO(dev: Byte; var status, cc: Byte);
var
    statRec: TU92DeviceStatus;
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
                cc := 1;
            end;
        end;
    finally
        Unlock;
    end;
end;

end.
