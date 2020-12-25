unit U9200Mux;

interface

uses SysUtils, U9200Memory, U9200Device;

type
  TU92Mux = class(TObject)
  private
    FControllers: TU92DeviceList;
    FMemory: TU92Memory;
    FLastServiced: Integer;
    function Channel(dev: Byte): Byte;
  public
    constructor Create(mem: TU92Memory);
    destructor Destroy; override;
    class procedure FetchBcw(mem: TU92Memory; chan: Byte; var w, m, t: Byte; var count, addr: Integer);
    class procedure StoreBcw(mem: TU92Memory; chan: Byte; w, m, t: Byte; count, addr: Integer);
    procedure AddController(dev: TU92Device);
    procedure Clear;
    procedure IntClear(dev: Byte);
    function IntPending(var dev, status: Byte): Boolean;
    function StartIO(dev, func: Byte): Byte;
    procedure TestIO(dev: Byte; var status, cc: Byte);
  end;

implementation

{ TU92Mux }

procedure TU92Mux.AddController(dev: TU92Device);
begin
    FControllers.Add(dev);
end;

function TU92Mux.Channel(dev: Byte): Byte;
begin
    if ((dev and $80) = 0) then
        Result := dev and $0f
    else
        Result := (dev and $78) shr 3;
end;

procedure TU92Mux.Clear;
var
    d: TU92Device;
begin
    for d in FControllers do
    begin
        d.Clear;
    end;
end;

constructor TU92Mux.Create(mem: TU92Memory);
begin
    inherited Create;
    FMemory := mem;
    FControllers := TU92DeviceList.Create;
end;

destructor TU92Mux.Destroy;
begin
    FreeAndNil(FControllers);
    inherited Destroy;
end;

class procedure TU92Mux.FetchBcw(mem: TU92Memory; chan: Byte; var w, m, t: Byte; var count, addr: Integer);
var
    bcw: Cardinal;
begin
    bcw := mem.BCW[chan];
    w := (bcw and $80000000) shr 31;
    m := (bcw and $40000000) shr 30;
    t := (bcw and $20000000) shr 29;
    count := (bcw and $1fff0000) shr 16;
    if (count = 0) then
        count := 8192;
    addr := bcw and $7fff;
end;

procedure TU92Mux.IntClear(dev: Byte);
var
    d: TU92Device;
begin
    d := FControllers.Devices[Channel(dev)];
    if (Assigned(d)) then
    begin
        d.IntClear;
    end else
        raise Exception.CreateFmt('Internal Error. Illegal device addres %d', [dev]);
end;

function TU92Mux.IntPending(var dev, status: Byte): Boolean;
var
    i: Integer;
    stat: TU92DeviceStatus;
    start: Integer;
begin
    dev := 0;
    status := 0;
    i := FLastServiced + 1;
    if (i >= FControllers.Count) then
        i := 0;
    start := i;
    repeat
        if (FControllers[i].IntPending) then
        begin
            stat := FControllers[i].Status;
            dev := stat.Device;
            status := stat.Status;
            FLastServiced := i;
            Result := True;
            Exit;
        end;
        Inc(i);
        if (i >= FControllers.Count) then
            i := 0;
    until (i = start);
    Result := False;
end;

function TU92Mux.StartIO(dev, func: Byte): Byte;
var
    d: TU92Device;
begin
    d := FControllers.Devices[Channel(dev)];
    if (not Assigned(d)) then
        Result := 3
    else
        Result := d.StartIO(dev, func);
end;

class procedure TU92Mux.StoreBcw(mem: TU92Memory; chan, w, m, t: Byte; count, addr: Integer);
var
    bcw: Cardinal;
begin
    bcw := mem.BCW[chan];
    bcw := (bcw and $dfffffff) or ((t and 1) shl 29);
    bcw := (bcw and $e000ffff) or ((count and $1fff) shl 16);
    bcw := (bcw and $ffff0000) or (addr and $7fff);
    mem.BCW[chan] := bcw;
end;

procedure TU92Mux.TestIO(dev: Byte; var status, cc: Byte);
var
    d: TU92Device;
begin
    d := FControllers.Devices[Channel(dev)];
    if (not Assigned(d)) then
        cc := 3
    else
        d.TestIO(dev, status, cc);
end;

end.
