unit U9200IPC;

interface

uses SysUtils, Classes, U9200Types, U9200Memory, U9200Device;

type
  // The emulator for the itegrated peripherals channel. i.e reader, printer, punch
  TU92IPC = class(TObject)
  private
    FDevices: TU92DeviceList;
    FMemory: TU92Memory;
    FLastServiced: Integer;
  public
    constructor Create(mem: TU92Memory);
    destructor Destroy; override;
    procedure AddDevice(dev: TU92Device);
    procedure Clear;
    procedure IntClear(dev: Byte);
    function IntPending(var dev, status: Byte): Boolean;
    function StartIO(dev, func: Byte): Byte;
    procedure TestIO(dev: Byte; var status, cc: Byte);
  end;

implementation

{ TU92IPC }

procedure TU92IPC.AddDevice(dev: TU92Device);
begin
    FDevices.Add(dev);
end;

procedure TU92IPC.Clear;
var
    d: TU92Device;
begin
    for d in FDevices do
    begin
        d.Clear;
    end;
end;

constructor TU92IPC.Create(mem: TU92Memory);
begin
    inherited Create;
    FDevices := TU92DeviceList.Create;
    FMemory := mem;
end;

destructor TU92IPC.Destroy;
begin
    FreeAndNil(FDevices);
    inherited Destroy;
end;

procedure TU92IPC.IntClear(dev: Byte);
var
    d: TU92Device;
begin
    d := FDevices.Devices[dev];
    if (Assigned(d)) then
    begin
        d.IntClear;
    end else
        raise Exception.CreateFmt('Internal Error. Illegal device addres %d', [dev]);
end;

function TU92IPC.IntPending(var dev, status: Byte): Boolean;
var
    i: Integer;
    start: Integer;
begin
    dev := 0;
    status := 0;
    i := FLastServiced + 1;
    if (i >= FDevices.Count) then
        i := 0;
    start := i;
    repeat
        if (FDevices[i].IntPending) then
        begin
            dev := FDevices[i].Address;
            status := FDevices[i].Status.Status;
            FLastServiced := i;
            Result := True;
            Exit;
        end;
        Inc(i);
        if (i >= FDevices.Count) then
            i := 0;
    until (i = start);
    Result := False;
end;

function TU92IPC.StartIO(dev, func: Byte): Byte;
var
    d: TU92Device;
begin
    d := FDevices.Devices[dev];
    if (not Assigned(d)) then
        Result := 3
    else
        Result := d.StartIO(dev, func);
end;

procedure TU92IPC.TestIO(dev: Byte; var status, cc: Byte);
var
    d: TU92Device;
begin
    d := FDevices.Devices[dev];
    if (not Assigned(d)) then
        cc := 3
    else
        d.TestIO(dev, status, cc);
end;

end.
