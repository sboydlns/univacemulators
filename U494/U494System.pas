unit U494System;

interface

uses Windows, SysUtils, Classes, U494Memory, U494Cpu, U494Interrupts, U494Reader, U494Printer;

type
  T494RTClock = class(TThread)
  private
    FCpu: T494Cpu;
    FMemory: T494Memory;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory);
    procedure Execute; override;
  end;

  T494DayClock = class(TThread)
  private
    FCpu: T494Cpu;
    FMemory: T494Memory;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory);
    procedure Execute; override;
  end;

  T494System = class
  private
    FCpu: T494Cpu;
    FMemory: T494Memory;
    FRTClock: T494RTClock;
    FDayClock: T494DayClock;
    function GetReader: T494Reader;
    function GetPunch: T494Punch;
    function GetPrinter: T494Printer;
  public
    constructor Create;
    destructor Destroy; override;
    property Cpu: T494Cpu read FCpu;
    property Memory: T494Memory read FMemory;
    property Reader: T494Reader read GetReader;
    property Printer: T494Printer read GetPrinter;
    property Punch: T494Punch read GetPunch;
  end;

implementation

uses U494ConsDevice, FH880Device, U494Util, U494Config;

{ T494System }

constructor T494System.Create;
var
    dev: T494Device;
    fname: String;
begin
    inherited;
    FMemory := T494Memory.Create;
    FCpu := T494Cpu.Create(Fmemory);
    FCpu.Channels[0] := T494ConsDevice.Create(FCpu, FMemory, 0);
    FCpu.Channels[1] := T494ReaderPunch.Create(FCpu, FMemory, 1);
    FCpu.Channels[2] := T494Printer.Create(FCpu, FMemory, 2);
    FCpu.Channels[5] := TFH880Device.Create(FCpu, FMemory, 5);
    fname := PublicDataDir + '\Univac 494 Emulator\Data\SYSVOL.drum';
    if (FileExists(fname)) then
        TFH880Device(FCpu.Channels[5]).DrumFile := fname
    else
        TFH880Device(FCpu.Channels[5]).DrumFile := '..\..\Drums\SYSVOL.drum';
    FRTClock := T494RTClock.Create(FCpu, FMemory);
    FRTClock.Start;
    if ((gConfig.Mode = m494) or (gConfig.Mode = m490)) then
    begin
        FDayCLock := T494DayClock.Create(FCpu, FMemory);
        FDayClock.Start;
    end;
    // Start all I/O devices
    for dev in FCpu.Channels do
    begin
        if (Assigned(dev)) then
            dev.Start;
    end;
end;

destructor T494System.Destroy;
var
    i: Integer;
    dev: T494Device;
begin
    if (Assigned(FRTClock)) then
    begin
        FRTClock.Terminate;
        FRTClock.WaitFor;
        FreeAndNil(FRTClock);
    end;
    if (Assigned(FDayClock)) then
    begin
        FDayClock.Terminate;
        FDayClock.WaitFor;
        FreeAndNil(FDayClock);
    end;
    for i := 0 to FCpu.Channels.Count - 1 do
    begin
        dev := FCpu.Channels[i];
        if (Assigned(dev)) then
        begin
            dev.Terminate;
            dev.WaitFor;
            dev.Free;
        end;
        FCpu.Channels[i] := nil;
    end;
    FCpu.Channels.Clear;
    FreeAndNil(FCpu);
    FreeAndNil(FMemory);
    inherited;
end;

function T494System.GetPrinter: T494Printer;
begin
    Result := T494Printer(FCpu.Channels[2]);
end;

function T494System.GetPunch: T494Punch;
begin
    Result := T494ReaderPunch(FCpu.Channels[1]).Punch;
end;

function T494System.GetReader: T494Reader;
begin
    Result := T494ReaderPunch(FCpu.Channels[1]).Reader;
end;

{ T494DayClock }

constructor T494RTClock.Create(cpu: T494Cpu; mem: T494Memory);
begin
    inherited Create(True);
    FCpu := cpu;
    FMemory := mem;
end;

procedure T494RTClock.Execute;
// The real time clock is supposed to fire every 200 microseconds. The
// only way to accomplish this under Windows is to spin on QueryPerformanceCounter.
// This uses excessive amounts of CPU time. Since 200 ms resolution is likely not
// going to be required for the emulator, we use Sleep(1) which gives us about
// 2 millisecond resolution. We capture the interval to the nearest multiple
// of 200 microseconds (1 millisecond for 490 mode) and update the real time clock counter accordingly.
var
    timerFreq, intervalStart, intervalEnd, ms200: Int64;
    word: T494Word;
    origWord: UInt32;
    int: T494Interrupt;
begin
    QueryPerformanceFrequency(timerFreq);
    QueryPerformanceCounter(intervalStart);
    while (not Terminated) do
    begin
        Sleep(1);
        if (gConfig.Mode = m1230) then
            word := FMemory.Fetch(RTClock1230, True)
        else
            word := FMemory.Fetch(RTClock, True);
        origWord := word.Value;
        QueryPerformanceCounter(intervalEnd);
        ms200 := Round(((intervalEnd - intervalStart) / timerFreq) * 5000);
        if ((gConfig.Mode = m494) and (ms200 > 0)) then
        begin
            QueryPerformanceCounter(intervalStart);
            word.Value := (word.Value + ms200) and $3ffff;
            FMemory.Store(RTClock, word, True);
            // Queue an interrupt if the counter wraps.
            if (word.Value < origWord) then
            begin
                int.IType := intClock;
                int.Vector := IRTClock;
                FCpu.Interrupts.Enqueue(int);
            end;
        end else if ((gConfig.Mode = m490) and (ms200 > 4)) then
        begin
            QueryPerformanceCounter(intervalStart);
            word.Value := (word.Value + (ms200 div 5)) and BITS15;
            FMemory.Store(RTClock, word, True);
            if (word.Value < origWord) then
            begin
                int.IType := intClock;
                int.Vector := IRTClock;
                FCpu.Interrupts.Enqueue(int);
            end;
        end else if ((gConfig.Mode = m1230) and (ms200 > 4)) then
        begin
            QueryPerformanceCounter(intervalStart);
            word.Value := (word.Value + (ms200 div 5)) and BITS15;
            FMemory.Store(RTClock1230, word, True);
        end;
    end;
end;

{ T494DayClock }

constructor T494DayClock.Create(cpu: T494Cpu; mem: T494Memory);
begin
    inherited Create(True);
    FCpu := cpu;
    FMemory := mem;
end;

procedure T494DayClock.Execute;
var
    count, hundredths: Integer;
    int: T494Interrupt;
    time: TSystemTime;
    word: T494Word;
begin
    count := 10;
    while (not Terminated) do
    begin
        Sleep(600);
        GetLocalTime(time);
        hundredths := Round(((time.wSecond + (time.wMilliseconds / 1000)) / 60) * 100);
        word.Value := ((time.wHour div 10) shl 28) or
                      ((time.wHour mod 10) shl 24) or
                      ((time.wMinute div 10) shl 20) or
                      ((time.wMinute mod 10) shl 16) or
                      ((hundredths div 10) shl 12) or
                      ((hundredths mod 10) shl 8);
        FMemory.Store(DayClock, word, True);
        // Queue an interrupt every 6 seconds
        Dec(count);
        if (count = 0) then
        begin
            int.IType := intClock;
            int.Vector := IDayClock;
            FCpu.Interrupts.Enqueue(int);
            count := 10;
        end;
    end;
end;

end.
