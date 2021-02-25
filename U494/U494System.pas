unit U494System;

interface

uses Windows, SysUtils, Classes, U494Memory, U494Cpu, U494Interrupts, U494Reader, U494Printer,
     U494ConsDevice;

type
  TRTClock = class(TThread)
  private
    FCpu: T494Cpu;
    FMemory: T494Memory;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory);
    procedure Execute; override;
  end;

  T1230RTClock = class(TRTClock)
  public
    procedure Execute; override;
  end;

  T490RTClock = class(TRTClock)
  public
    procedure Execute; override;
  end;

  T494RTClock = class(TRTClock)
  public
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
    FRTClock: TRTClock;
    FDayClock: T494DayClock;
    function GetReader: T494Reader;
    function GetPunch: T494Punch;
    function GetPrinter: T494Printer;
    function GetConsole: T494ConsDevice;
  public
    constructor Create;
    destructor Destroy; override;
    property Console: T494ConsDevice read GetConsole;
    property Cpu: T494Cpu read FCpu;
    property Memory: T494Memory read FMemory;
    property Reader: T494Reader read GetReader;
    property Printer: T494Printer read GetPrinter;
    property Punch: T494Punch read GetPunch;
  end;

implementation

uses FH880Device, U494Util, U494Config;

{ T494System }

constructor T494System.Create;
var
    dev: T494Device;
    fname: String;
    i: Integer;
    drum: T494Drum;
begin
    inherited;
    FMemory := T494Memory.Create;
    FCpu := T494Cpu.Create(Fmemory);
    case gConfig.ConsoleType of
      ct1232:   FCpu.Channels[gConfig.ConsoleChan] := T494ConsDevice.Create(FCpu, FMemory, gConfig.ConsoleChan);
    end;
    if (gConfig.RdrPunChan <> -1) then
        FCpu.Channels[gConfig.RdrPunChan] := T494ReaderPunch.Create(FCpu, FMemory, gConfig.RdrPunChan);
    if (gConfig.PrinterChan <> -1) then
        FCpu.Channels[gConfig.PrinterChan] := T494Printer.Create(FCpu, FMemory, gConfig.PrinterChan);
    for i := 0 to gConfig.DrumCount - 1 do
    begin
        drum := gConfig.Drums[i];
        case drum.DrumType of
          dtFH880:      FCpu.Channels[drum.Chan] := TFH880Device.Create(FCpu, FMemory, drum.Chan);
        end;
        if (Assigned(FCpu.Channels[drum.Chan])) then
        begin
            fname := PublicDataDir + '\Univac 494 Emulator\Data\SYSVOL.drum';
            if (FileExists(fname)) then
                TFH880Device(FCpu.Channels[drum.Chan]).DrumFile := fname
            else
                TFH880Device(FCpu.Channels[drum.Chan]).DrumFile := '..\..\Drums\SYSVOL.drum';
        end;
    end;
    case gConfig.Mode of
      m494:     FRTClock := T494RTClock.Create(FCpu, FMemory);
      m490:     FRTClock := T490RTClock.Create(FCpu, FMemory);
      m1230:    FRTClock := T1230RTClock.Create(FCpu, FMemory);
    end;
    if (Assigned(FRTClock)) then
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

function T494System.GetConsole: T494ConsDevice;
begin
    Result := T494ConsDevice(FCpu.Channels[gConfig.ConsoleChan]);
end;

function T494System.GetPrinter: T494Printer;
begin
    Result := nil;
    if (gConfig.PrinterChan <> -1) then
        Result := T494Printer(FCpu.Channels[gConfig.PrinterChan]);
end;

function T494System.GetPunch: T494Punch;
begin
    Result := nil;
    if (gConfig.RdrPunChan <> -1) then
        Result := T494ReaderPunch(FCpu.Channels[gConfig.RdrPunChan]).Punch;
end;

function T494System.GetReader: T494Reader;
begin
    Result := nil;
    if (gConfig.RdrPunChan <> -1) then
        Result := T494ReaderPunch(FCpu.Channels[gConfig.RdrPunChan]).Reader;
end;

{ T494DayClock }

procedure T494RTClock.Execute;
// The real time clock is supposed to fire every 200 microseconds. The
// only way to accomplish this under Windows is to spin on QueryPerformanceCounter.
// This uses excessive amounts of CPU time. Since 200 ms resolution is likely not
// going to be required for the emulator, we use Sleep(1) which gives us about
// 2 millisecond resolution. We capture the interval to the nearest multiple
// of 200 microseconds and update the real time clock counter accordingly.
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
        if (ms200 > 0) then
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

{ TGenericRTClock }

constructor TRTClock.Create(cpu: T494Cpu; mem: T494Memory);
begin
    inherited Create(True);
    FCpu := cpu;
    FMemory := mem;
end;

procedure TRTClock.Execute;
begin
    while (not Terminated) do
        Sleep(100);
end;

{ T1230RTClock }

procedure T1230RTClock.Execute;
// The real time clock is supposed to fire every millisecond. The
// only way to accomplish this under Windows is to spin on QueryPerformanceCounter.
// This uses excessive amounts of CPU time. Since millisecond resolution is likely not
// going to be required for the emulator, we use Sleep(1) which gives us about
// 2 millisecond resolution. We capture the interval to the nearest multiple
// of 1 millisecond and update the real time clock counter accordingly.
var
    timerFreq, intervalStart, intervalEnd, ms: Int64;
    word: T494Word;
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
        QueryPerformanceCounter(intervalEnd);
        ms := Round(((intervalEnd - intervalStart) / timerFreq) * 1000);
        if (ms > 0) then
        begin
            QueryPerformanceCounter(intervalStart);
            word.Value := (word.Value + ms) and BITS30;
            FMemory.Store(RTClock1230, word, True);
        end;
    end;
end;

{ T490RTClock }

procedure T490RTClock.Execute;
// The real time clock is supposed to fire every millisecond. The
// only way to accomplish this under Windows is to spin on QueryPerformanceCounter.
// This uses excessive amounts of CPU time. Since millisecond resolution is likely not
// going to be required for the emulator, we use Sleep(1) which gives us about
// 2 millisecond resolution. We capture the interval to the nearest multiple
// of 1 millisecond and update the real time clock counter accordingly.
var
    timerFreq, intervalStart, intervalEnd, ms: Int64;
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
        ms := Round(((intervalEnd - intervalStart) / timerFreq) * 1000);
        if (ms > 0) then
        begin
            QueryPerformanceCounter(intervalStart);
            word.Value := (word.Value + ms) and $3ffff;
            FMemory.Store(RTClock, word, True);
            // Queue an interrupt if the counter wraps.
            if (word.Value < origWord) then
            begin
                int.IType := intClock;
                int.Vector := IRTClock;
                FCpu.Interrupts.Enqueue(int);
            end;
        end;
    end;
end;

end.
