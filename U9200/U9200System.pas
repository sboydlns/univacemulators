unit U9200System;

interface

uses Windows, SysUtils, Classes, Forms, U9200Types, U9200Memory, U9200CPU, U9200IPC,
     U9200Reader, U9200Printer, U9200Punch, U9200OpReq, U9200Mux, U9200TapeController,
     U9200Tape, U9200Timer;

type
  TU9200 = class(TObject)
  private
    FState: TU92State;
    FMemory: TU92Memory;
    FCPU: TU92CPU;
    FIPC: TU92IPC;
    FMux: TU92Mux;
    FReader: TU92Reader;
    FPunch: TU92Punch;
    FPrinter: TU92Printer;
    FTapeController: TU92TapeController;
    FTapes: array [0..7] of TU92Tape;
    FOpReq: TU92OpReq;
    FTimer: TU92Timer;
    FOnError: TErrorEvent;
    FOnExecuteInstruction: TExecuteInstructionEvent;
    FOnFetchInstruction: TFetchInstructionEvent;
    FOnHalt: THaltEvent;
    FOnLampChange: TLampUpdateEvent;
    function GetTapes(dev: Byte): TU92Tape;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure DoError(Sender: TObject; E: Exception);
    procedure DoExecuteInstruction(Sender: TObject);
    procedure DoFetchInstruction(Sender: TObject);
    procedure DoHalt(Sender: TObject);
    procedure Load(fname: String); overload;
    procedure Load(dev: Byte); overload;
    procedure PowerOff;
    procedure PowerOn;
    procedure SingleStep(step: Boolean);
    procedure UpdateLamp(lamp, state: Integer);
    property CPU: TU92CPU read FCPU;
    property IPC: TU92IPC read FIPC;
    property Memory: TU92Memory read FMemory;
    property Mux: TU92Mux read FMux;
    property OnError: TErrorEvent read FOnError write FOnError;
    property OnExecuteInstruction: TExecuteInstructionEvent read FOnExecuteInstruction write FOnExecuteInstruction;
    property OnFetchInstruction: TFetchInstructionEvent read FOnFetchInstruction write FOnFetchInstruction;
    property OnHalt: THaltEvent read FOnHalt write FOnHalt;
    property OnLampChange: TLampUpdateEvent read FOnLampChange write FOnLampChange;
    property OpReq: TU92OpReq read FOpReq;
    property Printer: TU92Printer read FPrinter;
    property Punch: TU92Punch read FPunch;
    property Reader: TU92Reader read FReader;
    property State: TU92State read FState;
    property Tapes[dev: Byte]: TU92Tape read GetTapes;
    property Timer: TU92Timer read FTimer;
  end;

implementation

uses Math, U9200Device, U9200Config;

{ TU9200 }

procedure TU9200.Clear;
var
    holdState: TU92State;
begin
    holdState := FState;
    try
        FState := [usInitializing];
        FCPU.Clear;
        FIPC.Clear;
    finally
        FState := holdState;
    end;
end;

constructor TU9200.Create;
var
    i: Byte;
    j: Integer;
begin
    inherited;
    FState := [usPowerOff];
    FMemory := TU92Memory.Create(32 * 1024);                // 32K
    FReader := TU92Reader.Create(FMemory);                  // 400 CPM reader
    FPunch := TU92Punch.Create(FMemory);                    // 200 CPM punch
    FPrinter := TU92Printer.Create(FMemory);                // 600 LPM printer
    FTapeController := TU92TapeController.Create(FMemory);  // 8 Uniservo VI C tapes
    i := Low(FTapes);
    for j := 1 to gConfig.TapeCount do
    begin
        if (i <= High(FTapes)) then
        begin
            FTapes[i] := TU92Tape.Create(i);
            FTapeController.AddDevice(FTapes[i]);
            Inc(i);
        end;
    end;
    FOpReq := TU92OpReq.Create(FMemory);
    FTimer := TU92Timer.Create(FMemory);
    FIPC := TU92IPC.Create(FMemory);
    FIPC.AddDevice(FReader);
    FIPC.AddDevice(FPunch);
    FIPC.AddDevice(FPrinter);
    FIPC.AddDevice(FOpReq);
    FIPC.AddDevice(FTimer);
    FMux := TU92Mux.Create(FMemory);
    FMux.AddController(FTapeController);
    FCPU := TU92CPU.Create(FMemory, FIPC, FMux);
    FCPU.OnError := DoError;
    FCPU.OnExecuteInstruction := DoExecuteInstruction;
    FCPU.OnFetchInstruction := DoFetchInstruction;
    FCPU.OnHalt := DoHalt;
end;

destructor TU9200.Destroy;
begin
    FreeAndNil(FMemory);
    FreeAndNil(FIPC);
    FreeAndNil(FMux);
    FreeAndNil(FReader);
    FreeAndNil(FPunch);
    FreeAndNil(FOpReq);
    FreeAndNil(FTimer);
    FreeAndNil(FCPU);
    inherited;
end;

procedure TU9200.DoError(Sender: TObject; E: Exception);
begin
    if (Assigned(FOnError)) then
        FOnError(Sender, E);
end;

procedure TU9200.DoExecuteInstruction(Sender: TObject);
begin
    if (Assigned(FOnExecuteInstruction)) then
        FOnExecuteInstruction(Sender);
end;

procedure TU9200.DoFetchInstruction(Sender: TObject);
begin
    if (Assigned(FOnFetchInstruction)) then
        FOnFetchInstruction(Sender);
end;

procedure TU9200.DoHalt(Sender: TObject);
begin
    if (Assigned(FOnHalt)) then
        FOnHalt(Sender);
end;

function TU9200.GetTapes(dev: Byte): TU92Tape;
begin
    if (dev <= High(FTapes)) then
        Result := FTapes[dev]
    else
        raise Exception.CreateFmt('Illegal tape device number (%d)', [dev]);
end;

procedure TU9200.Load(dev: Byte);
begin
    if ((dev = 1) or (dev = 2)) then
    begin
        FMemory.BCW[dev] := $00500000;                          // Read 1st card to addr zero
        FIPC.StartIO(dev, READER_READ_TRANSLATE);
        FCPU.ClearInstructionFetched;
    end else
        raise Exception.Create('Illegal boot address');
    Sleep(100);
end;

procedure TU9200.Load(fname: String);
var
    fin: TFileStream;
    size: Integer;
begin
    fin := TFileStream.Create(fname, fmOpenRead);
    try
        size := Min(fin.Size, FMemory.Size);
        fin.Read(FMemory.Memory^, size);
    finally
        fin.Free;
    end;
    FCPU.ClearInstructionFetched;
end;

procedure TU9200.PowerOff;
begin
    if (usPowerOff in FState) then
        Exit;
    UpdateLamp(LAMP_POWER, LAMP_OFF);
    FState := [usPowerOff];
end;

procedure TU9200.PowerOn;
begin
    if (FState * [usPowerOn, usInitializing] <> []) then
        Exit;
    // Do a system reset
    Clear;
    // Turn on the power lamp
    UpdateLamp(LAMP_POWER, LAMP_ON);
    // Let everyone know that we are up
    FState := [usPowerOn];
end;

procedure TU9200.SingleStep(step: Boolean);
begin
    FCPU.SetSingleStep(step);
end;

procedure TU9200.UpdateLamp(lamp, state: Integer);
begin
    if (Assigned(FOnLampChange)) then
        FOnLampChange(Self, lamp, state);
end;

end.
