unit U494ConsDevice;

interface

uses SysUtils, Windows, U494Cpu, U494Memory, SyncObjs;

const
  BUFFER_LENGTH = 80;

type
  T494ConsDevice = class(T494Device)
  private
    FPipe: THandle;
    FConnected: Boolean;
    FInputBfr: array [1..BUFFER_LENGTH] of AnsiChar;
    FIBfrHead: Integer;
    FIBfrTail: Integer;
    FOutputBfr: array [1..BUFFER_LENGTH] of AnsiChar;
    FOBfrHead: Integer;
    FOBfrTail: Integer;
    procedure AddToInput(bfr: array of AnsiChar; len: Integer);
    procedure CopyFromOutput(var bfr: array of AnsiChar; len: Integer; var bytesCopied: Integer);
    procedure IncIndex(var value: Integer);
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
  end;

implementation

uses U494Util, U494Interrupts;

const
  OUTPUT_ENABLE = $01;
  PRINTER_ENABLE = $02;
  PRINTER_ENABLED = OUTPUT_ENABLE or PRINTER_ENABLE;
  PUNCH_OUTPUT_ENABLE = $04;
  INPUT_ENABLE = $08;
  KEYBOARD_ENABLE = $10;
  KEYBOARD_ENABLED = INPUT_ENABLE or KEYBOARD_ENABLE;
  PUNCH_INPUT_ENABLE = $20;
  PUNCH_STOP_START = $40;

{ T494ConsDevice }

procedure T494ConsDevice.AddToInput(bfr: array of AnsiChar; len: Integer);
var
    i, tail: Integer;
begin
    tail := FIBfrTail;
    for i := 0 to len - 1 do
    begin
        FInputBfr[tail] := bfr[i];
        IncIndex(tail);
        FIBfrTail := tail;
        if (tail = FIBfrHead) then
            Break;
    end;
end;

procedure T494ConsDevice.Clear;
begin
    FFunction.Value := 0;
end;

procedure T494ConsDevice.CopyFromOutput(var bfr: array of AnsiChar; len: Integer; var bytesCopied: Integer);
var
    i, head: Integer;
begin
    head := FOBfrHead;
    i := 0;
    bytesCopied := 0;
    while ((FOBfrHead <> FOBfrTail) and (i < len)) do
    begin
        bfr[i] := FOutputBfr[head];
        IncIndex(head);
        FOBfrHead := head;
        Inc(i);
        Inc(bytesCopied);
    end;
end;

constructor T494ConsDevice.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
var
    msg: String;
begin
    inherited;
    FPipe := CreateNamedPipe('\\.\pipe\U494Console',
                             PIPE_ACCESS_DUPLEX or FILE_FLAG_FIRST_PIPE_INSTANCE,
                             PIPE_TYPE_BYTE or PIPE_NOWAIT,
                             1,
                             80,
                             80,
                             100,
                             nil);
    if (FPipe = INVALID_HANDLE_VALUE) then
    begin
        msg := WinError;
        raise Exception.CreateFmt('Could not create console pipe. %s', [msg]);
    end;
    FIBfrHead := 1;
    FIBfrTail := 1;
    FOBfrHead := 1;
    FOBfrTail := 1;
end;

destructor T494ConsDevice.Destroy;
var
    bfr: AnsiString;
    bytesWritten: Cardinal;
begin
    if (FConnected) then
    begin
        bfr := '$$$shutdown$$$';
        WriteFile(FPipe, PAnsiChar(bfr)^, Length(bfr), bytesWritten, nil);
        FlushFileBuffers(FPipe);
    end;
    CloseHandle(FPipe);
    inherited Destroy;
end;

procedure T494ConsDevice.Execute;
var
    errno: Cardinal;
    msg: String;
    bfr: array [1..BUFFER_LENGTH] of AnsiChar;
    bytesRead, bytesWritten: DWORD;
    head, tail, bytesCopied: Integer;
    bcr: T494Bcr;
    word: T494Word;
begin
    try
        while (not Terminated) do
        begin
            if (not FConnected) then
            begin
                if (not ConnectNamedPipe(FPipe, nil)) then
                begin
                    errno := GetLastError;
                    if (errno = ERROR_NO_DATA) then
                    begin
                        DisconnectNamedPipe(FPipe);
                        FConnected := False;
                    end else
                    begin
                        FConnected := (errno = ERROR_PIPE_CONNECTED);
                    end;
                end;
                Continue;
            end;
            FEvent.WaitFor(10);

            if ((FFunction.Value and PRINTER_ENABLED) = PRINTER_ENABLED) then
            begin
                // Flush the output buffer
                CopyFromOutput(bfr, BUFFER_LENGTH, bytesCopied);
                while (bytesCopied > 0) do
                begin
                    if (not WriteFile(FPipe, bfr[1], bytesCopied, bytesWritten, nil)) then
                    begin
                        errno := GetLastError;
                        if (errno <> ERROR_NO_DATA) then
                        begin
                            // Did the client close the pipe?
                            if (errno = ERROR_BROKEN_PIPE) then
                            begin
                                DisconnectNamedPipe(FPipe);
                                FConnected := False;
                                Continue;
                            end;
                            msg := WinError;
                            raise Exception.CreateFmt('Pipe write error! %s', [msg]);
                        end;
                    end;
                    Dec(bytesCopied, bytesWritten);
                end;
                // Copy CPU buffer to output buffer
                bcr := FMemory.FetchBcr(BcrOut0, True);
                tail := FOBfrTail;
                while (OutputActive and
                       ((FFunction.Value and PRINTER_ENABLED) = PRINTER_ENABLED) and
                       (bcr.Count <> 0)) do
                begin
                    word := FMemory.Fetch(bcr.Address, True);
                    FOutputBfr[tail] := AnsiChar(Byte(word.Value and $3f));
                    IncIndex(tail);
                    FOBfrTail := tail;
                    bcr.Address := bcr.Address + 1;
                    bcr.Count := bcr.Count - 1;
                    FMemory.StoreBcr(BcrOut0, bcr, True);
                    if (tail = FOBfrHead) then
                        Break;
                end;
                if (OutputActive and (bcr.Count = 0)) then
                begin
                    if (FOutputMonitor) then
                        QueueInterrupt(intIO, IIsiOutput, 0);
                    TerminateOutput;
                end;
            end;
            // Check for input from the console process
            if (not ReadFile(FPipe, bfr[1], SizeOf(bfr), bytesRead, nil)) then
            begin
                errno := GetLastError;
                if (errno <> ERROR_NO_DATA) then
                begin
                    // Did the client close the pipe?
                    if (errno = ERROR_BROKEN_PIPE) then
                    begin
                        DisconnectNamedPipe(FPipe);
                        FConnected := False;
                        Continue;
                    end;
                    msg := WinError;
                    raise Exception.CreateFmt('Pipe read error! %s', [msg]);
                end;
            end else
            begin
                // Add input just read to the input buffer to
                // wait until input is activated.
                if (InputActive and
                    ((FFunction.Value and KEYBOARD_ENABLED) = KEYBOARD_ENABLED)) then
                    AddToInput(bfr, bytesRead);
            end;
            // Send any new input to the buffer if input is activated and
            // the keyword is enabled
            bcr := FMemory.FetchBcr(BcrIn0, True);
            head := FIBfrHead;
            while (InputActive and
                   ((FFunction.Value and KEYBOARD_ENABLED) = KEYBOARD_ENABLED) and
                   (head <> FIBfrTail) and
                   (bcr.Count <> 0)) do
            begin
                word := FMemory.Fetch(bcr.Address, True);
                word.Value := (word.Value and (not $3f)) or (Byte(FInputBfr[head]) and $3f);
                FMemory.Store(bcr.Address, word, True);
                IncIndex(head);
                FIBfrHead := head;
                bcr.Address := bcr.Address + 1;
                bcr.Count := bcr.Count - 1;
                FMemory.StoreBcr(BcrIn0, bcr, True);
            end;
            if (InputActive and (bcr.Count = 0)) then
            begin
                if (FInputMonitor) then
                    QueueInterrupt(intIO, IIsiInput, 0);
                TerminateInput;
            end;
        end;
    except
      on E: Exception do
      begin
        ShowException(E, ExceptAddr);
      end;
    end;
end;

procedure T494ConsDevice.ExternalFunction(func: T494Word);
begin
    FFunction.Value := func.Value;
end;

procedure T494ConsDevice.IncIndex(var value: Integer);
begin
    Inc(value);
    if (value > BUFFER_LENGTH) then
        value := 1;
end;

end.
