unit U494ConsDevice;

interface

uses SysUtils, Classes, Windows, U494Cpu, U494Memory, SyncObjs;

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
    FPunchFileName: String;
    FPunchFile: TFileStream;
    procedure AddToInput(bfr: array of AnsiChar; len: Integer);
    procedure CopyFromOutput(var bfr: array of AnsiChar; len: Integer; var bytesCopied: Integer);
    procedure IncIndex(var value: Integer);
    procedure SetPunchFile(const Value: String);
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    property PunchFile: String read FPunchFileName write SetPunchFile;
  end;

implementation

uses U494Util, U494Interrupts, U494Config;

const
  OUTPUT_ENABLE = $01;
  PRINTER_ENABLE = $02;
  PRINTER_ENABLED = OUTPUT_ENABLE or PRINTER_ENABLE;
  PUNCH_OUTPUT_ENABLE = $04;
  PUNCH_ENABLED = OUTPUT_ENABLE or PUNCH_OUTPUT_ENABLE;
  INPUT_ENABLE = $08;
  KEYBOARD_ENABLE = $10;
  KEYBOARD_ENABLED = INPUT_ENABLE or KEYBOARD_ENABLE;
  PUNCH_INPUT_ENABLE = $20;
  READER_START = $40;
  READER_ENABLED = INPUT_ENABLE or PUNCH_INPUT_ENABLE or READER_START;

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
    i: Integer;
begin
    Lock;
    try
        i := 0;
        bytesCopied := 0;
        while ((FOBfrHead <> FOBfrTail) and (i < len)) do
        begin
            if ((Byte(FOutputBfr[FObfrHead]) and $80) = $80) then
            begin
                // Process external function codes
                if (i = 0) then
                begin
                    FFunction.Value := Byte(FOutputBfr[FObfrHead]) and $7f;
                    IncIndex(FObfrHead);
                end;
                Break;
            end;
            bfr[i] := FOutputBfr[FObfrHead];
            IncIndex(FObfrHead);
            Inc(i);
            Inc(bytesCopied);
        end;
    finally
        Unlock;
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
    head, tail, bytesCopied, count: Integer;
    bcr: T494Bcr;
    word: T494Word;

    procedure DoOutput;
    begin
        // Flush the output buffer
        CopyFromOutput(bfr, BUFFER_LENGTH, bytesCopied);
        if ((FFunction.Value and PRINTER_ENABLE) = PRINTER_ENABLE) then
        begin
            // send output to console process
            count := bytesCopied;
            while (count > 0) do
            begin
                if (not WriteFile(FPipe, bfr[1], count, bytesWritten, nil)) then
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
                Dec(count, bytesWritten);
            end;
        end;
        if ((FFunction.Value and PUNCH_ENABLED) = PUNCH_ENABLED) then
        begin
            // send output to currently open paper tape file
            count := bytesCopied;
            while (Assigned(FPunchFile) and (count > 0)) do
            begin
                bytesWritten := FPunchFile.Write(bfr[1], count);
                Dec(count, bytesWritten);
            end;
        end;
        // Copy CPU buffer to output buffer
        Lock;
        try
            bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
            tail := FOBfrTail;
            while (OutputActive and
                   ((FFunction.Value and OUTPUT_ENABLE) = OUTPUT_ENABLE) and
                   (bcr.Count <> 0)) do
            begin
                word := FMemory.Fetch(bcr.Address, True);
                FOutputBfr[tail] := AnsiChar(Byte(word.Value and $3f));
                IncIndex(tail);
                FOBfrTail := tail;
                bcr.Address := bcr.Address + 1;
                bcr.Count := bcr.Count - 1;
                FMemory.StoreBcr(BcrOut(FChannel), bcr, True);
                if (tail = FOBfrHead) then
                    Break;
            end;
        finally
            Unlock;
        end;
        if (OutputActive and (bcr.Count = 0)) then
        begin
            if (FOutputMonitor) then
                QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
            TerminateOutput;
        end;
    end;

    procedure DoInput;
    begin
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
                    Exit;
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
        //
        if (InputActive) then
        begin
            // Send any new keyboard input to the buffer if the keyboard is enabled
            bcr := FMemory.FetchBcr(BcrIn(FChannel), True);
            head := FIBfrHead;
            while (((FFunction.Value and KEYBOARD_ENABLED) = KEYBOARD_ENABLED) and
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
                FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
            end;
            // Send paper tape reader input to the buffer if the reader is enabled
            while (Assigned(FPunchFile) and
                   ((FFunction.Value and READER_ENABLED) = READER_ENABLED) and
                   (bcr.Count <> 0)) do
            begin
                bytesRead := FPunchFile.Read(bfr[1], 1);
                if (bytesRead = 0) then                         // EOF?
                    Break;
                word.Value := (word.Value and (not $3f)) or (Byte(bfr[1]) and $3f);
                FMemory.Store(bcr.Address, word, True);
                bcr.Address := bcr.Address + 1;
                bcr.Count := bcr.Count - 1;
                FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
            end;

            if (InputActive and (bcr.Count = 0)) then
            begin
                if (FInputMonitor) then
                    QueueInterrupt(intIO, IIsiInput(FChannel), 0);
                TerminateInput;
            end;
        end;
    end;

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

            if ((FFunction.Value and OUTPUT_ENABLE) = OUTPUT_ENABLE) then
                DoOutput;
            DoInput;
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
    if (FFunction.Value = 0) then
    begin
        // If device is idle, set new function code now
        FFunction := func;
    end else
    begin
        // If device is potentially busy, put new function code on the queue
        // to be processed later.
        Lock;
        try
            FOutputBfr[FObfrTail] := AnsiChar(Byte(func.Value) or $80);
            IncIndex(FObfrTail);
        finally
            Unlock;
        end;
        FEvent.SetEvent;
    end;
end;

procedure T494ConsDevice.IncIndex(var value: Integer);
begin
    Inc(value);
    if (value > BUFFER_LENGTH) then
        value := 1;
end;

procedure T494ConsDevice.SetPunchFile(const Value: String);
begin
    FreeAndNil(FPunchFile);
    FPunchFileName := Value;
    if (FPunchFileName = '') then
        Exit;

    if (not FileExists(FPunchFileName)) then
    begin
        FPunchFile := TFileStream.Create(FPunchFileName, fmCreate);
        FreeAndNil(FPunchFile);
    end;
    FPunchFile := TFileStream.Create(FPunchFileName, fmOpenReadWrite or fmShareDenyNone);
end;

end.
