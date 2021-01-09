unit FH880Device;

interface

uses SysUtils, Windows, U494Cpu, U494Memory, SyncObjs, FH880File;

type
  TFH880Device = class(T494Device)
  private
    FDrum: TFH880File;
    FDrumFile: String;
    FFuncCode: Byte;
    FFuncAddress: Cardinal;
    FIdentifier: T494Word;
    procedure SetDrumFile(const Value: String);
  public
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure ActivateOutput(withMon: Boolean); override;
    procedure BootNoInterrupt(var func: Byte; var addr: UInt32);
    procedure BootWithInterrupt(var func: Byte; var addr: UInt32);
    procedure Clear; override;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    procedure ReadBlock(var func: Byte; var addr: UInt32);
    procedure ReadContinuous(var func: Byte; var addr: UInt32);
    procedure Search(var func: Byte; var addr: UInt32);
    procedure SearchBlock(var func: Byte; var addr: UInt32);
    procedure SearchBlockRead(var func: Byte; var addr: UInt32);
    procedure SearchRead(var func: Byte; var addr: UInt32);
    procedure SetStatusAddr(value: UInt32);
    procedure SetStatusCode(value: UInt32);
    function StatusCode: UInt32;
    procedure TermNoInterrupt(var func: Byte; var addr: UInt32);
    procedure TermWithInterrupt(var func: Byte; var addr: UInt32);
    procedure Write(var func: Byte; var addr: UInt32);
    property DrumFile: String read FDrumFile write SetDrumFile;
  end;

implementation

uses U494Interrupts;

const
  CWRITE = 2;
  CREAD_CONTINUOUS = 34;
  CREAD_BLOCK = 42;
  CSEARCH = 37;
  CSEARCH_READ = 38;
  CSEARCH_BLOCK = 45;
  CSEARCH_BLOCK_READ = 46;
  CBOOT_NO_INTERRUPT = 32;
  CBOOT_WITH_INTERRUPT = 40;
  CTERM_NO_INTERRUPT = 19;
  CTERM_WITH_INTERRUPT = 27;

  CEND_OF_BLOCK = UInt32(4);
  CSEARCH_FIND = Uint32(5);
  CEND_OF_FILE = UInt32(28);
  CNORMAL_COMPLETION = UInt32(32);
  CILLEGAL_FUNCTION = UInt32(40);
  CILLEGAL_ADDRESS = UInt32(44);

{ TFH880Device }

procedure TFH880Device.ActivateInput(withMon: Boolean);
begin
    Lock;
    try
        FInputMonitor := withMon;
        FInputActive := True;
        if (FFuncCode <> 0) then
            FEvent.SetEvent;
    finally
        Unlock;
    end;
end;

procedure TFH880Device.ActivateOutput(withMon: Boolean);
begin
    Lock;
    try
        FOutputMonitor := withMon;
        FOutputActive := True;
        if (FFuncCode <> 0) then
            FEvent.SetEvent;
    finally
        Unlock;
    end;
end;

procedure TFH880Device.BootNoInterrupt(var func: Byte; var addr: UInt32);
begin
    addr := 0;
    ReadContinuous(func, addr);
end;

procedure TFH880Device.BootWithInterrupt(var func: Byte; var addr: UInt32);
begin
    addr := 0;
    ReadBlock(func, addr);
end;

procedure TFH880Device.Clear;
begin
    FFuncCode := 0;
    FStatus := 0;
end;

destructor TFH880Device.Destroy;
begin
    FreeAndNil(FDrum);
    inherited Destroy;
end;

procedure TFH880Device.Execute;
var
    fcode: Byte;
    faddr: UInt32;
begin
    while (not Terminated) do
    begin
        FEvent.WaitFor;
        Lock;
        try
            fcode := FFuncCode;
            faddr := FFuncAddress;
        finally
            Unlock;
        end;
        case fcode of
          0:                     ;                  // do nothing while waiting for function code
          CWRITE:                Write(fcode, faddr);
          CREAD_CONTINUOUS:      ReadContinuous(fcode, faddr);
          CREAD_BLOCK:           ReadBlock(fcode, faddr);
          CSEARCH:               Search(fcode, faddr);
          CSEARCH_READ:          SearchRead(fcode, faddr);
          CSEARCH_BLOCK:         SearchBlock(fcode, faddr);
          CSEARCH_BLOCK_READ:    SearchBlockRead(fcode, faddr);
          CBOOT_NO_INTERRUPT:    BootNoInterrupt(fcode, faddr);
          CBOOT_WITH_INTERRUPT:  BootWithInterrupt(fcode, faddr);
          CTERM_NO_INTERRUPT:    TermNoInterrupt(fcode, faddr);
          CTERM_WITH_INTERRUPT:  TermWithInterrupt(fcode, faddr);
          else                   SetStatusCode(CILLEGAL_FUNCTION);
        end;
        Lock;
        try
            FFuncCode := fcode;
            FFuncAddress := faddr;
        finally
            Unlock;
        end;
        if (FStatus <> 0) then
        begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), FStatus);
            FStatus := 0;
        end;
    end;
end;

procedure TFH880Device.ExternalFunction(func: T494Word);
begin
    Lock;
    try
        // If the last ExternalFunction requested a search, then
        // this ExternalFunction must be supplying the identifier
        // word. Start search when identifier received.
        if ((FFuncCode = CSEARCH) or (FFuncCode = CSEARCH_READ) or
            (FFuncCode = CSEARCH_BLOCK) or (FFuncCode = CSEARCH_BLOCK_READ)) then
        begin
            FIdentifier := func;
            FInputActive := True;
            FEvent.SetEvent;
        end else
        begin
            FFunction := func;
            FFuncCode := FFunction.Value shr 24;
            FFuncAddress := FFunction.Value and $7fffff;
            // Start reads, writes and bootstraps here. Searches are started after the
            // identifier is received
            if ((FFuncCode <> CSEARCH) and (FFuncCode <> CSEARCH_READ) and
                (FFuncCode <> CSEARCH_BLOCK) and (FFuncCode <> CSEARCH_BLOCK_READ)) then
                FEvent.SetEvent;
        end;
    finally
        Unlock;
    end;
end;

procedure TFH880Device.ReadBlock(var func: Byte; var addr: UInt32);
var
    bcr: T494Bcr;
    word: T494Word;
begin
    FStatus := 0;
    bcr := FMemory.FetchBcr(BcrIn(FChannel), True);
    while (FInputActive and (bcr.Count > 0) and (StatusCode <> CEND_OF_FILE)) do
    begin
        word.Value := FDrum.ReadWord(addr);
        FMemory.Store(bcr.Address, word, True);
        if (addr >= (FH880_WORDS_PER_DRUM - 1)) then
            SetStatusCode(CEND_OF_FILE);
        Inc(addr);
        bcr.Address := bcr.Address + 1;
        bcr.Count := bcr.Count - 1;
        FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
        if (word.Value = BITS30) then
        begin
            // End of block detected. Read overflow word,
            // use it to set the status address and copy
            // it to the input buffer.
            if (addr < FH880_WORDS_PER_DRUM) then
            begin
                SetStatusCode(CEND_OF_BLOCK);
                word.Value := FDrum.ReadWord(addr);
                SetStatusAddr(word.Value);
                if (bcr.Count > 0) then
                begin
                    word.Value := FStatus;
                    FMemory.Store(bcr.Address, word, True);
                    bcr.Address := bcr.Address + 1;
                    bcr.Count := bcr.Count - 1;
                    FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
                end;
            end else
            begin
                SetStatusCode(CEND_OF_FILE);
            end;
            Break;
        end;
    end;
    if (FInputMonitor and (bcr.Count = 0)) then
        QueueInterrupt(intIO, IIsiInput(FChannel), 0);
    TerminateInput;
    func := 0;
end;

procedure TFH880Device.ReadContinuous(var func: Byte; var addr: UInt32);
var
    bcr: T494Bcr;
    word: T494Word;
begin
    FStatus := 0;
    bcr := FMemory.FetchBcr(BcrIn(FChannel), True);
    while (FInputActive and (bcr.Count > 0) and (StatusCode <> CEND_OF_FILE)) do
    begin
        word.Value := FDrum.ReadWord(addr);
        FMemory.Store(bcr.Address, word, True);
        if (addr >= (FH880_WORDS_PER_DRUM - 1)) then
            SetStatusCode(CEND_OF_FILE);
        Inc(addr);
        bcr.Address := bcr.Address + 1;
        bcr.Count := bcr.Count - 1;
        FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
    end;
    if (FInputMonitor and (bcr.Count = 0)) then
    begin
        QueueInterrupt(intIO, IIsiInput(FChannel), 0);
        TerminateInput;
    end;
end;

procedure TFH880Device.Search(var func: Byte; var addr: UInt32);
var
    word: T494Word;
begin
    FStatus := 0;
    while (FInputActive and (StatusCode = 0)) do
    begin
        word.Value := FDrum.ReadWord(addr);
        if (word.Value = FIdentifier) then
        begin
            SetStatusCode(CSEARCH_FIND);
            SetStatusAddr(addr);
        end else if (addr >= (FH880_WORDS_PER_DRUM - 1)) then
            SetStatusCode(CEND_OF_FILE);
        Inc(addr);
    end;
    if (func = CSEARCH) then
        TerminateInput;
    func := 0;
end;

procedure TFH880Device.SearchBlock(var func: Byte; var addr: UInt32);
var
    word: T494Word;
begin
    FStatus := 0;
    while (FInputActive and (StatusCode = 0)) do
    begin
        word.Value := FDrum.ReadWord(addr);
        if (word.Value = FIdentifier) then
        begin
            SetStatusCode(CSEARCH_FIND);
            SetStatusAddr(addr);
            Inc(addr);
        end else
        begin
            Inc(addr);
            if (word.Value = BITS30) then
            begin
                // End of block detected. Read overflow word,
                // use it to set the status address.
                if (addr < FH880_WORDS_PER_DRUM) then
                begin
                    SetStatusCode(CEND_OF_BLOCK);
                    word.Value := FDrum.ReadWord(addr);
                    SetStatusAddr(word.Value);
                end else
                begin
                    SetStatusCode(CEND_OF_FILE);
                end;
            end else if (addr >= FH880_WORDS_PER_DRUM) then
                SetStatusCode(CEND_OF_FILE);
        end;
    end;
    if (func = CSEARCH_BLOCK) then
        TerminateInput;
    func := 0;
end;

procedure TFH880Device.SearchBlockRead(var func: Byte; var addr: UInt32);
begin
    SearchBlock(func, addr);
    Dec(addr);
    if (StatusCode = CSEARCH_FIND) then
        ReadBlock(func, addr);
    TerminateInput;
    func := 0;
end;

procedure TFH880Device.SearchRead(var func: Byte; var addr: UInt32);
begin
    Search(func, addr);
    Dec(addr);
    if (StatusCode = CSEARCH_FIND) then
        ReadContinuous(func, addr);
end;

procedure TFH880Device.SetDrumFile(const Value: String);
begin
    if (Assigned(FDrum)) then
        raise Exception.Create('Drum file cannot be changed');
    FDrumFile := Value;
    FDrum := TFH880File.Create(FDrumFile, fmOpenReadWrite or fmShareExclusive);
end;

procedure TFH880Device.SetStatusAddr(value: UInt32);
begin
    FStatus := (FStatus and (not $7fffff)) or (value and $7fffff);
end;

procedure TFH880Device.SetStatusCode(value: UInt32);
begin
    FStatus := (FStatus and (not $3f000000)) or (value shl 24);
end;

function TFH880Device.StatusCode: UInt32;
begin
    Result := FStatus shr 24;
end;

procedure TFH880Device.TermNoInterrupt(var func: Byte; var addr: UInt32);
begin
    FStatus := 0;
    if (FInputActive) then
        TerminateInput
    else if (FOutputActive) then
        TerminateOutput;
    func := 0;
end;

procedure TFH880Device.TermWithInterrupt(var func: Byte; var addr: UInt32);
begin
    FStatus := 0;
    if (FInputActive) then
        TerminateInput
    else if (FOutputActive) then
        TerminateOutput;
    func := 0;
    SetStatusCode(CNORMAL_COMPLETION);
end;

procedure TFH880Device.Write(var func: Byte; var addr: UInt32);
var
    bcr: T494Bcr;
    word: T494Word;
begin
    FStatus := 0;
    bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
    while ((bcr.Count > 0) and (StatusCode <> CEND_OF_FILE)) do
    begin
        word := FMemory.Fetch(bcr.Address, True);
        FDrum.WriteWord(addr, word.Value);
        if (addr >= (FH880_WORDS_PER_DRUM - 1)) then
            SetStatusCode(CEND_OF_FILE);
        Inc(addr);
        bcr.Address := bcr.Address + 1;
        bcr.Count := bcr.Count - 1;
        FMemory.StoreBcr(BcrOut(FChannel), bcr, True);
    end;
    if (FOutputMonitor and (bcr.Count = 0)) then
    begin
        QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
        TerminateOutput;
    end;
end;

end.
