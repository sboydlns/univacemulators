unit U494Reader;

interface

uses SysUtils, Classes, SyncObjs, U494Memory, U494Cpu, CardFile;

type
  TReadType = ( rtTranslate, rtImageCol, rtImageRow);

  T494Reader = class(T494CardDevice)
  private
    FStacker0Count: Integer;
    FStacker1Count: Integer;
    FStacker2Count: Integer;
    FReadStationLoaded: Boolean;
    FFuncCode: Byte;
    FWithExtInterrupt: Boolean;
    FReadType: TReadType;
    FReadStation: TCardRec;
    FOnFeed: TNotifyEvent;
    procedure DoFeed;
    procedure DoOnFeed;
    procedure Read;
    procedure ReadAndFeed;
    procedure ReadImageCol;
    procedure ReadImageRow;
    procedure ReadTranslate;
    procedure Store(bfr: TCardRec);
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure ActivateOutput(withMon: Boolean); override;
    procedure Clear; override;
    procedure EmptyHopper;
    procedure Feed;
    function HasCardToFeed: Boolean;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property Stacker0Count: Integer read FStacker0Count;
    property Stacker1Count: Integer read FStacker1Count;
    property Stacker2Count: Integer read FStacker2Count;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

  T494Punch = class(T494CardDevice)
  private
    FOutputFile0: TCardFileStream;
    FOutputFile1: TCardFileStream;
    FCardFileDir: String;
    FStacker0Count: Integer;
    FStacker1Count: Integer;
    FPunchStationLoaded: Boolean;
    FReadStationLoaded: Boolean;
    FFuncCode: Byte;
    FWithExtInterrupt: Boolean;
    FPunchStation: TCardRec;
    FReadStation: TCardRec;
    FHopper: Integer;
    FPunchType: TReadType;
    FOnFeed: TNotifyEvent;
    procedure DoFeed;
    procedure DoOnFeed;
    procedure Fetch(var bfr: TCardRec);
    procedure Punch(stkr: Integer);
    procedure PunchImageCol;
    procedure PunchImageRow;
    procedure PunchTranslate;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure Clear; override;
    procedure EmptyHopper(num: Integer);
    procedure EmptyHoppers;
    procedure Feed;
    function HasCardToFeed: Boolean;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    procedure SaveHopper(num: Integer; fname: String);
    property OnFeed: TNotifyEvent read FOnFeed write FOnFeed;
    property Stacker0Count: Integer read FStacker0Count;
    property Stacker1Count: Integer read FStacker1Count;
    property PunchStationLoaded: Boolean read FPunchStationLoaded;
    property ReadStationLoaded: Boolean read FReadStationLoaded;
  end;

  T494ReaderPunch = class(T494Device)
  private
    FFirstPass: Boolean;
    FReader: T494Reader;
    FPunch: T494Punch;
  public
    constructor Create(cpu: T494Cpu; mem: T494Memory; chan: Byte); override;
    destructor Destroy; override;
    procedure ActivateInput(withMon: Boolean); override;
    procedure ActivateOutput(withMon: Boolean); override;
    procedure Clear; override;
    function InputActive: Boolean; override;
    procedure Execute; override;
    procedure ExternalFunction(func: T494Word); override;
    function OutputActive: Boolean; override;
    property Reader: T494Reader read FReader;
    property Punch: T494Punch read FPunch;
  end;

implementation

uses Math, EmulatorTypes, U494Interrupts;

const
  CPUNCH_STACKER0 = $02;
  CPUNCH_STACKER1 = $03;
  CPUNCH_TRANSLATE = $04;
  CPUNCH_IMAGE_COL = $05;
  CPUNCH_IMAGE_ROW = $06;
  CTERMINATE = $13;
  CREAD_NO_FEED = $21;
  CREAD = $22;
  CFEED = $23;
  CREAD_TRANSLATE = $32;
  CREAD_IMAGE_COL = $33;
  CREAD_IMAGE_ROW = $34;
  CINTERRUPT = $08;

  CSEQUENCE_ERROR = $10 shl 24;
  CCOUNT_ERROR = $18 shl 24;
  CNORMAL_COMPLETION = $20 shl 24;
  CILLEGAL_FUNCTION = $28 shl 24;
  CREAD_PUNCH_CHECK = $2C shl 24;
  CINAPPROPRAITE_FUNCTION = $30 shl 24;
  CILLEGAL_CHARACTER = $38 shl 24;
  CINTERLOCK = $3C shl 24;

{ TU92Reader }

procedure T494Reader.ActivateInput(withMon: Boolean);
begin
    FInputMonitor := withMon;
    FInputActive := True;
end;

procedure T494Reader.ActivateOutput(withMon: Boolean);
// Meaningless for card reader, ignore
begin
    ;
end;

procedure T494Reader.Clear;
begin
end;

constructor T494Reader.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited;
    FReadType := rtTranslate;
    FHopperEmpty := True;
end;

destructor T494Reader.Destroy;
begin
    FreeAndNil(FCurrentFile);
    inherited;
end;

procedure T494Reader.DoFeed;
begin
    Lock;
    try
        if (FReadStationLoaded) then
            Inc(FStacker0Count);
        if ((not Assigned(FCurrentFile)) or (FCurrentFile.Eof)) then
        begin
            if (not OpenNextFile) then
            begin
                FHopperEmpty := True;
                FReadStationLoaded := False;
                Exit;
            end;
        end;
        FHopperEmpty := False;
        FReadStationLoaded := True;                 // mark read station as loaded
        FCurrentFile.ReadRaw(FReadStation);
        Dec(FInputCount);                           // adjust hopper counts
        if (FInputCount < 0) then
            FInputCount := 0;
    finally
        Unlock;
        if (Assigned(FOnFeed)) then
            Queue(DoOnFeed);
    end;
//    Sleep(100);                                     // Sleep 0.1 secs to simulate 600 CPM
    Sleep(20);
end;

procedure T494Reader.DoOnFeed;
begin
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

procedure T494Reader.EmptyHopper;
begin
    Lock;
    try
        FFiles.Clear;
        FreeAndNil(FCurrentFile);
        FInputCount := 0;
        FStacker0Count := 0;
        FStacker1Count := 0;
        FStacker2Count := 0;
    finally
        Unlock;
    end;
end;

procedure T494Reader.Execute;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            case FFuncCode of
              0:
                Continue;
              CREAD_NO_FEED:
                Read;
              CREAD:
                ReadAndFeed;
              CFEED:
                Feed;
              else
              begin
                FFuncCode := 0;
                QueueInterrupt(intIO, IIsiExternal(FChannel), CILLEGAL_FUNCTION);
              end;
            end;
        end;
    end;
end;

procedure T494Reader.ExternalFunction(func: T494Word);
begin
    Lock;
    try
        if (FFuncCode <> 0) then
        begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), CINAPPROPRAITE_FUNCTION);
            Exit;
        end;
        FFuncCode := (func.Value shr 24) and (not CINTERRUPT);
        FWithExtInterrupt := ((func.Value shr 24) and CINTERRUPT) <> 0;
        case FFuncCode of
          0:
          begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), CILLEGAL_FUNCTION);
            Exit;
          end;
          CREAD_TRANSLATE:
          begin
            FReadType := rtTranslate;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          CREAD_IMAGE_COL:
          begin
            FReadType := rtImageCol;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          CREAD_IMAGE_ROW:
          begin
            FReadType := rtImageRow;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          else
          begin
            FEvent.SetEvent;
          end;
        end;
    finally
        Unlock;
    end;
end;

procedure T494Reader.Feed;
begin
    DoFeed;
end;

function T494Reader.HasCardToFeed: Boolean;
begin
    Result := ((not FHopperEmpty) or FReadStationLoaded);
end;

procedure T494Reader.Read;
begin
    if (not FReadStationLoaded) then                // Try to read a card. Failure will be caught later
        Feed;
    case FReadType of
      rtTranslate:
        ReadTranslate;
      rtImageCol:
        ReadImageCol;
      rtImageRow:
        ReadImageRow;
    end;
end;

procedure T494Reader.ReadAndFeed;
begin
    Read;
    Feed;
end;

procedure T494Reader.ReadImageCol;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadImage(bfr, FReadStation);
        Store(bfr);
        FInputActive := False;
        FFuncCode := 0;
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Reader.ReadImageRow;
var
    c, r: Integer;
    col, mask, bit: WORD;
    word: T494Word;
    bcr: T494Bcr;
begin
    if (FReadStationLoaded) then
    begin
        bcr := FMemory.FetchBcr(BcrIn(FChannel), True);
        for r := 15 downto 4 do
        begin
            word.Value := 0;
            mask := 1 shl r;
            for c := 1 to FReadStation.Count div 2 do
            begin
                col := FReadStation.ColumnAsWord[c];
                bit := (col and mask) shr r;
                word.Value := (word.Value shl 1) or bit;
                if ((c mod 30) = 0) then
                begin
                    FMemory.Store(bcr.Address, word, True);
                    word := 0;
                    bcr.Address := bcr.Address + 1;
                    bcr.Count := bcr.Count - 1;
                    FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
                    if (bcr.Count = 0) then
                        Break;
                end;
            end;
            if (bcr.Count = 0) then
                Break;
            word.Value := word.Value shl 10;
            FMemory.Store(bcr.Address, word, True);
            bcr.Address := bcr.Address + 1;
            bcr.Count := bcr.Count - 1;
            FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
        end;
        FInputActive := False;
        FFuncCode := 0;
        if (FInputMonitor and (bcr.Count = 0)) then
        begin
            QueueInterrupt(intIO, IIsiInput(FChannel), 0);
            TerminateInput;
        end;
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Reader.ReadTranslate;
var
    bfr: TCardRec;
begin
    if (FReadStationLoaded) then
    begin
        TCardFileStream.ReadFieldata(bfr, FReadStation);
        Store(bfr);
        FInputActive := False;
        FFuncCode := 0;
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Reader.Store(bfr: TCardRec);
var
    i, count: Integer;
    bcr: T494Bcr;
    word: T494Word;
begin
    bcr := FMemory.FetchBcr(BcrIn(FChannel), True);
    word := 0;
    count := 0;
    i := Low(bfr.Columns);
    while (FInputActive and (i <= bfr.Count) and (bcr.Count > 0)) do
    begin
        word.Value := (word.Value shl 6) or (bfr.Columns[i] and $3f);
        Inc(count);
        if (count > 4) then
        begin
            FMemory.Store(bcr.Address, word, True);
            word := 0;
            count := 0;
            bcr.Address := bcr.Address + 1;
            bcr.Count := bcr.Count - 1;
            FMemory.StoreBcr(BcrIn(FChannel), bcr, True);
        end;
        Inc(i);
    end;
    if (FInputMonitor and (bcr.Count = 0)) then
    begin
        QueueInterrupt(intIO, IIsiInput(FChannel), 0);
        TerminateInput;
    end;
end;

{ T494ReaderPunch }

procedure T494ReaderPunch.ActivateInput(withMon: Boolean);
begin
    FReader.ActivateInput(withMon);
end;

procedure T494ReaderPunch.ActivateOutput(withMon: Boolean);
begin
    FPunch.ActivateOutput(withMon);
end;

procedure T494ReaderPunch.Clear;
begin
    FReader.Clear;
    FPunch.Clear;
end;

constructor T494ReaderPunch.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited;
    FFirstPass := True;
    FReader := T494Reader.Create(cpu, mem, chan);
    FPunch := T494Punch.Create(cpu, mem, chan);
end;

destructor T494ReaderPunch.Destroy;
begin
    FReader.Terminate;
    FReader.WaitFor;
    FPunch.Terminate;
    FPunch.WaitFor;
    FreeAndNil(FReader);
    FreeAndNil(FPunch);
    inherited Destroy;
end;

procedure T494ReaderPunch.Execute;
begin
    while (not Terminated) do
    begin
        if (FFirstPass) then
        begin
            FReader.Start;
            FPunch.Start;
            FFirstPass := False;
        end;
        Sleep(100);
    end;
end;

procedure T494ReaderPunch.ExternalFunction(func: T494Word);
var
    cmd: Byte;
    int: Boolean;
begin
    Lock;
    try
        // Isolate the command code and the interrupt flag
        cmd := (func.Value shr 24) and (not CINTERRUPT);
        int := ((func.Value shr 24) and CINTERRUPT) <> 0;
        if (cmd < $08) then
            FPunch.ExternalFunction(func)
        else if (cmd > $20) then
            FReader.ExternalFunction(func)
        else if (cmd = CTERMINATE) then
        begin
            FPunch.TerminateOutput;
            FReader.TerminateInput;
            if (int) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
        end;
    finally
        Unlock;
    end;
end;

function T494ReaderPunch.InputActive: Boolean;
begin
    Result := FReader.InputActive;
end;

function T494ReaderPunch.OutputActive: Boolean;
begin
    Result := FPunch.OutputActive;
end;

{ TU92Punch }

procedure T494Punch.ActivateInput(withMon: Boolean);
// Meaingless for card punch, ignore
begin
    ;
end;

procedure T494Punch.Clear;
begin
end;

constructor T494Punch.Create(cpu: T494Cpu; mem: T494Memory; chan: Byte);
begin
    inherited;
    FCardFileDir := UserDataDir;
    FOutputFile0 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
    FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    FHopperEmpty := True;
end;

destructor T494Punch.Destroy;
var
    fname: String;
begin
    fname := FOutputFile0.FileName;
    FreeAndNil(FOutputFile0);
    DeleteFile(fname);
    fname := FOutputFile1.FileName;
    FreeAndNil(FOutputFile1);
    DeleteFile(fname);
    FreeAndNil(FCurrentFile);
    inherited;
end;

procedure T494Punch.DoFeed;
begin
    Lock;
    try
        if (FPunchStationLoaded) then
        begin
            if (FHopper = 0) then
            begin
                Inc(FStacker0Count);
            end else
            begin
                Inc(FStacker1Count);
            end;
            FPunchStationLoaded := False;
        end;
        if (FReadStationLoaded) then
        begin
            FPunchStation := FReadStation;
            FPunchStationLoaded := True;
        end;
        if ((not Assigned(FCurrentFile)) or (FCurrentFile.Eof)) then
        begin
            if (not OpenNextFile) then
            begin
                FHopperEmpty := True;
                FReadStationLoaded := False;
                Exit;
            end;
        end;
        FReadStationLoaded := True;                 // mark read station as loaded
        FCurrentFile.ReadRaw(FReadStation);
        Dec(FInputCount);                           // adjust hopper counts
        if (FInputCount < 0) then
            FInputCount := 0;
    finally
        Unlock;
        if (Assigned(FOnFeed)) then
            Queue(DoOnFeed);
    end;
//    Sleep(100);                                     // Sleep 0.1 secs to simulate 600 CPM
    Sleep(10);
end;

procedure T494Punch.DoOnFeed;
begin
    if (Assigned(FOnFeed)) then
        FOnFeed(Self);
end;

procedure T494Punch.EmptyHopper(num: Integer);
begin
    Lock;
    try
        if (num = 1) then
        begin
            FStacker0Count := 0;
            FOutputFile0.Free;
            FOutputFile0 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end else
        begin
            FStacker1Count := 0;
            FOutputFile1.Free;
            FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        end;
    finally
        Unlock;
    end;
end;

procedure T494Punch.EmptyHoppers;
begin
    Lock;
    try
        FFiles.Clear;
        FreeAndNil(FCurrentFile);
        FInputCount := 0;
        FStacker0Count := 0;
        FStacker1Count := 0;
        FOutputFile0.Free;
        FOutputFile0 := TCardFileStream.Create(FCardFileDir + '\punch1.h16', fmCreate);
        FOutputFile1.Free;
        FOutputFile1 := TCardFileStream.Create(FCardFileDir + '\punch2.h16', fmCreate);
    finally
        Unlock;
    end;
end;

procedure T494Punch.Execute;
begin
    while (not Terminated) do
    begin
        if (FEvent.WaitFor(100) = wrSignaled) then
        begin
            case FFuncCode of
              0:
                Continue;
              CPUNCH_STACKER0:
                Punch(0);
              CPUNCH_STACKER1:
                Punch(1);
              else
              begin
                FFuncCode := 0;
                QueueInterrupt(intIO, IIsiExternal(FChannel), CILLEGAL_FUNCTION);
              end;
            end;
        end;
    end;
end;

procedure T494Punch.ExternalFunction(func: T494Word);
begin
    Lock;
    try
        if (FFuncCode <> 0) then
        begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), CINAPPROPRAITE_FUNCTION);
            Exit;
        end;
        FFuncCode := (func.Value shr 24) and (not CINTERRUPT);
        FWithExtInterrupt := ((func.Value shr 24) and CINTERRUPT) <> 0;
        case FFuncCode of
          0:
          begin
            QueueInterrupt(intIO, IIsiExternal(FChannel), CILLEGAL_FUNCTION);
            Exit;
          end;
          CPUNCH_TRANSLATE:
          begin
            FPunchType := rtTranslate;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          CPUNCH_IMAGE_COL:
          begin
            FPunchType := rtImageCol;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          CPUNCH_IMAGE_ROW:
          begin
            FPunchType := rtImageRow;
            FFuncCode := 0;
            if (FWithExtInterrupt) then
                QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
          end;
          else
          begin
            FEvent.SetEvent;
          end;
        end;
    finally
        Unlock;
    end;
end;

procedure T494Punch.Feed;
begin
    FHopper := 0;
    DoFeed;
end;

procedure T494Punch.Fetch(var bfr: TCardRec);
var
    i: Integer;
    bcr: T494Bcr;
    word: T494Word;
begin
    for i := 1 to 80 do
        bfr.Columns[i] :=  Ord(TCodeTranslator.AsciiToFieldata(' '));
    bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
    i := Low(bfr.Columns);
    while (FOutputActive and (i <= 80) and (bcr.Count > 0)) do
    begin
        word := FMemory.Fetch(bcr.Address, True);
        bfr.Columns[i] := (word.Value shr 24) and $3f;
        bfr.Columns[i + 1] := (word.Value shr 18) and $3f;
        bfr.Columns[i + 2] := (word.Value shr 12) and $3f;
        bfr.Columns[i + 3] := (word.Value shr 6) and $3f;
        bfr.Columns[i + 4] := word.Value and $3f;
        bcr.Address := bcr.Address + 1;
        bcr.Count := bcr.Count - 1;
        FMemory.StoreBcr(BcrOut(FChannel), bcr, True);
        Inc(i, 5);
    end;
    bfr.Count := 80;
end;

function T494Punch.HasCardToFeed: Boolean;
begin
    Result := ((not FHopperEmpty) or FReadStationLoaded or FPunchStationLoaded);
end;

procedure T494Punch.Punch(stkr: Integer);
begin
    if (not FReadStationLoaded) then                // Try to feed first card. Failure will be caught later
    begin
        Feed;
        Feed;
    end;
    FHopper := stkr;
    case FPunchType of
      rtTranslate:
        PunchTranslate;
      rtImageCol:
        PunchImageCol;
      rtImageRow:
        PunchImageRow;
    end;
    Feed;
end;

procedure T494Punch.PunchImageCol;
var
    bfr: TCardRec;
    rawBfr: TCardRec;
    bcr: T494Bcr;
begin
    if (FPunchStationLoaded) then
    begin
        Fetch(bfr);
        TCardFileStream.WriteImage(rawBfr, bfr);
        if (FHopper = 0) then
            FOutputFile0.Merge(rawBfr, FPunchStation)
        else
            FOutputFile1.Merge(rawBfr, FPunchStation);
        FOutputActive := False;
        FFuncCode := 0;
        bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
        if (FInputMonitor and (bcr.Count = 0)) then
            QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Punch.PunchImageRow;
var
    bcr: T494Bcr;
    w: T494Word;
    rawBfr: TCardRec;
    c, r, b: Integer;
    mask, bit: WORD;
begin
    if (FPunchStationLoaded) then
    begin
        rawBfr.Clear;
        bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
        r := 15;
        while (FOutputActive and (bcr.Count > 0) and (r >= 4)) do
        begin
            w := FMemory.Fetch(bcr.Address, True);
            b := 29;
            for c := 1 to 80 do
            begin
                mask := 1 shl b;
                bit := (w.Value and mask) shr b;
                rawBfr.ColumnAsWord[c] := rawBfr.ColumnAsWord[c] or (bit shl r);
                Dec(b);
                if (b > 0) then
                begin
                    bcr.Address := bcr.Address + 1;
                    bcr.Count := bcr.Count - 1;
                    if (bcr.Count = 0) then
                        Break;
                    w := FMemory.Fetch(bcr.Address, True);
                    b := 29;
                end;
            end;
            Dec(r);
        end;
        if (FHopper = 0) then
            FOutputFile0.Merge(rawBfr, FPunchStation)
        else
            FOutputFile1.Merge(rawBfr, FPunchStation);
        FOutputActive := False;
        FFuncCode := 0;
        if (FInputMonitor and (bcr.Count = 0)) then
            QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Punch.PunchTranslate;
var
    bfr: TCardRec;
    bcr: T494Bcr;
    rawBfr: TCardRec;
begin
    if (FPunchStationLoaded) then
    begin
        Fetch(bfr);
        //
        TCardFileStream.WriteFieldata(rawBfr, bfr);
        if (FHopper = 0) then
            FOutputFile0.Merge(rawBfr, FPunchStation)
        else
            FOutputFile1.Merge(rawBfr, FPunchStation);
        FOutputActive := False;
        FFuncCode := 0;
        bcr := FMemory.FetchBcr(BcrOut(FChannel), True);
        if (FInputMonitor and (bcr.Count = 0)) then
            QueueInterrupt(intIO, IIsiOutput(FChannel), 0);
        if (FWithExtInterrupt) then
            QueueInterrupt(intIO, IIsiExternal(FChannel), CNORMAL_COMPLETION);
    end else
    begin
        FFuncCode := 0;
        QueueInterrupt(intIO, IIsiExternal(FChannel), CINTERLOCK);
    end;
end;

procedure T494Punch.SaveHopper(num: Integer; fname: String);
var
    split: Integer;
begin
    split := LastDelimiter('.', fname);
    if (split = 0) then
        fname := fname + '.h16';
    if (num = 1) then
        FOutputFile0.SaveToFile(fname)
    else
        FOutputFile1.SaveToFile(fname);
end;

end.
