unit FH880File;

interface

uses SysUtils, Classes;

const
  FH880_WORDS_PER_TRACK = 1024;
  FH880_BYTES_PER_TRACK = 1024 * SizeOf(UInt32);
  FH880_TRACKS_PER_DRUM = 768;
  FH880_WORDS_PER_DRUM = FH880_TRACKS_PER_DRUM * FH880_TRACKS_PER_DRUM;
  FH880_DRUMS_PER_UNIT = 8;
  FH880_MFD_START = FH880_BYTES_PER_TRACK * 3;
  FH880_VOLUME_START = FH880_BYTES_PER_TRACK * 2;

type
  TMasterFileDirectory = class;

  TFileControlBlock = packed record
  private
    function GetSize: UInt32;
    function GetStart: UInt32;
    procedure SetSize(const Value: UInt32);
    procedure SetStart(const Value: UInt32);
  public
    Name: array [0..1] of UInt32;           // 2 word file name
    FTracks: UInt32;                        // H1 = # tracks, H2 = start track
    LastLen: UInt32;                        // # words occupied in last track
                                            // inc. link to next track in word zero
    property Size: UInt32 read GetSize write SetSize;
    property Start: UInt32 read GetStart write SetStart;
  end;

  TFH880File = class(TFileStream)
  private
    FMfd: TMasterFileDirectory;
    procedure CheckEof(addr: UInt32);
    procedure SetPosition(addr: UInt32);
  public
    constructor Create(const AFileName: string; Mode: Word);
    destructor Destroy; override;
    function AllocTrack: UInt32;
    procedure CloseFile(fcb: TFileControlBlock);
    function CreateFile(fname: AnsiString): TFileControlBlock;
    procedure DeleteFile(fname: AnsiString);
    function FileExists(fname: AnsiString; var fcb: TFileControlBlock): Boolean;
    procedure Format(vol: AnsiString);
    procedure FreeTrack(addr: UInt32);
    function ReadWord(addr: UInt32): UInt32;
    procedure WriteWord(addr: UInt32; value: UInt32);
  end;

  TMasterFileDirectory = class(TObject)
  private
    FIsOpen: Boolean;
    FDrum: TFH880File;
    FCrntTrackAddr: Uint32;
    FCrntTrack: array [0..255] of TFileControlBlock;
    procedure FileNameToFieldata(fname: AnsiString; var rslt: array of UInt32);
    function FindFile(fname: AnsiString): Integer; overload;
    function FindFile(fname: array of UInt32): Integer; overload;
  public
    constructor Create(drum: TFH880File);
    procedure CloseFile(fcb: TFileControlBlock);
    function CreateFile(fname: AnsiString): TFileControlBlock;
    procedure DeleteFile(fname: AnsiString);
    function FileExists(fname: AnsiString; var fcb: TFileControlBlock): Boolean;
    function FirstFile(var fcb: TFileControlBlock): Integer;
    function NextFile(lastOff: Integer; var fcb: TFileControlBlock): Integer;
    procedure Open;
    property IsOpen: Boolean read FIsOpen;
  end;

  TDrumFileMode = ( dfmCreate, dfmOpen );

  TDrumFile = class(TObject)
  private
    FDrum: TFH880File;
    FFcb: TFileControlBlock;
    FCrntTrack: UInt32;
    FNumTracks: UInt32;
    FBuffer: array [0..1024] of UInt32;
    FBufferOffset: Integer;
    procedure WriteTrack;
  public
    constructor Create(drum: TFH880File; fname: AnsiString; mode: TDrumFileMode);
    destructor Destroy; override;
    procedure Write(value: array of UInt32; len: Integer);
    procedure WriteText(s: AnsiString);
  end;

implementation

uses EmulatorTypes, U494Memory;

{ TFH880File }

function TFH880File.AllocTrack: UInt32;
var
    bfr: array [0..1023] of UInt32;
    i: Integer;
    w, b: UInt32;
begin
    Result := 0;
    Position := FH880_VOLUME_START;
    Read(bfr[0], FH880_BYTES_PER_TRACK);
    for i := 2 to 206 do
    begin
        if (bfr[i] <> UInt32(-1)) then
        begin
            Result := (i - 2) * 30;
            w := bfr[i];
            b := BIT29;
            while ((w and b) <> 0) do
            begin
                b := b shr 1;
                Inc(Result);
            end;
            if (b <> 0) then
            begin
                w := w or b;
                bfr[i] := w;
                Break;
            end;
        end;
    end;
    if ((Result = 0) or (Result > FH880_WORDS_PER_DRUM)) then
        raise Exception.Create('Drum full');
    Position := FH880_VOLUME_START;
    Write(bfr[0], FH880_BYTES_PER_TRACK);
end;

procedure TFH880File.CheckEof(addr: UInt32);
begin
    if ((addr * SizeOf(UInt32)) > (Size - SizeOf(UInt32))) then
        raise Exception.Create('Attempt to read/write past end of FH880 drum file');
end;

procedure TFH880File.CloseFile(fcb: TFileControlBlock);
begin
    FMfd.CloseFile(fcb);
end;

constructor TFH880File.Create(const AFileName: string; Mode: Word);
begin
    try
        inherited;
    except
      on E: Exception do
      begin
          ShowException(E, ExceptAddr);
      end;
    end;
    FMfd := TMasterFileDirectory.Create(Self);
end;

function TFH880File.CreateFile(fname: AnsiString): TFileControlBlock;
var
    i: Integer;
begin
    i := FMfd.FindFile(fname);
    if (i >= 0) then
        raise Exception.CreateFmt('File %s already exists', [fname]);
    Result := FMfd.CreateFile(fname);
    Result.SetStart(AllocTrack);
    Result.SetSize(1);
    WriteWord(Result.GetStart * FH880_WORDS_PER_TRACK, 0);
end;

procedure TFH880File.DeleteFile(fname: AnsiString);
begin
    FMfd.DeleteFile(fname);
end;

destructor TFH880File.Destroy;
begin
    FreeAndNil(FMfd);
    inherited;
end;

function TFH880File.FileExists(fname: AnsiString; var fcb: TFileControlBlock): Boolean;
begin
    Result := FMfd.FileExists(fname, fcb);
end;

procedure TFH880File.Format(vol: AnsiString);
// Write enough data to allocate an entire simulated drum and
// create the control structures required by MOS in the first few
// tracks:
//
// Track #  Function                Word #  Function
//  0       Boot block
//  1       Boot block
//  2       Volume Control Block    0-1     Volume ID
//                                  2-206   Free space bitmap
//  3       Master File Directory   0       $MFD$
//                                  1       Unused (spaces)
//                                  2       Link to 1st word of MFD (word 4 of this track)
//                                  3       Unused (zero)
//                                  4-1021  Collection of 4 word file entries
//                                  1022    End of block marker (all ones)
//                                  1023    Link to next MFD track
//
var
    i, d, t, bytesWritten: Integer;
    track: array  [1..FH880_WORDS_PER_TRACK] of UInt32;
begin
    // Init track buffer
    for i := 1 to FH880_WORDS_PER_TRACK do
        track[i] := 0;
    // Write enough data to fill the simulated drum
    for d := 1 to FH880_DRUMS_PER_UNIT do
    begin
        for t := 1 to FH880_TRACKS_PER_DRUM do
        begin
            bytesWritten := Write(track, FH880_BYTES_PER_TRACK);
            if (bytesWritten <> FH880_BYTES_PER_TRACK) then
                raise Exception.Create('OOPS! Format failed');
        end;
    end;
    Position := FH880_VOLUME_START;
    // Write the volume ID
    vol := TCodeTranslator.AsciiToFieldata(AnsiString(Copy(String(vol) + StringOfChar(' ', 10), 1, 10)));
    track[1] := (Ord(vol[1]) shl 24) or
                (Ord(vol[2]) shl 18) or
                (Ord(vol[3]) shl 12) or
                (Ord(vol[4]) shl 6) or
                Ord(vol[5]);
    track[2] := (Ord(vol[6]) shl 24) or
                (Ord(vol[7]) shl 18) or
                (Ord(vol[8]) shl 12) or
                (Ord(vol[9]) shl 6) or
                Ord(vol[10]);
    Write(track, 2 * SizeOf(UInt32));
    // Initialize the bitmap to show first 4 tracks used and last 6 tracks
    // (those > 6144) unavailable
    track[1] := $f shl 26;
    track[2] := 0;
    track[205] := $3f;
    Write(track, 205 * SizeOf(UInt32));
    // Write MFD header
    Position := FH880_MFD_START;
    track[1] := (Ord(TCodeTranslator.AsciiToFieldata('$')) shl 24) or
                (Ord(TCodeTranslator.AsciiToFieldata('M')) shl 18) or
                (Ord(TCodeTranslator.AsciiToFieldata('F')) shl 12) or
                (Ord(TCodeTranslator.AsciiToFieldata('D')) shl 6) or
                Ord(TCodeTranslator.AsciiToFieldata('$'));
    track[2] := (Ord(TCodeTranslator.AsciiToFieldata(' ')) shl 24) or
                (Ord(TCodeTranslator.AsciiToFieldata(' ')) shl 18) or
                (Ord(TCodeTranslator.AsciiToFieldata(' ')) shl 12) or
                (Ord(TCodeTranslator.AsciiToFieldata(' ')) shl 6) or
                Ord(TCodeTranslator.AsciiToFieldata(' '));
    track[3] := 3;
    track[207] := 0;
    track[1023] := UInt32(-1);
    Write(track, FH880_BYTES_PER_TRACK);
end;

procedure TFH880File.FreeTrack(addr: UInt32);
var
    bfr: array [0..1023] of UInt32;
    w, b: UInt32;
begin
    Position := FH880_VOLUME_START;
    Read(bfr[0], FH880_BYTES_PER_TRACK);
    w := (addr div 30) + 2;
    b := (addr mod 30);
    bfr[w] := bfr[w] and (not (BIT29 shr b));
    Position := FH880_VOLUME_START;
    Write(bfr[0], FH880_BYTES_PER_TRACK
);
end;

function TFH880File.ReadWord(addr: UInt32): UInt32;
var
    wrd: UInt32;
begin
    addr := addr;
    if (addr <> Position) then
        SetPosition(addr)
    else
        CheckEof(addr);
    Read(wrd, SizeOf(UInt32));
    Result := wrd;
end;

procedure TFH880File.SetPosition(addr: UInt32);
begin
    CheckEof(addr);
    Position := addr * SizeOf(UInt32);
end;

procedure TFH880File.WriteWord(addr, value: UInt32);
begin
    addr := addr;
    if (addr <> Position) then
        SetPosition(addr)
    else
        CheckEof(addr);
    Write(value, SizeOf(UInt32));
end;

{ TMasterFileDirectory }

procedure TMasterFileDirectory.CloseFile(fcb: TFileControlBlock);
var
    i: Integer;
begin
    i := FindFile(fcb.Name);
    FCrntTrack[i].FTracks := fcb.FTracks;
    FCrntTrack[i].LastLen := fcb.LastLen;
    FDrum.Position := FCrntTrackAddr;
    FDrum.Write(FCrntTrack[0], FH880_BYTES_PER_TRACK);
end;

constructor TMasterFileDirectory.Create(drum: TFH880File);
begin
    inherited Create;
    FDrum := drum;
end;

function TMasterFileDirectory.CreateFile(fname: AnsiString): TFileControlBlock;
var
    i: Integer;
begin
    i := FindFile('$FREE$');
    if (i < 0) then
        raise Exception.Create('MFD full');
    Result := FCrntTrack[i];
    FileNameToFieldata(fname, Result.Name);
    Result.FTracks := 0;
    Result.LastLen := 0;
    FDrum.Position := FCrntTrackAddr;
    FCrntTrack[i] := Result;
    FDrum.Write(FCrntTrack[0], FH880_BYTES_PER_TRACK);
end;

procedure TMasterFileDirectory.DeleteFile(fname: AnsiString);
var
    i: Integer;
    fcb: TFileControlBlock;
    trackAddr, nextTrack: UInt32;
begin
    if (Trim(String(fname)) = '$MFD$') then
        raise Exception.Create('Deleting the MFD is not allowed');
    i := FindFile(fname);
    if (i >= 0) then
    begin
        fcb := FCrntTrack[i];
        fcb.Name[0] := 0;
        fcb.Name[1] := 0;
        trackAddr := fcb.GetStart;
        while (trackAddr <> 0) do
        begin
            nextTrack := FDrum.ReadWord(trackAddr * FH880_WORDS_PER_TRACK);
            FDrum.FreeTrack(trackAddr);
            trackAddr := nextTrack;
        end;
        fcb.FTracks := 0;
        fcb.LastLen := 0;
        FCrntTrack[i] := fcb;
        FDrum.Position := FCrntTrackAddr;
        FDrum.Write(FCrntTrack[0], FH880_BYTES_PER_TRACK);
    end;
end;

function TMasterFileDirectory.FileExists(fname: AnsiString; var fcb: TFileControlBlock): Boolean;
var
    i: Integer;
begin
    i := FindFile(fname);
    if (i >= 0) then
    begin
        fcb := FCrntTrack[i];
        Result := True;
    end else
        Result := False;
end;

procedure TMasterFileDirectory.FileNameToFieldata(fname: AnsiString; var rslt: array of UInt32);
// Convert file name to Fieldata and load into 2 words
var
    i, shft: Integer;
    c, f: AnsiChar;
begin
    rslt[0] := 0;
    rslt[1] := 0;
    i := 0;
    shft := 24;
    fname := AnsiString(Copy(UpperCase(String(fname)) + StringOfChar(' ', 10), 1, 10));
    for c in fname do
    begin
        f := TCodeTranslator.AsciiToFieldata(c);
        rslt[i] := rslt[i] or (UInt32(Ord(f)) shl shft);
        Dec(shft, 6);
        if (shft < 0) then
        begin
            Inc(i);
            shft := 24;
        end;
    end;
end;

function TMasterFileDirectory.FindFile(fname: array of UInt32): Integer;
// Find a file and returns it's index in the current MFB track
var
    lfcb: TFileControlBlock;
    done: Boolean;
begin
    Open;
    //
    done := False;
    while (not done) do
    begin
        for Result := 0 to 254 do
        begin
            lfcb := FCrntTrack[Result];
            if ((lfcb.Name[0] = fname[0]) and (lfcb.Name[1] = fname[1])) then
                Exit;
        end;
        if (FCrntTrack[255].LastLen <> 0) then
        begin
            // Read next MFD track
            FDrum.Position := FCrntTrack[255].LastLen * FH880_BYTES_PER_TRACK;
            FCrntTrackAddr := FDrum.Position;
            FDrum.Read(FCrntTrack[0], SizeOf(FCrntTrack));
        end else
            done := True;
    end;
    Result := -1;
end;

function TMasterFileDirectory.FindFile(fname: AnsiString): Integer;
var
    fdname: array [0..1] of UInt32;
begin
    // Convert file name to Fieldata and load into 2 words
    if (fname = '$FREE$') then
    begin
        fdname[0] := 0;
        fdname[1] := 0;
    end else
        FileNameToFieldata(fname, fdname);
    Result := FindFile(fdname);
end;

function TMasterFileDirectory.FirstFile(var fcb: TFileControlBlock): Integer;
begin
    Open;
    Result := NextFile(-1, fcb);
end;

function TMasterFileDirectory.NextFile(lastOff: Integer; var fcb: TFileControlBlock): Integer;
var
    i: Integer;
    done: Boolean;
begin
    Result := -1;
    Inc(lastOff);
    done := False;
    while (not done) do
    begin
        for i := lastOff to 254 do
        begin
            fcb := FCrntTrack[i];
            if ((fcb.Name[0] <> 0) and (fcb.Name[1] <> 0)) then
            begin
                Result := i;
                Exit;
            end;
        end;
        if (FCrntTrack[255].LastLen <> 0) then
        begin
            // Read next MFD track
            FDrum.Position := FCrntTrack[255].LastLen * FH880_BYTES_PER_TRACK;
            FCrntTrackAddr := FDrum.Position;
            FDrum.Read(FCrntTrack[0], SizeOf(FCrntTrack));
        end else
            done := True;
    end;
end;

procedure TMasterFileDirectory.Open;
begin
    FDrum.Position := FH880_MFD_START;
    FCrntTrackAddr := FH880_MFD_START;
    FDrum.Read(FCrntTrack[0], SizeOf(FCrntTrack));
    FIsOpen := True;
end;

{ TDrumFile }

constructor TDrumFile.Create(drum: TFH880File; fname: AnsiString; mode: TDrumFileMode);
begin
    inherited Create;
    FDrum := drum;
    if (mode = dfmCreate) then
    begin
        FFcb := drum.CreateFile(fname);
    end else
    begin
        ;
    end;
    FCrntTrack := FFcb.GetStart;
    FBufferOffset := 1;
end;

destructor TDrumFile.Destroy;
begin
    if (FBufferOffset > 1) then
    begin
        FDrum.Position := FCrntTrack * FH880_BYTES_PER_TRACK;
        FDrum.Write(FBuffer[0], FH880_BYTES_PER_TRACK);
        Inc(FNumTracks);
    end;
    FFcb.SetSize(FNumTracks);
    FDrum.CloseFile(FFcb);
    inherited;
end;

procedure TDrumFile.Write(value: array of UInt32; len: Integer);
var
    i: Integer;
begin
    for i := 0 to len - 1 do
    begin
        FBuffer[FBufferOffset] := value[i];
        Inc(FBufferOffset);
        if (FBufferOffset > 1023) then
            WriteTrack;
    end;
    FFcb.LastLen := FBufferOffset;
end;

procedure TDrumFile.WriteText(s: AnsiString);
var
    w: UInt32;
    shft: Integer;
    c: AnsiChar;
begin
    s := AnsiString(TrimRight(String(s)));
    s := s + AnsiString(StringOfChar(' ', 5 - (Length(s) mod 5)));
    w := 0;
    shft := 24;
    for c in s do
    begin
        w := w or (Ord(TCodeTranslator.AsciiToFieldata(c)) shl shft);
        Dec(shft, 6);
        if (shft < 0) then
        begin
            FBuffer[FBufferOffset] := w;
            Inc(FBufferOffset);
            if (FBufferOffset > 1023) then
                WriteTrack;
            shft := 24;
            w := 0;
        end;
    end;
    // Write last word if any
    if (w <> 0) then
    begin
        FBuffer[FBufferOffset] := w;
        Inc(FBufferOffset);
        if (FBufferOffset > 1023) then
            WriteTrack;
    end;
    // Write end of line sentinel
    FBuffer[FBufferOffset] := UInt32(-1);
    Inc(FBufferOffset);
    if (FBufferOffset > 1023) then
        WriteTrack;
    FFcb.LastLen := FBufferOffset;
end;

procedure TDrumFile.WriteTrack;
begin
    FBuffer[0] := FDrum.AllocTrack;
    Inc(FNumTracks);
    FDrum.Position := FCrntTrack * FH880_BYTES_PER_TRACK;
    FDrum.Write(FBuffer[0], FH880_BYTES_PER_TRACK);
    FCrntTrack := FBuffer[0];
    FBuffer[0] := 0;
    FBufferOffset := 1;
end;

{ TFileControlBlock }

function TFileControlBlock.GetSize: UInt32;
begin
    Result := (FTracks shr 15) and BITS15;
end;

function TFileControlBlock.GetStart: UInt32;
begin
    Result := FTracks and BITS15;
end;

procedure TFileControlBlock.SetSize(const Value: UInt32);
begin
    FTracks := (FTracks and BITS15) or ((Value and BITS15) shl 15);
end;

procedure TFileControlBlock.SetStart(const Value: UInt32);
begin
    FTracks := FTracks or (Value and BITS15);
end;

end.
