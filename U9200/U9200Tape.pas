unit U9200Tape;

interface

// Uniservo VI C emulator

uses SysUtils, Classes, Dialogs, Generics.Collections, SyncObjs, TapeFile, U9200Device;

const
  // Sense byte 0 bits
  U6C_SENSE0_INVALID_FUNCTION = $80;
  U6C_SENSE0_INTERVENTION_REQUIRED = $40;
  U6C_SENSE0_BUS_OUT_CHECK = $20;
  U6C_SENSE0_EQUIPMENT_CHECK = $10;
  U6C_SENSE0_DATA_CHECK = $08;
  U6C_SENSE0_DATA_LATE = $04;
  U6C_SENSE0_WORD_COUNT_ZERO = $02;
  U6C_SENSE0_DATA_CONVERTER_CHECK = $01;
  // Sense byte 1 bits
  U6C_SENSE1_NOISE = $80;
  U6C_SENSE1_TAPE_UNIT_STATUS_A = $40;
  U6C_SENSE1_TAPE_UNIT_STATUS_B = $20;
  U6C_SENSE1_7_TRACK = $10;
  U6C_SENSE1_LOAD_POINT = $08;
  U6C_SENSE1_END_OF_TAPE = $04;
  U6C_SENSE1_FILE_PROTECT = $02;
  // Sense byte 2 not used
  // Sense byte 3 bits
  U6C_SENSE3_READ_VP_ERROR = $80;
  U6C_SENSE3_LP_ERROR = $40;
  U6C_SENSE3_SKEW = $20;
  U6C_SENSE3_CRC_READ_ERROR = $10;
  U6C_SENSE3_WRITE_VP_ERROR = $08;
  U6C_SENSE3_BACKWARD = $02;
  // Sense byte 4 bits
  U6C_SENSE4_RUNAWAY_CHECK = $80;
  U6C_SENSE4_TAPE_MOTION_FAULT = $40;
  U6C_SENSE4_STALL = $04;
  U6C_SENSE4_TAPE_FAULT = $02;
  //
  U6C_MAX_BLOCK_SIZE = 8192;

type
  TU92TapeBlock = record
  public
    Length: Smallint;
    Buffer: array [0..U6C_MAX_BLOCK_SIZE-1] of Byte;
  end;

  TU92Tape = class(TObject)
  private
    function GetFileName: String;
    function GetFilePosition: Int64;
    function GetFileSize: Int64;
  protected
    FAddress: Byte;
    FSense: array [0..4] of Byte;
    FState: TU92DeviceState;
    FStatus: Byte;
    FStatusPending: Boolean;
    FTapeFile: TTapeFile;
    FLock: TCriticalSection;
    procedure ClearSenseBytes;
    function GetEot: Boolean;
    function GetInterventionRequired: Boolean;
    function GetInvalidFunction: Boolean;
    function GetLoadPoint: Boolean;
    function GetMounted: Boolean;
    function GetOnline: Boolean;
    function GetSenseBytes(idx: Integer): Byte;
    function GetStatus: Byte;
    function GetWriteEnable: Boolean;
    procedure Lock;
    procedure PostStatus(stat: Byte);
    procedure SetEot(const Value: Boolean);
    procedure SetInterventionRequired(const Value: Boolean);
    procedure SetInvalidFunction(const Value: Boolean);
    procedure SetOnLine(const Value: Boolean);
    procedure SetLoadPoint(const Value: Boolean);
    procedure SetWriteEnable(const Value: Boolean);
    procedure Unlock;
  public
    constructor Create(addr: Byte);
    destructor Destroy; override;
    procedure BackSpaceBlock;
    procedure BackSpaceFile;
    procedure Erase;
    procedure ForwardSpaceBlock;
    procedure ForwardSpaceFile;
    procedure ManualRewind;
    function ModeSet(mode, density: Byte): Byte;
    procedure Mount(fname: String; writeEnable: Boolean);
    procedure ReadBackward(var bfr: TU92TapeBlock);
    procedure ReadForward(var bfr: TU92TapeBlock);
    procedure Rewind;
    procedure RewindWithLock;
    procedure Sense(var bfr: TU92TapeBlock);
    function Test: Byte;
    procedure Unmount;
    procedure Write(const bfr: TU92TapeBlock);
    procedure WriteTapeMark;
    property Address: Byte read FAddress;
    property Eot: Boolean read GetEot;
    property FileName: String read GetFileName;
    property FilePosition: Int64 read GetFilePosition;
    property FileSize: Int64 read GetFileSize;
    property InterventionRequired: Boolean read GetInterventionRequired write SetInterventionRequired;
    property InvalidFunction: Boolean read GetInvalidFunction write SetInvalidFunction;
    property OnLine: Boolean read GetOnline write SetOnLine;
    property LoadPoint: Boolean read GetLoadPoint;
    property Mounted: Boolean read GetMounted;
    property SenseBytes[idx: Integer]: Byte read GetSenseBytes;
    property State: TU92DeviceState read FState;
    property Status: Byte read GetStatus;
    property StatusPending: Boolean read FStatusPending;
    property WriteEnable: Boolean read GetWriteEnable;
  end;

  TU92TapeList = class(TObjectList<TU92Tape>)
  private
    function GetDevices(addr: Byte): TU92Tape;
  public
    constructor Create;
    property Devices[addr: Byte]: TU92Tape read GetDevices;
  end;

implementation

uses U9200Types;

{ TU92TapeList }

constructor TU92TapeList.Create;
begin
    inherited;
    OwnsObjects := False;
end;

function TU92TapeList.GetDevices(addr: Byte): TU92Tape;
var
    dev: TU92Tape;
begin
    for dev in Self do
    begin
        if (dev.Address = addr) then
        begin
            Result := dev;
            Exit;
        end;
    end;
    Result := nil;
end;

{ TU92Tape }

procedure TU92Tape.BackSpaceBlock;
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.BackwardSpaceBlock;
            stat := MUX_DEVICE_END;
            SetEot(False);
            if (FTapeFile.Eof) then
                stat := stat or MUX_EXCEPTION;
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.BackSpaceFile;
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.BackwardSpaceFile;
            stat := MUX_DEVICE_END;
            SetEot(False);
            if (FTapeFile.Bot) then
                stat := stat or MUX_EXCEPTION;
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.ClearSenseBytes;
begin
    FSense[0] := FSense[0] and U6C_SENSE0_INTERVENTION_REQUIRED;
    FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_NOISE);
    FSense[2] := 0;
    FSense[3] := 0;
    FSense[4] := 0;
end;

constructor TU92Tape.Create(addr: Byte);
begin
    inherited Create;
    FLock := TCriticalSection.Create;
    FAddress := addr;
    FState := [udsReady];
    FSense[0] := U6C_SENSE0_INTERVENTION_REQUIRED;
    FSense[1] := U6C_SENSE1_TAPE_UNIT_STATUS_B or U6C_SENSE1_FILE_PROTECT;
    FSense[2] := 0;
    FSense[3] := 0;
    FSense[4] := 0;
end;

destructor TU92Tape.Destroy;
begin
    FreeAndNil(FLock);
    inherited Destroy;
end;

procedure TU92Tape.Erase;
begin
    ClearSenseBytes;
    if (OnLine) then
    begin
        FTapeFile.Erase;
        PostStatus(MUX_DEVICE_END);
    end else
        PostStatus(MUX_UNIT_CHECK);
end;

procedure TU92Tape.ForwardSpaceBlock;
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.ForwardSpaceBlock;
            stat := MUX_DEVICE_END;
            SetLoadPoint(False);
            if (FTapeFile.Eof) then
                stat := stat or MUX_EXCEPTION;
            SetEot(FTapeFile.Eot);
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.ForwardSpaceFile;
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.ForwardSpaceFile;
            stat := MUX_DEVICE_END;
            SetLoadPoint(False);
            if (FTapeFile.Eot) then
                stat := stat or MUX_EXCEPTION;
            SetEot(FTapeFile.Eot);
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

function TU92Tape.GetEot: Boolean;
begin
    Result := ((FSense[1] and U6C_SENSE1_END_OF_TAPE) <> 0);
end;

function TU92Tape.GetFileName: String;
begin
    if (Assigned(FTapeFile)) then
        Result := FTapeFile.FileName
    else
        Result := '';
end;

function TU92Tape.GetFilePosition: Int64;
begin
    if (Assigned(FTapeFile)) then
        Result := FTapeFile.Position
    else
        Result := 0;
end;

function TU92Tape.GetFileSize: Int64;
begin
    if (Assigned(FTapeFile)) then
        Result := FTapeFile.Size
    else
        Result := 0;
end;

function TU92Tape.GetInterventionRequired: Boolean;
begin
    Result := ((FSense[0] and U6C_SENSE0_INTERVENTION_REQUIRED) <> 0);
end;

function TU92Tape.GetInvalidFunction: Boolean;
begin
    Result := ((FSense[0] and U6C_SENSE0_INVALID_FUNCTION) <> 0);
end;

function TU92Tape.GetLoadPoint: Boolean;
begin
    Result := ((FSense[1] and U6C_SENSE1_LOAD_POINT) <> 0);
end;

function TU92Tape.GetMounted: Boolean;
begin
    Result := Assigned(FTapeFile);
end;

function TU92Tape.GetOnline: Boolean;
begin
    Result := ((udsReady in FState) and (udsOnLine in FState));
end;

function TU92Tape.GetSenseBytes(idx: Integer): Byte;
begin
    if ((idx >= 0) and (idx <= 4)) then
        Result := FSense[idx]
    else
        raise Exception.Create('Invalid sense byte number');
end;

function TU92Tape.GetStatus: Byte;
begin
    Lock;
    try
        Result := FStatus;
        FStatus := 0;
        FStatusPending := False;
    finally
        Unlock;
    end;
end;

function TU92Tape.GetWriteEnable: Boolean;
begin
    Result := ((FSense[1] and U6C_SENSE1_FILE_PROTECT) = 0);
end;

procedure TU92Tape.Lock;
begin
    FLock.Acquire;
end;

procedure TU92Tape.ManualRewind;
begin
    FTapeFile.Rewind;
    SetLoadPoint(True);
end;

function TU92Tape.ModeSet(mode, density: Byte): Byte;
begin
    // Since things such as 7-track, 9-track, data conversion and recording density are
    // meaningless to this emulator, this function is implemented as a NOP.
    if (not OnLine) then
    begin
        Result := MUX_UNIT_CHECK;
    end else
        Result := MUX_DEVICE_END or MUX_CHANNEL_END;
end;

procedure TU92Tape.Mount(fname: String; writeEnable: Boolean);
var
    mode: Word;
begin
    FreeAndNil(FTapeFile);
    if (FileExists(fname)) then
    begin
        if (writeEnable) then
            mode := fmOpenReadWrite
        else
            mode := fmOpenRead;
    end else
        mode := fmCreate;
    mode := mode or fmShareExclusive;
    FTapeFile := TTapeFile.Create(fname, mode);
    SetLoadPoint(True);
    SetWriteEnable(writeEnable);
end;

procedure TU92Tape.PostStatus(stat: Byte);
begin
    Lock;
    try
        FStatus := stat;
        FStatusPending := True;
    finally
        Unlock;
    end;
end;

procedure TU92Tape.ReadBackward(var bfr: TU92TapeBlock);
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.ReadBackward(@(bfr.Buffer[0]), bfr.Length);
            stat := MUX_CHANNEL_END or MUX_DEVICE_END;
            SetEot(False);
            if (FTapeFile.Eof) then
                stat := stat or MUX_EXCEPTION;
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.ReadForward(var bfr: TU92TapeBlock);
var
    stat: Byte;
begin
    ClearSenseBytes;
    try
        if (OnLine) then
        begin
            FTapeFile.ReadForward(@(bfr.Buffer[0]), bfr.Length);
            stat := MUX_CHANNEL_END or MUX_DEVICE_END;
            SetLoadPoint(False);
            if (FTapeFile.Eof) then
                stat := stat or MUX_EXCEPTION;
            SetEot(FTapeFile.Eot);
            PostStatus(stat);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.Rewind;
begin
    ClearSenseBytes;
    if (not OnLine) then
    begin
        PostStatus(MUX_UNIT_CHECK);
    end else
    begin
        FTapeFile.Rewind;
        SetLoadPoint(True);
        PostStatus(MUX_DEVICE_END);
    end;
end;

procedure TU92Tape.RewindWithLock;
begin
    ClearSenseBytes;
    if (not OnLine) then
    begin
        PostStatus(MUX_UNIT_CHECK);
    end else
    begin
        Unmount;
        PostStatus(MUX_DEVICE_END);
    end;
end;

procedure TU92Tape.Sense(var bfr: TU92TapeBlock);
var
    i: Integer;
begin
    for i := 0 to 4 do
        bfr.Buffer[i] := FSense[i];
    bfr.Length := 5;
    PostStatus(MUX_CHANNEL_END or MUX_DEVICE_END);
end;

procedure TU92Tape.SetEot(const Value: Boolean);
begin
    if (Value) then
        FSense[1] := FSense[1] or U6C_SENSE1_END_OF_TAPE
    else
        FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_END_OF_TAPE);
end;

procedure TU92Tape.SetInterventionRequired(const Value: Boolean);
begin
    if (Value) then
        FSense[0] := FSense[0] or U6C_SENSE0_INTERVENTION_REQUIRED
    else
        FSense[0] := FSense[0] and ($ffffffff xor U6C_SENSE0_INTERVENTION_REQUIRED);
end;

procedure TU92Tape.SetInvalidFunction(const Value: Boolean);
begin
    FSense[0] := FSense[0] or U6C_SENSE0_INVALID_FUNCTION;
end;

procedure TU92Tape.SetLoadPoint(const Value: Boolean);
begin
    if (Value) then
        FSense[1] := FSense[1] or U6C_SENSE1_LOAD_POINT
    else
        FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_LOAD_POINT);
end;

procedure TU92Tape.SetOnLine(const Value: Boolean);
begin
    if (Value) then
    begin
        FState := FState + [udsOnLine];
        FSense[0] := FSense[0] and ($ffffffff xor U6C_SENSE0_INTERVENTION_REQUIRED);
        FSense[1] := FSense[1] or U6C_SENSE1_TAPE_UNIT_STATUS_A;
        FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_TAPE_UNIT_STATUS_B);
    end else
    begin
        FState := FState - [udsOnLine];
        FSense[0] := FSense[0] or U6C_SENSE0_INTERVENTION_REQUIRED;
        FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_TAPE_UNIT_STATUS_A);
        FSense[1] := FSense[1] or U6C_SENSE1_TAPE_UNIT_STATUS_B;
    end;
end;

procedure TU92Tape.SetWriteEnable(const Value: Boolean);
begin
    if (Value) then
        FSense[1] := FSense[1] and ($ffffffff xor U6C_SENSE1_FILE_PROTECT)
    else
        FSense[1] := FSense[1] or U6C_SENSE1_FILE_PROTECT;
end;

function TU92Tape.Test: Byte;
begin
    Lock;
    try
        ClearSenseBytes;
        if (FStatusPending) then
        begin
            Result := FStatus;
            FStatusPending := False;
        end else
            Result := 0;
    finally
        Unlock;
    end;
end;

procedure TU92Tape.Unlock;
begin
    FLock.Release;
end;

procedure TU92Tape.Unmount;
begin
    FreeAndNil(FTapeFile);
    SetLoadPoint(False);
    SetWriteEnable(False);
    SetOnline(False);
end;

procedure TU92Tape.Write(const bfr: TU92TapeBlock);
begin
    ClearSenseBytes;
    try
        if (OnLine and WriteEnable) then
        begin
            FTapeFile.Write(@(bfr.Buffer[0]), bfr.Length);
            SetLoadPoint(False);
            PostStatus(MUX_CHANNEL_END or MUX_DEVICE_END);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

procedure TU92Tape.WriteTapeMark;
begin
    ClearSenseBytes;
    try
        if (OnLine and WriteEnable) then
        begin
            FTapeFile.WriteTapeMark;
            SetLoadPoint(False);
            PostStatus(MUX_CHANNEL_END or MUX_DEVICE_END);
        end else
            PostStatus(MUX_UNIT_CHECK);
    except
      on E: Exception do
      begin
        PostStatus(MUX_UNIT_CHECK or MUX_CHANNEL_END or MUX_DEVICE_END);
        ShowMessage(E.Message);
      end;
    end;
end;

end.
