unit U9200Types;

interface

uses Windows, WinApi.SHFolder, Messages, SysUtils, Classes, Generics.Collections,
     Generics.Defaults, Forms;

const
    // LED colours
    LC_WHITE = 1;
    LC_GREEN = 2;
    LC_RED = 3;

    // Lamp IDs
    LAMP_POWER = 1;
    LAMP_PRINTER_ABN = 2;
    LAMP_READER_ABN = 3;
    LAMP_PUNCH_ABN = 4;
    LAMP_RUN = 5;
    LAMP_STOP = 6;
    LAMP_PROC_ABN = 7;
    LAMP_TEST = 8;

    // Lamp states
    LAMP_ON = 1;
    LAMP_OFF = 0;

    // MUX channel status bits
    MUX_ATTENTION = $80;
    MUX_STATUS_MODIFIER = $40;
    MUX_CONTROL_UNIT_END = $20;
    MUX_BUSY = $10;
    MUX_CHANNEL_END = $08;
    MUX_DEVICE_END = $04;
    MUX_UNIT_CHECK = $02;
    MUX_EXCEPTION = $01;

type
  EMemoryError = class(Exception)
  end;

  EProcessorError = class(Exception)
  end;

  EDecimalError = class(Exception)
  end;

  TU92States = ( usPowerOff, usPowerOn, usInitializing );
  TU92State = set of TU92States;

  TU92CPUStates = ( ucsProcessor, ucsIO, ucsSingleStep, ucsHalted, ucsStalled, ucsError,
                    ucsInstructionFetched );
  TU92CPUState = set of TU92CPUStates;

  TAurStopWatch = class(TObject)
  private
    FStartTics: Int64;
    FStopTics: Int64;
  public
    function ElapsedMS: Int64;
    procedure Start;
    procedure Stop;
  end;

  TOpcodeProc = procedure(fal: Byte; fad1, fad2: Smallint) of object;

  TInstType = ( itDirective, itRX, itSI, itSS1, itSS2, itBranch, itUnknown );

  TU92Opcode = record
  public
    Opcode: Byte;
    Length: Byte;
    Proc: TOpcodeProc;
    InstType: TInstType;
    Code: String;
  end;

  TU92OpcodeList = class (TList<TU92Opcode>)
  public
    function FindOpcode(opcode: Byte): TU92Opcode;
    function IsOpcode(opcode: Byte): Boolean;
  end;

  TLampUpdateEvent = procedure(Sender: TObject; lamp, state: Integer) of Object;
  TExecuteInstructionEvent = procedure(Sender: TObject) of object;
  TFetchInstructionEvent = procedure(Sender: TObject) of object;
  THaltEvent = procedure(Sender: TObject) of object;
  TErrorEvent = procedure(Sender: TObject; E: Exception) of object;

  function Opcode(op, length: Byte; proc: TOpcodeProc; code: String = ''; typ: TInstType = itUnknown): TU92Opcode;

implementation

function GetDir(csidl: Integer): String;
var
    stat: Integer;
    path: array [0 .. MAX_PATH] of Char;
begin
    stat := ShGetFolderPath(0, csidl, 0, SHGFP_TYPE_CURRENT, path);
    if (stat = 0) then
    begin
        SetString(Result, path, StrLen(path));
        if (Result[Length(Result)] = '\') then
            Result := Copy(Result, 1, Length(Result) - 1);
    end
    else
    begin
        Result := '.';
    end;
end;

function Opcode(op, length: Byte; proc: TOpcodeProc; code: String; typ: TInstType): TU92Opcode;
begin
    Result.Opcode := op;
    Result.Length := length;
    Result.Proc := proc;
    Result.InstType := typ;
    Result.Code := code;
end;

{ TAurStopWatch }

function TAurStopWatch.ElapsedMS: Int64;
var
    stopTics: Int64;
    freq: Int64;
begin
    QueryPerformanceFrequency(freq);
    if (FStopTics = 0) then
        QueryPerformanceCounter(stopTics)
    else
        stopTics := FStopTics;
    Result := Round(((stopTics - FStartTics) / freq) * 1000);
end;

procedure TAurStopWatch.Start;
begin
    QueryPerformanceCounter(FStartTics);
    FStopTics := 0;
end;

procedure TAurStopWatch.Stop;
begin
    QueryPerformanceCounter(FStopTics);
end;


{ TU92OpcodeList }

function TU92OpcodeList.FindOpcode(opcode: Byte): TU92Opcode;
var
    compare: TComparison<TU92Opcode>;
    i: Integer;
    oprec: TU92Opcode;
begin
    compare := function(const Left, Right: TU92Opcode): Integer
    begin
        if (Left.Opcode < Right.Opcode) then
            Result := -1
        else if (Left.Opcode > Right.Opcode) then
            Result := 1
        else
            Result := 0;
    end;

    oprec.Opcode := opcode;
    if (not BinarySearch(oprec, i, TComparer<TU92Opcode>.Construct(compare))) then
        raise EProcessorError.Create('Illegal opcode');
    Result := Self[i];
end;

function TU92OpcodeList.IsOpcode(opcode: Byte): Boolean;
begin
    try
        FindOpcode(opcode);
        Result := True;
    except
        Result := False;
    end;
end;

end.
