unit U0773;

interface

uses SysUtils, Classes,
     Channels, IPC;

const
  // 0773 printer command codes and flags
  PRN_LOAD_VFB = $63;
  PRN_LOAD_LCB = $FB;
  PRN_PRINT = $01;
  PRN_ADVANCE = $07;
  PRN_LOAD_PRINT_BFR = $E3;
  PRN_READ_PRINT_BFR = $02;
  PRN_READ_LCB = $0A;
  PRN_READ_VFB = $12;
  PRN_DIAGNOSE = $05;
  PRN_SENSE = $04;
  // Sense byte 0
  PRN_CMD_REJECT = $80;
  PRN_INTERVENTION = $40;
  PRN_EQUIP_CHECK = $10;
  PRN_DATA_CHECK = $08;
  PRN_OVERRUN = $04;
  PRN_STOP = $02;
  PRN_DEVICE_CHECK = $01;
  // Sense byte 1
  PRN_FORMS_OUT = $80;
  PRN_FORMS_LOW = $40;
  PRN_VFB_CHECK = $20;
  PRN_FORMS_CHECK = $10;
  PRN_BAND_CHECK = $08;
  PRN_PRINT_BFR_PARITY = $04;
  PRN_VFB_PARITY = $02;
  PRN_LCB_PARITY = $01;

type
  T0773 = class(TIPCDevice)
  private
    FSense: array [0..1] of Byte;
    FLCB: array [0..65] of Byte;
    FVFB: array [0..143] of Byte;
    FPrintBfr: AnsiString;
    FLineNum: Integer;
    FCommand: Byte;
    FBCW: TIPCBCW;
    FPrintFile: TFileStream;
    FLcbLoaded: Boolean;
    FVfbLoaded: Boolean;
    FOverflow: Boolean;
    function CheckInit: Boolean;
    procedure ClearSense;
    procedure DeviceEnd;
    procedure DoAdvance;
    procedure DoDiagnose;
    procedure DoLoadLcb;
    procedure DoLoadPrintBfr;
    procedure DoLoadVfb;
    procedure DoPrint;
    procedure DoReadLcb;
    procedure DoReadPrintBfr;
    procedure DoReadVfb;
    procedure DoSense;
    function FetchBuffer(var bfr: AnsiString): Boolean;
    procedure Overflow;
    function StoreBuffer(bfr: PByte; len: Integer): Boolean;
  public
    constructor Create(num: Byte); override;
    destructor Destroy; override;
    procedure ProcessCommand; override;
    procedure SIO; override;
  end;

implementation

uses EmulatorTypes, Globals, Memory, U9030Types;

{ T0773 }

function T0773.CheckInit: Boolean;
begin
    Result := True;
    ClearSense;
    if (not FVfbLoaded) then
    begin
        FSense[1] := PRN_VFB_PARITY;
        FChannel.QueueStatus(MakeStatus(DEVICE_END OR UNIT_CHECK, 0));
        Result := False;
    end;
    if (not FLcbLoaded) then
    begin
        FSense[1] := PRN_LCB_PARITY;
        FChannel.QueueStatus(MakeStatus(DEVICE_END OR UNIT_CHECK, 0));
        Result := False;
    end;
end;

procedure T0773.ClearSense;
begin
    FillChar(FSense, SizeOf(FSense), 0);
end;

constructor T0773.Create(num: Byte);
begin
    inherited;
    FCommand := 255;
    FLineNum := 1;
    FBCW := TIPCBCW.Create(PRINTER_BCW0);
    FPrintFile := TFileStream.Create(DataDir + '\U0773.prn', fmCreate);
end;

destructor T0773.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FBCW);
    FreeAndNil(FPrintFile);
    inherited;
end;

procedure T0773.DeviceEnd;
begin
    FChannel.QueueStatus(MakeStatus(DEVICE_END, 0));
end;

procedure T0773.DoAdvance;
var
    i, j: Integer;
    skip, codeFound: Boolean;
    num: Byte;
    crlf, cr, ff: AnsiString;
begin
    ClearSense;
    // Check LCB & VFB Status
    if (not CheckInit) then
        Exit;

    FOverflow := False;
    cr := AnsiString(#13);
    crlf := AnsiString(#13#10);
    ff := AnsiString(#12);
    skip := (FCommand and $80) <> 0;
    num := (FCommand and $78) shr 3;
    if (skip) then
    begin
        num := num and $07;
        if (num = 7) then
        begin
            FPrintFile.Write(PAnsiChar(ff)^, Length(ff));
            FLineNum := 1;
        end else
        begin
            codeFound := False;
            i := FLineNum + 1;
            repeat
                if (FVFB[i] = num) then
                begin
                    codeFound := True;
                    Break;
                end;
                Inc(i);
                if (i > High(FVFB)) then
                    i := 1;
            until (i = (FLineNum + 1));
            if (not codeFound) then
            begin
                // skip code not found
                FSense[1] := PRN_VFB_CHECK;
                FChannel.QueueStatus(MakeStatus(DEVICE_END or UNIT_CHECK, 0));
                Exit;
            end;
            if (i <= FLineNum) then
            begin
                FPrintFile.Write(PAnsiChar(ff)^, Length(ff));
                for j := 1 to i - 1 do
                    FPrintFile.Write(PAnsiChar(crlf)^, Length(crlf));
            end else
            begin
                for j := FLineNum to i do
                    FPrintFile.Write(PAnsiChar(crlf)^, Length(crlf));
            end;
            FLineNum := i;
        end;
    end else
    begin
        if (num = 0) then
        begin
            FPrintFile.Write(PAnsiChar(cr)^, Length(cr));
        end else
        begin
            while (num > 0) do
            begin
                FPrintFile.Write(PAnsiChar(crlf)^, Length(crlf));
                Inc(FLineNum);
                if (FLineNum > High(FVFB)) then
                    FLineNum := 1
                else if (FVFB[FLineNum] = 1) then
                    FOverflow := True
                else if (FVFB[FLineNum] = 7) then
                    FLineNum := 1;
                Dec(num);
            end;
        end;
    end;
    DeviceEnd;
end;

procedure T0773.DoDiagnose;
begin
    ClearSense;
    DeviceEnd;
end;

procedure T0773.DoLoadLcb;
var
    i: Integer;
begin
    FLcbLoaded := True;
    ClearSense;
    for i := Low(FLCB) to High(FLCB) do
        FLCB[i] := 0;
    i := 0;
    while ((i <= High(FLCB)) and (FBCW.ActvCount <> 0)) do
    begin
        FLCB[i] := Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
        Inc(i);
        FBCW.ActvAddress := FBCW.ActvAddress + 1;
        FBCW.ActvCount := FBCW.ActvCount - 1;
    end;
    DeviceEnd;
end;

procedure T0773.DoLoadPrintBfr;
begin
    ClearSense;
    // Post form overflow status from last command
    if (FOverflow) then
    begin
        Overflow;
        FOverflow := False;
        Exit;
    end;

    if (FetchBuffer(FPrintBfr)) then
        DeviceEnd;
end;

procedure T0773.DoLoadVfb;
var
    i: Integer;
begin
    ClearSense;
    FVfbLoaded := True;
    ClearSense;
    for i := Low(FVFB) to High(FVFB) do
        FVFB[i] := 0;
    i := 0;
    while ((i <= High(FVFB)) and (FBCW.ActvCount <> 0)) do
    begin
        FVFB[i] := Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
        Inc(i);
        FBCW.ActvAddress := FBCW.ActvAddress + 1;
        FBCW.ActvCount := FBCW.ActvCount - 1;
    end;
    DeviceEnd;
end;

procedure T0773.DoPrint;
begin
    ClearSense;
    // Check LCB & VFB Status
    if (not CheckInit) then
        Exit;
    // Post form overflow status from last command
    if (FOverflow) then
    begin
        Overflow;
        FOverflow := False;
        Exit;
    end;

    if (FetchBuffer(FPrintBfr)) then
    begin
        FPrintFile.Write(PAnsiChar(FPrintBfr)^, Length(FPrintBfr));
        DoAdvance;
    end;
end;

procedure T0773.DoReadLcb;
begin
    ClearSense;
    // Post form overflow status from last command
    if (FOverflow) then
    begin
        Overflow;
        FOverflow := False;
        Exit;
    end;

    if (StoreBuffer(PByte(@FLCB), Length(FLCB))) then
        DeviceEnd;
end;

procedure T0773.DoReadPrintBfr;
begin
    ClearSense;
    // Post form overflow status from last command
    if (FOverflow) then
    begin
        Overflow;
        FOverflow := False;
        Exit;
    end;

    if (StoreBuffer(PByte(PAnsiChar(FPrintBfr)), Length(FPrintBfr))) then
        DeviceEnd;
end;

procedure T0773.DoReadVfb;
begin
    ClearSense;
    // Post form overflow status from last command
    if (FOverflow) then
    begin
        Overflow;
        FOverflow := False;
        Exit;
    end;

    if (StoreBuffer(PByte(@FVFB), Length(FVFB))) then
        DeviceEnd;
end;

procedure T0773.DoSense;
begin
    if (StoreBuffer(PByte(@FSense), 2)) then
        DeviceEnd;
end;

function T0773.FetchBuffer(var bfr: AnsiString): Boolean;
var
    i: Integer;
    c: AnsiChar;
begin
    Result := True;

    if (FBCW.ActvChain) then
        raise Exception.Create('Data chaining not supported');
    if (FBCW.ActvCount = 0) then
        FBCW.ActvCount := 1024;

    try
        bfr := '';
        i := 0;
        while ((i < 144) and (FBCW.ActvCount <> 0) and (not FBCW.ActvTerm)) do
        begin
            c := TCodeTranslator.EbcdicToAscii(Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress));
            if ((c < ' ') or (c > '~')) then
                c := ' ';
            bfr := bfr + c;
            Inc(i);
            FBCW.ActvCount := FBCW.ActvCount - 1;
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
        end;
    except
        Result := False;
        FChannel.QueueStatus(MakeStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS));
    end;
end;

procedure T0773.Overflow;
begin
    FChannel.QueueStatus(MakeStatus(DEVICE_END or UNIT_EXCEPTION, 0));
end;

procedure T0773.ProcessCommand;
begin
    if (FCommand <> 0) then
    begin
        if (FCommand = PRN_LOAD_VFB) then
        begin
            DoLoadVfb;
        end else if (FCommand = PRN_LOAD_LCB) then
        begin
            DoLoadLcb;
        end else if (FCommand = PRN_LOAD_PRINT_BFR) then
        begin
            DoLoadPrintBfr;
        end else if ((FCommand and $1f) = PRN_READ_PRINT_BFR) then
        begin
            DoReadPrintBfr;
        end else if ((FCommand and $1f) = PRN_READ_LCB) then
        begin
            DoReadLcb;
        end else if ((FCommand and $1f) = PRN_READ_VFB) then
        begin
            DoReadVfb;
        end else
        begin
            case FCommand and $07 of
              PRN_SENSE:    DoSense;
              PRN_PRINT:    DoPrint;
              PRN_ADVANCE:  DoAdvance;
              PRN_DIAGNOSE: DoDiagnose;
              else          raise Exception.Create('0773 command not implemented');
            end;
        end;
        FCommand := 0;
    end;
    FBusy := False;
end;

procedure T0773.SIO;
begin
    if (FBusy) then
        // Since this is checked at the channel level,
        // this should never happer.
        Exit;
    FBusy := True;
    FCommand := FBCW.Command;
    FCmdRecvd.SetEvent;
end;

function T0773.StoreBuffer(bfr: PByte; len: Integer): Boolean;
begin
    Result := True;
    while ((len > 0) and (FBCW.ActvCount <> 0)) do
    begin
        try
            Core.StoreByte(FBCW.ActvKey, FBCW.ActvAddress, bfr^);
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
            FBCW.ActvCount := FBCW.ActvCount - 1;
            Inc(bfr);
            Dec(len);
        except
            Result := False;
            FChannel.QueueStatus(MakeStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS));
        end;
    end;
end;

end.
