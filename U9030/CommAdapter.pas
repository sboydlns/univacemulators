unit CommAdapter;

interface

uses SysUtils, Classes, Generics.Collections, SyncObjs, Dialogs,
     IdBaseComponent, IdComponent, IdCustomTCPServer, IdTelnetServer, IdContext, IdGlobal, IdTelnet,
     Globals, IPC;

type
  TTerminal = class
  public
    Context: TIdContext;
  end;

  TTerminalList = class(TList<TTerminal>)
  public
    function IndexOf(context: TIdContext): Integer; overload;
  end;

  TCommAdapter = class(TIPCDevice)
  private
  protected
    FSense: array [0..1] of Byte;
    FBCW: TIPCBCW;
    FControlBytes: array [1..4] of Byte;
    FActive: Boolean;
    FTelnet: TIdTelnetServer;
    FTerminals: TTerminalList;
    procedure ClearSense;
    procedure DoClear;
    procedure DoLoadCharacterDetect(table: Integer);
    procedure DoLoadCharacterInterpret(table: Integer);
    procedure DoLoadControlBytes(start: Integer);
    procedure DoNop;
    procedure DoRead; virtual;
    procedure DoSense;
    procedure DoTurnOff;
    procedure DoWrite; virtual;
    procedure DoWriteTurnaround; virtual;
    function NewTerminal: TTerminal; virtual;
    procedure ProcessCommand; override;
    function StoreBuffer(bfr: PByte; len: Integer): Boolean;
    procedure TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
                                   var AAuthenticated: Boolean);
    procedure TelnetConnect(AContext: TIdContext); virtual;
    procedure TelnetDisconnect(AContext: TIdContext);
    procedure TelnetExecute(AContext: TIdContext);
    procedure TelnetListenException(AThread: TIdListenerThread; AException: Exception);
  public
    constructor Create(num: Byte); override;
    destructor Destroy; override;
    procedure SIO; override;
  end;

implementation

uses EmulatorTypes, Memory, U9030Types, Channels;

var
    i, j: Integer;
    CharacterDetect: array [1..4, 0..255] of Byte;
    CharDetectLock: TCriticalSection;
    CharacterInterpret: array [1..4, 0..15] of UInt16;
    CharInterpretLock: TCriticalSection;

{ TCommAdapter }

procedure TCommAdapter.ClearSense;
begin
    FillChar(FSense, SizeOf(FSense), 0);
end;

constructor TCommAdapter.Create(num: Byte);
// This only supports comm. adapter zero. BCW address calculation needs
// to be updated to support comm. adapter 1.
begin
    inherited;
    FCommand := 0;
    FBCW := TIPCBCW.Create(CA0_BCW0 + ((FDeviceNum - 4) * 16));
    FTelnet := TIdTelnetServer.Create(nil);
    FTelnet.LoginAttempts := 0;
    FTelnet.LoginMessage := 'Sperry*Univac 90/30 Line Adapter';
    FTelnet.MaxConnections := 10;
    FTelnet.DefaultPort := 9030 + FDeviceNum;
    FTelnet.OnAuthentication := TelnetAuthentication;
    FTelnet.OnConnect := TelnetConnect;
    FTelnet.OnDisconnect := TelnetDisconnect;
    FTelnet.OnExecute := TelnetExecute;
    FTelnet.OnListenException := TelnetListenException;
    FTelnet.Active := True;
    FTerminals := TTerminalList.Create;
end;

destructor TCommAdapter.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FBCW);
    FreeAndNil(FTelnet);
    FreeAndNil(FTerminals);
    inherited;
end;

procedure TCommAdapter.DoClear;
//var
//    term: TTerminal;
begin
    ClearSense;
    FActive := False;
//    for term in FTerminals do
//        term.Context.Connection.Disconnect;
    QueueStatus(DEVICE_END, 0);
end;

procedure TCommAdapter.DoLoadCharacterDetect(table: Integer);
var
    i: Integer;
begin
    ClearSense;
    CharDetectLock.Acquire;
    try
        i := 0;
        while ((i <= 255) and (FBCW.ActvCount > 0)) do
        begin
            CharacterDetect[table, i] := Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
            FBCW.ActvCount := FBCW.ActvCount - 1;
            Inc(i);
        end;
    finally
        CharDetectLock.Release;
    end;
    QueueStatus(DEVICE_END, 0);
end;

procedure TCommAdapter.DoLoadCharacterInterpret(table: Integer);
var
    i, count: Integer;
    val: UInt16;
begin
    ClearSense;
    CharInterpretLock.Acquire;
    try
        i := 0;
        count := 0;
        val := 0;
        while ((i <= 15) and (FBCW.ActvCount > 0)) do
        begin
            val := (val shl 8) or Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
            Inc(count);
            if ((count mod 2) = 0) then
            begin
                CharacterInterpret[table, i] := val;
                val := 0;
                Inc(i);
            end;
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
            FBCW.ActvCount := FBCW.ActvCount - 1;
        end;
    finally
        CharInterpretLock.Release;
    end;
    QueueStatus(DEVICE_END, 0);
end;

procedure TCommAdapter.DoLoadControlBytes(start: Integer);
var
    i: Integer;
begin
    ClearSense;
    i := start;
    while ((i <= 4) and (FBCW.ActvCount > 0)) do
    begin
        FControlBytes[i] := Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
        FBCW.ActvCount := FBCW.ActvCount - 1;
        FBCW.ActvAddress := FBCW.ActvAddress + 1;
        Inc(i);
    end;
    if (FBCW.ActvCount > 0) then
        DoSense
    else
        QueueStatus(DEVICE_END, 0);
end;

procedure TCommAdapter.DoNop;
// No-op. Do nothing. Do not even return status.
begin
end;

procedure TCommAdapter.DoRead;
begin
    // This is overridden in child classes
end;

procedure TCommAdapter.DoSense;
begin
{ TODO : implement this }
end;

procedure TCommAdapter.DoTurnOff;
begin
    ClearSense;
    { TODO : we need to something here to cancel terminal output commands 01, 11, 09, 06, 13, and 1f }
    QueueStatus(DEVICE_END, 0);
end;

procedure TCommAdapter.DoWrite;
begin
{ TODO : Need to cancel following commands 11, OD, 09, 02 (half duplex), O6, 03, OF, 13, and 1F. }
    // Overridden by protocol specific line adapter classes
end;

procedure TCommAdapter.DoWriteTurnaround;
// Do a write and then turn the port around for input. Not sure that turning the port
// around requires anything to be done here.
begin
    DoWrite;
end;

function TCommAdapter.NewTerminal: TTerminal;
begin
    Result := TTerminal.Create;
end;

procedure TCommAdapter.ProcessCommand;
begin
    if (FCommand <> 0) then
    begin
        case FCommand of
          $00, $40, $80, $c0,
          $4e, $8e, $ce, $12,
          $52, $92, $d2:
          begin
            DoNop;
          end;
          $01, $41:
          begin
            DoWrite;
          end;
          $02:
          begin
            DoRead;
          end;
          $03:
          begin
            DoTurnOff;
          end;
          $0f:
          begin
            DoClear;
          end;
          $15, $55, $95, $d5:
          begin
            DoLoadControlBytes(((FCommand and $c0) shr 6) + 1);
          end;
          $19, $59, $99, $d9:
          begin
            DoLoadCharacterDetect(((FCommand and $c0) shr 6) + 1);
          end;
          $1d, $5d, $9d, $dd:
          begin
            DoLoadCharacterInterpret(((FCommand and $c0) shr 6) + 1);
          end;
          $81, $c1:
          begin
            DoWriteTurnaround;
          end;
          else
            raise Exception.Create('Not implemented');
        end;
    end;
end;

procedure TCommAdapter.SIO;
begin
    if (FBusy) then
        // Since this is checked at the channel level,
        // this should never happen.
        raise Exception.Create('Comm adapter busy');
//        Exit;
    FBusy := True;
    FCommand := FBCW.Command;
    FCmdRecvd.SetEvent;
end;

function TCommAdapter.StoreBuffer(bfr: PByte; len: Integer): Boolean;
var
    count: Integer;
    addr: TMemoryAddress;
begin
    Result := True;
    count := FBCW.ActvCount;
    addr := FBCW.ActvAddress;
    if (count = 0) then
        count := 1024;
    while ((len > 0) and (count <> 0)) do
    begin
        try
            Core.StoreByte(FBCW.ActvKey, addr, bfr^);
            Inc(addr);
            Dec(count);
            Inc(bfr);
            Dec(len);
        except
            Result := False;
            QueueStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS);
        end;
    end;
    FBCW.ActvCount := count;
    FBCW.ActvAddress := addr;
    FBCW.ActvTerm := True;
    { TODO : need to do something here to support data chaining }
end;

procedure TCommAdapter.TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
  var AAuthenticated: Boolean);
begin
    AAuthenticated := True;
end;

procedure TCommAdapter.TelnetConnect(AContext: TIdContext);
var
    term: TTerminal;
begin
    term := NewTerminal;
    if (not Assigned(term)) then
    begin
        AContext.Connection.Disconnect;
        Exit;
    end;
    term.Context := AContext;
    FTerminals.Add(term);
    AContext.Connection.IOHandler.ReadTimeout := 100;
    // Send a fake Telnet command to make the client think it is talking to
    // a Telnet server
    AContext.Connection.IOHandler.Write(TNC_IAC);
    AContext.Connection.IOHandler.Write(Ord(' '));
end;

procedure TCommAdapter.TelnetDisconnect(AContext: TIdContext);
var
    i: Integer;
begin
    i := FTerminals.IndexOf(AContext);
    if (i <> -1) then
    begin
        FTerminals[i].Free;
        FTerminals.Delete(i);
    end;
end;

procedure TCommAdapter.TelnetExecute(AContext: TIdContext);
begin

end;

procedure TCommAdapter.TelnetListenException(AThread: TIdListenerThread; AException: Exception);
begin
    raise Exception.Create(AException.Message);
end;

{ TTerminalList }

function TTerminalList.IndexOf(context: TIdContext): Integer;
var
    i: Integer;
begin
    Result := -1;
    for i := 0 to Count - 1 do
    begin
        if (Items[i].Context = context) then
        begin
            Result := i;
            Break;
        end;
    end;
end;

initialization
    CharDetectLock := TCriticalSection.Create;
    CharInterpretLock := TCriticalSection.Create;
    for i := 1 to 4 do
    begin
        for j := 0 to 255 do
            CharacterDetect[i, j] := 0;
        for j := 0 to 15 do
            CharacterInterpret[i, j] := 0;
    end;
end.
