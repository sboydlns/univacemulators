unit Console;

interface

uses SysUtils, Classes, SyncObjs, Dialogs,
     IdBaseComponent, IdComponent, IdCustomTCPServer, IdTelnetServer, IdContext, IdGlobal, IdTelnet,
     IPC, Channels;

const
  // Console command codes and flags
  CONS_READ = $02;
  CONS_WRITE = $01;
  CONS_SENSE = $00;
  CONS_TRANSLATE = $04;
  CONS_DIAG = $80;
  CONS_LOCK_KBD = $40;
  CONS_PRINT = $20;
  // Console sense byte 1
  CONS_POWER_OFF = $80;
  CONS_COP_NO_RESP = $40;
  CONS_OPR_PRINT = $20;
  CONS_WAIT_ACTIVE = $10;
  CONS_TRANSMIT_ACTIVE = $08;
  CONS_AUX_FEATURE = $04;
  CONS_PRINT_TIMEOUT = $02;

type
  TConsole = class(TIPCDevice)
  private
    FSense: array [0..1] of Byte;
    FBCW: TIPCBCW;
    FTelnet: TIdTelnetServer;
    FConsole: TIdContext;
    FBuffer: String;
    FStxSeen: Boolean;
    FInputBfr: TStringList;
    FInputBfrLock: TCriticalSection;
    procedure ClearSense;
    procedure DeviceEnd;
    procedure DoAttention;
    procedure DoRead;
    procedure DoSense;
    procedure DoWrite;
    procedure NotConnected;
    function StoreBuffer(bfr: PByte; len: Integer): Boolean;
    procedure TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
                                   var AAuthenticated: Boolean);
    procedure TelnetConnect(AContext: TIdContext);
    procedure TelnetDisconnect(AContext: TIdContext);
    procedure TelnetExecute(AContext: TIdContext);
    procedure TelnetListenException(AThread: TIdListenerThread; AException: Exception);
  protected
    procedure DoTimer; override;
    procedure QueueStatus(dstat, cstat: Byte); override;
  public
    constructor Create(num: Byte); override;
    destructor Destroy; override;
    procedure ProcessCommand; override;
    procedure Shutdown;
    procedure SIO; override;
  end;

implementation

uses EmulatorTypes, U9030Types, Globals, Memory;

{ TConsole }

procedure TConsole.ClearSense;
begin
    FillChar(FSense, SizeOf(FSense), 0);
end;

constructor TConsole.Create(num: Byte);
begin
    inherited;
    FCommand := 255;
    FBCW := TIPCBCW.Create(CONS_BCW0);
    FTelnet := TIdTelnetServer.Create(nil);
    FTelnet.LoginAttempts := 0;
    FTelnet.LoginMessage := 'Sperry*Univac 90/30 Console';
    FTelnet.MaxConnections := 1;
    FTelnet.DefaultPort := 9030;
    FTelnet.OnAuthentication := TelnetAuthentication;
    FTelnet.OnConnect := TelnetConnect;
    FTelnet.OnDisconnect := TelnetDisconnect;
    FTelnet.OnExecute := TelnetExecute;
    FTelnet.OnListenException := TelnetListenException;
    FTelnet.Active := True;
    FInputBfr := TStringList.Create;
    FInputBfrLock := TCriticalSection.Create;
end;

destructor TConsole.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FBCW);
    FreeAndNil(FTelnet);
    FreeAndNil(FInputBfr);
    FreeAndNil(FInputBfrLock);
    inherited;
end;

procedure TConsole.DeviceEnd;
begin
    QueueStatus(DEVICE_END, 0);
end;

procedure TConsole.DoAttention;
begin
    SendAttention;
end;

procedure TConsole.DoRead;
var
    s: AnsiString;
    translate: Boolean;
begin
    ClearSense;
    if (not Assigned(FConsole)) then
    begin
        NotConnected;
        Exit;
    end;

    if (FBCW.ActvChain) then
        raise Exception.Create('Data chaining not supported');
    if (FBCW.ActvCount = 0) then
        FBCW.ActvCount := 1024;

    translate := (FCommand and CONS_TRANSLATE) = 0;
    // Wait for any input from the console process
    while ((not Terminated) and (FInputBfr.Count = 0)) do
        Sleep(10);

    FInputBfrLock.Acquire;
    try
        s := AnsiString(FInputBfr[0]);
        FInputBfr.Delete(0);
    finally
        FInputBfrLock.Release;
    end;
    if (translate) then
        s := TCodeTranslator.AsciiToEbcdic(s);
    //
    if (StoreBuffer(PByte(PAnsiString(s)), Length(s))) then
        DeviceEnd;
end;

procedure TConsole.DoSense;
begin
    if (StoreBuffer(PByte(@FSense), 2)) then
        DeviceEnd;
end;

procedure TConsole.DoTimer;
begin
    FInputBfrLock.Acquire;
    try
        if (FInputBfr.Count > 0) then
        begin
            if (FInputBfr[0] = Chr(BEL)) then
            begin
                FInputBfr.Delete(0);
                DoAttention;
            end;
        end;
    finally
        FInputBfrLock.Release;
    end;
end;

procedure TConsole.DoWrite;
var
    addr: TMemoryAddress;
    count: THalfWord;
    s: AnsiString;
    translate: Boolean;
    b: Byte;
begin
    ClearSense;
    if (not Assigned(FConsole)) then
    begin
        NotConnected;
        Exit;
    end;

    if (FBCW.ActvChain) then
        raise Exception.Create('Data chaining not supported');
    if (FBCW.ActvCount = 0) then
        FBCW.ActvCount := 1024;

    translate := (FCommand and CONS_TRANSLATE) = 0;
    addr := FBCW.ActvAddress;
    count := FBCW.ActvCount;
    if (count = 0) then
        count := 1024;
    while (count > 0) do
    begin
        try
            b := Core.FetchByte(FBCW.ActvKey, addr);
        except
            b := 0;
            QueueStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS);
            Exit;
        end;
        if (translate) then
            s := s + TCodeTranslator.EbcdicToAscii(b)
        else
            s := s + AnsiChar(Chr(b));
        Inc(addr);
        Dec(count);
    end;
    FBCW.ActvAddress := addr;
    FBCW.ActvCount := count;
    FBCW.ActvTerm := True;
    if ((FBCW.Command and CONS_LOCK_KBD) = 0) then
        s := s + AnsiChar(DC4);
    s := AnsiChar(STX) + s + AnsiChar(ETX);
    if (Assigned(FConsole)) then
        FConsole.Connection.IOHandler.Write(TIdBytes(s));
    DeviceEnd;
end;

procedure TConsole.NotConnected;
begin
    ClearSense;
    FSense[0] := SENSE_INTERVENTION;
    FSense[1] := CONS_POWER_OFF;
    QueueStatus(UNIT_CHECK, 0);
end;

procedure TConsole.ProcessCommand;
begin
    case FCommand and $3 of
      CONS_READ:    DoRead;
      CONS_WRITE:   DoWrite;
      CONS_SENSE:   DoSense;
      else          raise Exception.Create('Console command not implemented');
    end;
end;

procedure TConsole.QueueStatus(dstat, cstat: Byte);
var
    stat: TStatus;
begin
    stat := MakeStatus(dstat, cstat);
    FBusy := False;
    FCommand := 255;
    FChannel.QueueStatus(stat);
end;

procedure TConsole.Shutdown;
begin
    if (Assigned(FConsole)) then
        FConsole.Connection.IOHandler.Write($fe);
end;

procedure TConsole.SIO;
begin
    if (FBusy) then
        // Since this is checked at the channel level,
        // this should never happer.
        Exit;
    FBusy := True;
    FCommand := FBCW.Command;
    FCmdRecvd.SetEvent;
end;

function TConsole.StoreBuffer(bfr: PByte; len: Integer): Boolean;
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
            QueueStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS);
        end;
    end;
end;

procedure TConsole.TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
  var AAuthenticated: Boolean);
begin
    AAuthenticated := True;
end;

procedure TConsole.TelnetConnect(AContext: TIdContext);
begin
    FConsole := AContext;
    FConsole.Connection.IOHandler.ReadTimeout := 100;
    // Send a fake Telnet command to make the client thinks it is talking to
    // a Telnet server
    FConsole.Connection.IOHandler.Write(TNC_IAC);
    FConsole.Connection.IOHandler.Write(Ord(' '));
end;

procedure TConsole.TelnetDisconnect(AContext: TIdContext);
begin
    if (AContext = FConsole) then
        FConsole := nil;
end;

procedure TConsole.TelnetExecute(AContext: TIdContext);
var
    bfr: TIdBytes;
    i: Integer;
    etxSeen: Boolean;
begin
    AContext.Connection.IOHandler.ReadTimeout := 10;
    AContext.Connection.IOHandler.ReadBytes(bfr, -1, False);
    if (Length(bfr) = 0) then
        Exit;

    etxSeen := False;
    i := Low(bfr);
    while ((not FStxSeen) and (i <= High(bfr))) do
    begin
        FStxSeen := ((bfr[i] = STX) or (bfr[i] = BEL));
        if (bfr[i] = BEL) then
            FBuffer := Chr(BEL);
        Inc(i);
    end;
    if (FStxSeen) then
    begin
        while ((not etxSeen) and (i <= High(bfr))) do
        begin
            if (bfr[i] = ETX) then
            begin
                etxSeen := True;
            end else
            begin
                FBuffer := FBuffer + Char(AnsiChar(bfr[i]));
                Inc(i);
            end;
        end;
    end;
    if (etxSeen and (Length(FBuffer) > 0)) then
    begin
        FInputBfrLock.Acquire;
        try
            FInputBfr.Add(FBuffer);
            FStxSeen := False;
            FBuffer := '';
        finally
            FInputBfrLock.Release;
        end;
    end;
end;

procedure TConsole.TelnetListenException(AThread: TIdListenerThread; AException: Exception);
begin
    raise Exception.Create(AException.Message);
end;

end.
