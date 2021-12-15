unit UniscopeAdapter;

interface

uses SysUtils, Classes, SyncObjs, IdContext, IdTelnet,
     CommAdapter;

type
  TMessageTypes = ( mtTrafficPoll, mtAck, mtStatusPoll,
                    mtText, mtRetransmitReq, mtMsgWait );
  TMessageType = set of TMessageTypes;

  TUniscopeTerminal = class(TTerminal)
  private
    FBufferLock: TCriticalSection;
  public
    Rid: Byte;
    Sid: Byte;
    LastPoll: TMessageType;
    DataPending: Boolean;
    AckPending: Boolean;
    Buffer: AnsiString;
    constructor Create;
    destructor Destroy; override;
    procedure LockBuffer;
    procedure UnlockBuffer;
  end;

  // A class to simulate an 8609 terminal multiplexor. Terminal sessions connect
  // using Telnet. Terminal addresses are assigned from the 31 SIDs ($51 thru $6f)
  // available for the line adapters RID. Multi-drop lines are not supported nor should
  // they be necessary.
  TUniscopeAdapter = class(TCommAdapter)
  private
    FRid: Byte;
    FStxSeen: Boolean;
  protected
    procedure DoRead; override;
    procedure DoWrite; override;
    function NewSid: Byte;
    function NewTerminal: TTerminal; override;
    procedure TelnetConnect(AContext: TIdContext); override;
    procedure TelnetExecute(AContext: TIdContext); override;
  public
    constructor Create(num, rid: Byte); reintroduce;
  end;

implementation

uses IdGlobal, EmulatorTypes, Globals, U9030Types, Channels, Trace;

{ TUniscopeAdapter }

constructor TUniscopeAdapter.Create(num, rid: Byte);
begin
    inherited Create(num);
    FRid := rid;
end;

procedure TUniscopeAdapter.DoRead;
// Loop through the currently connected terminals looking for pending input
var
    i, bytesStored, actvCount: Integer;
    term: TUniscopeTerminal;
    bfr: array of Byte;
    tbfr: AnsiString;
    c: Byte;
    ackSent: Boolean;
    statSent: Boolean;
    textSent: Boolean;

    procedure AppendAck;
    begin
        bfr[2] := term.Sid;
        SetLength(bfr, Length(bfr) + 2);
        bfr[High(bfr) - 1] := DLE;
        bfr[High(bfr)] := Ord('1');
    end;

    procedure AppendTraffic;
    begin
        bfr[2] := term.Sid;
        SetLength(bfr, Length(bfr) + 2);
        bfr[High(bfr) - 1] := DLE;
        bfr[High(bfr)] := Ord('0');
    end;

    procedure AppendText;
    var
        j, k: Integer;
    begin
        bfr[2] := term.Sid;
        j := High(bfr);
        SetLength(bfr, Length(bfr) + Length(term.Buffer));
        for k := 1 to Length(term.Buffer) do
            bfr[j + k] := Byte(term.Buffer[k]);
    end;

begin
    // Initialize buffer to hold any input that may be found
    SetLength(bfr, 4);
    bfr[0] := SOH;
    bfr[1] := FRid;
    bfr[2] := $50;
    bfr[3] := $70;
    // Append any outstanding acknowledgement
    ackSent := False;
    for i := 0 to FTerminals.Count - 1 do
    begin
        term := TUniscopeTerminal(FTerminals[i]);
        if (([mtTrafficPoll, mtStatusPoll] * term.LastPoll) <> []) then
        begin
            if ((not ackSent) and (term.AckPending)) then
            begin
                AppendAck;
                ackSent := True;
                term.AckPending := False;
                term.LastPoll := [];
            end;
        end;
    end;
    // Append any outstanding status replies
    statSent := False;
    for i := 0 to FTerminals.Count - 1 do
    begin
        term := TUniscopeTerminal(FTerminals[i]);
        if (([mtStatusPoll] * term.LastPoll) <> []) then
        begin
            if ((not statSent) and term.DataPending) then
            begin
                AppendTraffic;
                statSent := True;
                term.LastPoll := [];
            end;
        end;
    end;
    // Append any data waiting to be sent
    if (not statSent) then
    begin
        textSent := False;
        for i := 0 to FTerminals.Count - 1 do
        begin
            term := TUniscopeTerminal(FTerminals[i]);
            if (([mtTrafficPoll] * term.LastPoll) <> []) then
            begin
                term.LockBuffer;
                try
                    if ((not textSent) and term.DataPending) then
                    begin
                        AppendText;
                        textSent := True;
                        term.DataPending := False;
                        term.Buffer := '';
                        term.LastPoll := [];
                    end;
                finally
                    term.UnlockBuffer;
                end;
            end;
        end;
    end;
    //
    if (Length(bfr) = 4) then
    begin
        // No traffic
        SetLength(bfr, 2);
        bfr[0] := EOT;
        bfr[1] := EOT;
    end else
    begin
        // There was traffic, terminate the buffer
        //
        // Originally, I was appending ETX to the buffer here. It turns
        // out that ETX is suppressed by the character interpretation table
        // so we don't want the terminating ETX after all.
        //        SetLength(bfr, Length(bfr) + 1);
        //        bfr[High(bfr)] := ETX;
    end;
    //
    if (IOTraceEnabled) then
    begin
        tbfr := AnsiString(Format('  Rid = %2.2x Sid = %2.2x Did = %2.2x'#13#10, [bfr[1], bfr[2], bfr[3]]));
        IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        tbfr := '  ';
        for c in bfr do
            tbfr := tbfr + AnsiString(Format('%2.2x', [c]));
        tbfr := tbfr + #13#10;
        IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
    end;
    bytesStored := 0;
    while (bytesStored < Length(bfr)) do
    begin
        actvCount := FBCW.ActvCount;
        if (actvCount = 0) then
            actvCount := 1024;
        if (StoreBuffer(@bfr[bytesStored], Length(bfr) - bytesStored)) then
        begin
            Inc(bytesStored, actvCount);
            if ((FBCW.ActvCount = 0) and FBCW.ActvChain) then
            begin
                Sleep(10);                      // give the CPU a chance to catch up
                if (FBCW.F) then
                begin
                    FBCW.ActvAddress := FBCW.ReplAddress;
                    FBCW.ActvChain := FBCW.ReplChain;
                    FBCW.ActvCount := FBCW.ReplCount;
                    FBCW.ActvKey := FBCW.ReplKey;
                    FBCW.F := False;
                    if (FBCW.ReplChain) then
                    begin
                        QueueStatus(0, BUFFER_TERMINATE);
                        Sleep(10);              // short pause to allow CPU to process status
                    end;
                end else
                begin
                    FBCW.ActvChain := False;
                    FBCW.ReplChain := False;
                    bytesStored := Length(bfr);
                end;
            end else
            begin
                bytesStored := Length(bfr);
            end;
        end;
    end;
    Sleep(10);
    QueueStatus(DEVICE_END, 0);
end;

procedure TUniscopeAdapter.DoWrite;
var
    b, rid, sid, did: Byte;
    bfr, tbfr: AnsiString;
    c: AnsiChar;
    gotSOH, gotSTX, gotDLE: Boolean;
    i, len: Integer;
    term: TUniscopeTerminal;
    mtype: TMessageType;
    done: Boolean;
//    ms: Integer;
begin
    bfr := '';
    gotSOH := False;
    gotSTX := False;
    gotDLE := False;
    rid := 0;
    sid := 0;
    did := 0;
    mtype := [];

    done := False;
    while (not done) do
    begin
        len := FBCW.ActvCount;
        if (len = 0) then
            len := 1024;
        while (len > 0) do
        begin
            try
                b := Core.FetchByte(FBCW.ActvKey, FBCW.ActvAddress);
            except
                b := 0;
                QueueStatus(DEVICE_END or UNIT_CHECK, INVALID_ADDRESS);
                Exit;
            end;
            if (gotSOH) then
            begin
                // Parse the message header to see what kind of message we have
                if (rid = 0) then
                    rid := b
                else if (sid = 0) then
                    sid := b
                else if (did = 0) then
                    did := b
                else if (b = STX) then
                begin
                    gotSTX := True;
                    mtype := [mtText];
                end else if (not gotSTX) then
                begin
                    case b of
                     ETX:
                     begin
                        mtype := mtype + [mtTrafficPoll];
                     end;
                     BEL:
                     begin
                        mtype := mtype + [mtMsgWait];
                     end;
                     ENQ:
                     begin
                       mtype := mtype + [mtStatusPoll];
                     end;
                     DLE:
                     begin
                       gotDLE := True;
                     end;
                     NAK:
                     begin
                       if (gotDLE) then
                           mtype := mtype + [mtRetransmitReq];
                       gotDLE := False;
                     end;
                     Ord('1'):
                     begin
                        if (gotDLE) then
                            mtype := mtype + [mtAck];
                        gotDLE := False;
                     end;
                    end;
                end;
            end else
            begin
                if (b = SOH) then
                    gotSOH := True;
            end;
            bfr := bfr + AnsiChar(b);
            FBCW.ActvAddress := FBCW.ActvAddress + 1;
            Dec(len);
        end;

        FBCW.ActvCount := len;
        FBCW.ActvTerm := True;

        if (FBCW.ActvChain) then
        begin
            Sleep(10);                      // give the CPU a chance to catch up
            if (FBCW.F) then
            begin
                FBCW.ActvAddress := FBCW.ReplAddress;
                FBCW.ActvChain := FBCW.ReplChain;
                FBCW.ActvCount := FBCW.ReplCount;
                FBCW.ActvKey := FBCW.ReplKey;
                FBCW.F := False;
                if (FBCW.ReplChain) then
                begin
                    QueueStatus(0, BUFFER_TERMINATE);
                    Sleep(10);              // short pause to allow CPU to process status
                end;
            end else
            begin
                FBCW.ActvChain := False;
                FBCW.ReplChain := False;
                done := True;
            end;
        end else
        begin
            done := True;
        end;
    end;
    //
    if (IOTraceEnabled) then
    begin
        tbfr := AnsiString(Format('  Rid = %2.2x Sid = %2.2x Did = %2.2x'#13#10, [rid, sid, did]));
        IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        tbfr := '  ';
        for c in bfr do
            tbfr := tbfr + AnsiString(Format('%2.2x', [Ord(c)]));
        tbfr := tbfr + #13#10;
        IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        if (mtText in mtype) then
        begin
            tbfr := '  Text'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
        if (mtMsgWait in mtype) then
        begin
            tbfr := '  Msg Wait'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
        if (mtStatusPoll in mtype) then
        begin
            tbfr := '  Status'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
        if (mtRetransmitReq in mtype) then
        begin
            tbfr := '  NAK'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
        if (mtAck in mtype) then
        begin
            tbfr := '  ACK'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
        if (mtTrafficPoll in mtype) then
        begin
            tbfr := '  Poll'#13#10;
            IOTraceFile.Write(PAnsiChar(tbfr)^, Length(tbfr));
        end;
    end;
    // Either send the message to the appropriate terminal(s) or set the state as required
    for i := 0 to FTerminals.Count - 1 do
    begin
        term := TUniscopeTerminal(FTerminals[i]);
        if (((rid = $20) or (rid = term.Rid)) and
            ((sid = $50) or (sid = term.Sid))) then
        begin
            if (([mtText, mtMsgWait] * mtype) <> []) then
            begin
                term.Context.Connection.IOHandler.Write(TIdBytes(bfr));
                term.AckPending := True;
                term.LockBuffer;
                try
                    term.DataPending := False;
                    term.Buffer := '';
                finally
                    term.UnlockBuffer;
                end;
            end;
            term.LastPoll := mtype;
        end;
    end;
    // testing
//    ms := Round((Length(bfr) / (19200 / 8)) * 1000);
//    Sleep(ms);
    // testing
    //
    Sleep(10);
    QueueStatus(DEVICE_END, 0);
end;

function TUniscopeAdapter.NewSid: Byte;
// Assign a terminal address from the 31 available by scanning the
// list of connections to see if a particular address has been used.
var
    term: TTerminal;
    done, found: Boolean;
begin
    Result := $51;
    done := False;
    while (not done) do
    begin
        found := False;
        for term in FTerminals do
        begin
            if (TUniscopeTerminal(term).Sid = Result) then
            begin
                found := True;
                Break;
            end;
        end;
        if (not found) then
        begin
            done := True;
        end else
        begin
            Inc(Result);
            if (Result > $6f) then
            begin
                done := True;
                Result := 0;
            end;
        end;
    end;
end;

function TUniscopeAdapter.NewTerminal: TTerminal;
var
    sid: Byte;
begin
    Result := TUniscopeTerminal.Create;
    TUniscopeTerminal(Result).Rid := FRid;
    sid := NewSid;
    if (sid = 0) then
    begin
        Result.Free;
        Result := nil;
    end else
        TUniscopeTerminal(Result).Sid := sid;
end;

procedure TUniscopeAdapter.TelnetConnect(AContext: TIdContext);
var
    i: Integer;
begin
    inherited;
    // Send the RID and SID prefixed by TNC_IAC so that the terminal emulator
    // will know what its assigned address is.
    i := FTerminals.IndexOf(AContext);
    if (i <> -1) then
    begin
        AContext.Connection.IOHandler.Write(TNC_IAC);
        AContext.Connection.IOHandler.Write(TNC_IAC);
        AContext.Connection.IOHandler.Write(TUniscopeTerminal(FTerminals[i]).Rid);
        AContext.Connection.IOHandler.Write(TUniscopeTerminal(FTerminals[i]).Sid);
    end;
end;

procedure TUniscopeAdapter.TelnetExecute(AContext: TIdContext);
var
    bfr: TIdBytes;
    i: Integer;
    etxSeen: Boolean;
    term: TUniscopeTerminal;
begin
    i := FTerminals.IndexOf(AContext);
    if (i = -1) then
        Exit;
    term := TUniscopeTerminal(FTerminals[i]);

    AContext.Connection.IOHandler.ReadTimeout := 10;
    AContext.Connection.IOHandler.ReadBytes(bfr, -1, False);
    if (Length(bfr) = 0) then
        Exit;


    etxSeen := False;
    i := Low(bfr);
    while ((not FStxSeen) and (i <= High(bfr))) do
    begin
        FStxSeen := ((bfr[i] = STX) or (bfr[i] = BEL));
        if (FStxSeen) then
        begin
            term.LockBuffer;
            try
                term.DataPending := False;
                term.Buffer := AnsiChar(bfr[i]);
            finally
                term.UnlockBuffer;
            end;
        end;
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
                term.Buffer := term.Buffer + AnsiChar(bfr[i]);
                Inc(i);
            end;
        end;
    end;
    if (etxSeen and (Length(term.Buffer) > 0)) then
    begin
        term.LockBuffer;
        try
            term.DataPending := True;
        finally
            term.UnlockBuffer;
        end;
    end;
end;

{ TUniscopeTerminal }

constructor TUniscopeTerminal.Create;
begin
    inherited;
    FBufferLock := TCriticalSection.Create;
end;

destructor TUniscopeTerminal.Destroy;
begin
    FreeAndNil(FBufferLock);
    inherited;
end;

procedure TUniscopeTerminal.LockBuffer;
begin
    FBufferLock.Acquire;
end;

procedure TUniscopeTerminal.UnlockBuffer;
begin
    FBufferLock.Release;
end;

end.
