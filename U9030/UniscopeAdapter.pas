unit UniscopeAdapter;

interface

uses SysUtils, Classes, IdContext, IdTelnet,
     CommAdapter;

type
  TMessageTypes = ( mtTrafficPoll, mtAck, mtStatusPoll,
                    mtText, mtRetransmitReq, mtMsgWait );
  TMessageType = set of TMessageTypes;

  TUniscopeTerminal = class(TTerminal)
  public
    Rid: Byte;
    Sid: Byte;
    AckPending: Boolean;                // Processor is owed an ACK
    LastPoll: TMessageType;
    { TODO :
This is going to need to have state information so that the TUnsicopeAdapter
class can respond to polls without having to actually send the pools to the
terminal. This means that the terminal must transmit text & status data to
TUniscopeAdapter as events occur at the terminal. This data must be cached
here for use in contructing responses to polls. }
  end;

  // A class to emulate an 8609 terminal multiplexor. Terminal sessions connect
  // using Telnet. Terminal addresses are assigned from the 31 SIDs ($51 thru $6f)
  // available for the line adapters RID.
  TUniscopeAdapter = class(TCommAdapter)
  private
    FRid: Byte;
  protected
    procedure DoRead; override;
    procedure DoWrite; override;
    function NewSid: Byte;
    function NewTerminal: TTerminal; override;
    procedure TelnetConnect(AContext: TIdContext); override;
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
    i: Integer;
    term: TUniscopeTerminal;
    bfr: array of Byte;
    tbfr: AnsiString;
    c: Byte;
    ackSent: Boolean;

    procedure AppendAck;
    begin
        bfr[2] := term.Sid;
        SetLength(bfr, Length(bfr) + 2);
        bfr[High(bfr) - 1] := DLE;
        bfr[High(bfr)] := Ord('1');
    end;
begin
    // Initialize buffer to hold any input that may be found
    SetLength(bfr, 4);
    bfr[0] := SOH;
    bfr[1] := FRid;
    bfr[2] := $50;
    bfr[3] := $70;
    //
    ackSent := False;
    for i := 0 to FTerminals.Count - 1 do
    begin
        term := TUniscopeTerminal(FTerminals[i]);
        if (([mtTrafficPoll, mtStatusPoll] * term.LastPoll) <> []) then
        begin
            if ((not ackSent) and term.AckPending) then
            begin
                AppendAck;
                ackSent := True;
                term.AckPending := False;
            end;
        end;
        term.LastPoll := [];
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
    if (StoreBuffer(@bfr[0], Length(bfr))) then
        QueueStatus(DEVICE_END, 0);
end;

procedure TUniscopeAdapter.DoWrite;
var
    b, rid, sid, did: Byte;
    bfr, tbfr: AnsiString;
    c: AnsiChar;
    gotSOH, gotSTX, gotDLE: Boolean;
    i: Integer;
    term: TUniscopeTerminal;
    mtype: TMessageType;
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

    if (FBCW.ActvChain) then
        raise Exception.Create('Data chaining not supported');
    if (FBCW.ActvCount = 0) then
        FBCW.ActvCount := 1024;

    while (FBCW.ActvCount > 0) do
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
        FBCW.ActvCount := FBCW.ActvCount - 1;
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

    FBCW.ActvTerm := True;
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
            end;
            term.LastPoll := mtype;
        end;
    end;
    // testing
//    ms := Round((Length(bfr) / (19200 / 8)) * 1000);
//    Sleep(ms);
    // testing
    //
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

end.
