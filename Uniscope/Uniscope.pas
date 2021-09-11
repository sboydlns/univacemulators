unit Uniscope;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics,
  Vcl.Forms, IdTelnet, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal;

type
  TUniscopeModel = ( umConsole, umU100, umU200 );

  TUniscopeSize = ( us16x64, us12x80, us24x80 );

  TMessageTypes = ( mtTrafficPoll, mtAck, mtStatusPoll,
                    mtText, mtRetransmitReq, mtMsgWait );
  TMessageType = set of TMessageTypes;

  TAttribute = ( tProtected, tTabStop );

  TAttributes = set of TAttribute;

  TUniscope = class(TWinControl)
  private
    FCanvas: TControlCanvas;
    FTimer: TTimer;
    FTelnet: TIdTelnet;
    FCharSize: TSize;
    FCharSizeValid: Boolean;
    FInitialized: Boolean;
    FCursorOn: Boolean;
    FHostControl: Boolean;
    FTraceFile: TFileStream;
    FLockKbd: Boolean;
    FKbdLocked: Boolean;
    FProtected: Boolean;
    FRid: Byte;
    FSid: Byte;
    function GetCanvas: TCanvas;
    procedure SetBackColour(const Value: TColor);
    procedure SetTextColour(const Value: TColor);
    function GetBackColour: TColor;
    function GetTextColour: TColor;
    function GetFont: TFont;
    procedure SetSize(const Value: TUniscopeSize);
    function GetDisplaySize: TSize;
    function GetCharSize: TSize;
    procedure SetTelnet(const Value: TIdTelnet);
  protected
    FRow: Integer;
    FCol: Integer;
    FMaxRow: Integer;
    FMaxCol: Integer;
    FCharacters: array of Char;
    FAttributes: array of TAttributes;
    FModel: TUniscopeModel;
    FSize: TUniscopeSize;
    FPrntrFile: TFileStream;
    procedure BackSpace;
    procedure ClearBuffer;
    procedure CursorHome;
    procedure CursorPosition(row, col: Byte);
    procedure DecCursor;
    procedure DeleteLine;
    procedure DoAcknowledge;
    procedure DoBell;
    procedure DoRetransmit;
    procedure DoStatus;
    procedure DoTimer(Sender: TObject);
    procedure DoTrafficPoll;
    procedure Down;
    procedure DrawStatus;
    procedure EndProtected;
    procedure EraseDisplay(prot: Boolean);
    procedure FindSOE(var row, col: Integer);
    function GetCharacter: Char;
    procedure HideCursor;
    procedure Home;
    procedure IncCursor;
    function IsProtected(row, col: Integer): Boolean;
    procedure Left;
    procedure MsgWait;
    procedure OpenPrntrFile;
    procedure Print;
    procedure PrintTransparent;
    procedure Refresh;
    procedure Right;
    procedure SetArrayLengths;
    procedure SetCharacter(c: Char);
    procedure SkipProtectBackward;
    procedure SkipProtectForward;
    procedure StartProtected;
    procedure TelnetConnected(Sender: TObject);
    procedure TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
    procedure TelnetDisconnected(Sender: TObject);
    procedure Transmit;
    procedure Up;
  public
    constructor Create(AOwner: TComponent; model: TUniscopeModel; size: TUniscopeSize); reintroduce;
    destructor Destroy; override;
    procedure Clear;
    property Canvas: TCanvas read GetCanvas;
    procedure CharOut(c: Byte; incr: Boolean = True); overload;
    procedure CharOut(c: Char; incr: Boolean = True); overload;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState); reintroduce;
    procedure Repaint; override;
    property CharSize: TSize read GetCharSize;
    property ColCount: Integer read FMaxCol;
    property DisplaySize: TSize read GetDisplaySize;
    property RowCount: Integer read FMaxRow;
  published
    property BackColour: TColor read GetBackColour write SetBackColour;
    property Font: TFont read GetFont;
    property Model: TUniscopeModel read FModel write FModel;
    property Size: TUniscopeSize read FSize write SetSize;
    property Telnet: TIdTelnet read FTelnet write SetTelnet;
    property TextColour: TColor read GetTextColour write SetTextColour;
  end;

procedure Register;

implementation

uses Dialogs, EmulatorTypes;

procedure Register;
begin
  RegisterComponents('LNS Conrols', [TUniscope]);
end;

const
  SOE_CHAR = '►';
  TAB_CHAR = '·';
  START_BLINK_CHAR = '«';
  END_BLINK_CHAR = '»';
  CURSOR_CHAR = '█';

{ TUniscope }

procedure TUniscope.BackSpace;
begin
    HideCursor;
    DecCursor;
    CharOut(' ', False);
    DrawStatus;
end;

procedure TUniscope.CharOut(c: Byte; incr: Boolean);
begin
    CharOut(Char(AnsiChar(c)), incr);
end;

procedure TUniscope.CharOut(c: Char; incr: Boolean);
begin
    FCanvas.TextOut(FCol * FCharSize.cx, FRow * FCharSize.cy, c);
    SetCharacter(c);
    if (incr) then
        IncCursor;
    DrawStatus;
end;

procedure TUniscope.Clear;
begin
    ClearBuffer;
    Refresh;
end;

procedure TUniscope.ClearBuffer;
var
    i: Integer;
begin
    for i := Low(FCharacters) to High(FCharacters) do
    begin
        FCharacters[i] := ' ';
        FAttributes[i] := [];
    end;
end;

constructor TUniscope.Create(AOwner: TComponent; model: TUniscopeModel; size: TUniscopeSize);
var
    i: Integer;
begin
    inherited Create(AOwner);
    Self.Model := model;
    Self.Size := size;
    if (model = umConsole) then
        FTraceFile := TFileStream.Create('c:\temp\uniscope.trc', fmCreate);
    FCanvas := TControlCanvas.Create;
    FCanvas.Control := Self;
    FTimer := TTimer.Create(AOwner);
    FTimer.Interval := 1;
    FTimer.OnTimer := DoTimer;
    FTimer.Enabled := True;
    SetArrayLengths;
    for i := Low(FCharacters) to High(FCharacters) do
        FCharacters[i] := ' ';
end;

procedure TUniscope.CursorHome;
begin
    FRow := 0;
    FCol := 0;
    DrawStatus;
end;

procedure TUniscope.CursorPosition(row, col: Byte);
begin
    FRow := row;
    FCol := col;
    DrawStatus;
end;

procedure TUniscope.DecCursor;
begin
    Dec(FCol);
    if (FCol < 0) then
    begin
        FCol := FMaxCol - 1;
        Dec(FRow);
        if (FRow < 0) then
            FRow := FMaxRow - 1;
    end;
    SkipProtectBackward;
end;


procedure TUniscope.DeleteLine;
var
    dest, src, count: Integer;
begin
    if (FRow < (FMaxRow - 1)) then
    begin
        dest := FRow * FMaxCol;
        src := (FRow + 1) * FMaxCol;
        for count := 1 to (FMaxRow * FMaxCol) - src do
        begin
            FCharacters[dest] := FCharacters[src];
            FAttributes[dest] := FAttributes[src];
            Inc(dest);
            Inc(src);
        end;
    end;
    dest := (FMaxRow - 1) * FMaxCol;
    for count := 1 to FMaxCol do
    begin
        FCharacters[dest] := ' ';
        FAttributes[dest] := [];
        Inc(dest);
    end;
    Refresh;
end;

destructor TUniscope.Destroy;
begin
    FreeAndNil(FTraceFile);
    FreeAndNil(FPrntrFile);
    FreeAndNil(FTimer);
    FreeAndNil(FCanvas);
    inherited;
end;

procedure TUniscope.DoAcknowledge;
begin

end;

procedure TUniscope.DoBell;
begin

end;

procedure TUniscope.DoRetransmit;
begin

end;

procedure TUniscope.DoStatus;
begin

end;

procedure TUniscope.DoTimer(Sender: TObject);
var
    c: Char;
    row, col, i: Integer;
begin
    if (not FInitialized) then
    begin
        // Do some initialization the first time the timer fires and then
        // set the interval to 0.5 seconds for cursor and blink character blinking.
        Clear;
        FTimer.Interval := 500;
        FInitialized := True;
    end;
    if (FCursorOn) then
    begin
        FCursorOn := False;
        FCanvas.TextOut(FCol * FCharSize.cx, FRow * FCharSize.cy, FCharacters[FRow * FMaxCol + FCol]);
        i := Low(FCharacters);
        for row := 0 to FMaxRow - 1 do
            for col := 0 to FMaxCol - 1 do
        begin
            c := FCharacters[i];
            if ((c = START_BLINK_CHAR) or (c = END_BLINK_CHAR)) then
                FCanvas.TextOut(col * FCharSize.cx, row * FCharSize.cy, ' ');
            Inc(i);
        end;
    end else
    begin
        FCursorOn := True;
        FCanvas.TextOut(FCol * FCharSize.cx, FRow * FCharSize.cy, CURSOR_CHAR);
        i := Low(FCharacters);
        for row := 0 to FMaxRow - 1 do
            for col := 0 to FMaxCol - 1 do
        begin
            c := FCharacters[i];
            if ((c = START_BLINK_CHAR) or (c = END_BLINK_CHAR)) then
                FCanvas.TextOut(col * FCharSize.cx, row * FCharSize.cy, c);
            Inc(i);
        end;
    end;
end;

procedure TUniscope.DoTrafficPoll;
var
    bfr: String;
begin
    { TODO : probably not needed }
    bfr := Chr(EOT) + Chr(EOT);
    FTelnet.SendString(bfr);
end;

procedure TUniscope.Down;
begin
    HideCursor;
    if (FRow < FMaxRow) then
        Inc(FRow);
    SkipProtectForward;
    DrawStatus;
end;

procedure TUniscope.DrawStatus;
var
    holdCol: TColor;
    s: String;
    r: TRect;
begin
    r := Rect(0, FMaxRow * FCharSize.cy, FMaxCol * FCharSize.cx, (FMaxRow + 1) * FCharSize.cy);
    FCanvas.FillRect(r);

    s := Format('%2d %2d', [FRow + 1, FCol + 1]);
    FCanvas.TextOut(0, FMaxRow * FCharSize.cy, s);

    if ((FModel <> umConsole) and (FRid <> 0)) then
    begin
        s := Format('%2.2x:%2.2x', [FRid, FSid]);
        FCanvas.TextOut((FMaxCol - 10) * FCharSize.cx, FMaxRow * FCharSize.cy, s);
    end;

    if (FKbdLocked) then
    begin
        holdCol := TextColour;
        TextColour := BackColour;
        BackColour := holdCol;
        FCanvas.TextOut((FMaxCol - 4) * FCharSize.cx, FMaxRow * FCharSize.cy, 'WAIT');
        holdCol := TextColour;
        TextColour := BackColour;
        BackColour := holdCol;
    end;
end;

procedure TUniscope.EndProtected;
begin
    FProtected := False;
end;

procedure TUniscope.EraseDisplay(prot: Boolean);
var
    i: Integer;
begin
    i := (FRow * FMaxCol) + FCol;
    while (i <= High(FCharacters)) do
    begin
        if (prot or (not (tProtected in FAttributes[i]))) then
        begin
            FCharacters[i] := ' ';
            FAttributes[i] := [];
        end;
        Inc(i);
    end;
    Refresh;
end;

procedure TUniscope.FindSOE(var row, col: Integer);
// Scan backward to find SOE
var
    soeFound: Boolean;
begin
    row := FRow;
    col := FCol;
    soeFound := False;
    while (row >= 0) do
    begin
        while (col >= 0) do
        begin
            if (FCharacters[(row * FMaxCol) + col] = SOE_CHAR) then
            begin
                soeFound := True;
                Break;
            end;
            Dec(col);
        end;
        if (soeFound) then
            Break;
        Dec(row);
        col := FMaxCol;
    end;
    if (not soeFound) then
    begin
        col := 0;
        row := 0;
    end;
end;

function TUniscope.GetBackColour: TColor;
begin
    Result := FCanvas.Brush.Color;
end;

function TUniscope.GetCanvas: TCanvas;
begin
    Result := TCanvas(FCanvas);
end;

function TUniscope.GetCharacter: Char;
begin
    Result := FCharacters[(FRow * FMaxCol) + FCol];
end;

function TUniscope.GetCharSize: TSize;
begin
    if (not FCharSizeValid) then
    begin
        FCharSize := FCanvas.TextExtent('X');
        FCharSize.cx := FCharSize.cx + 1;
        FCharSizeValid := True;
    end;
    Result := FCharSize;
end;

function TUniscope.GetDisplaySize: TSize;
begin
    Result.cx := FMaxCol * CharSize.cx;
    Result.cy := (FMaxRow + 1) * CharSize.cy;
end;

function TUniscope.GetFont: TFont;
begin
    Result := FCanvas.Font;
end;

function TUniscope.GetTextColour: TColor;
begin
    Result := FCanvas.Font.Color;
end;

procedure TUniscope.HideCursor;
begin
    FCanvas.TextOut(FCol * FCharSize.cx, FRow * FCharSize.cy, GetCharacter);
end;

procedure TUniscope.Home;
begin
    HideCursor;
    FRow := 0;
    FCol := 0;
    SkipProtectForward;
    DrawStatus;
end;

procedure TUniscope.IncCursor;
begin
    Inc(FCol);
    if (FCol >= FMaxCol) then
    begin
        FCol := 0;
        Inc(FRow);
        if (FRow >= FMaxRow) then
            FRow := 0;
    end;
    SkipProtectForward;
end;

function TUniscope.IsProtected(row, col: Integer): Boolean;
var
    i: Integer;
begin
    i := (row * FMaxCol) + col;
    Result := (tProtected in FAttributes[i]);
end;

procedure TUniscope.KeyPress(var Key: Char);
begin
    if (FKbdLocked) then
        Exit;

    case Key of
      #8:
      begin
        BackSpace;
      end;
      else
      begin
        if ((Key >= ' ') and (Key <= '~')) then
        begin
            if (not IsProtected(FRow, FCol)) then
            begin
                CharOut(UpCase(Key));
            end;
        end;
      end;
    end;
end;

procedure TUniscope.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    case Key of
      VK_F11:       MsgWait;
      VK_F12:       Transmit;
      VK_HOME:      Home;
      VK_LEFT:      Left;
      VK_RIGHT:     Right;
      VK_UP:        Up;
      VK_DOWN:      Down;
    end;
end;

procedure TUniscope.Left;
begin
    HideCursor;
    DecCursor;
    DrawStatus;
end;

procedure TUniscope.MsgWait;
var
    bfr: AnsiString;
begin
    bfr := AnsiChar(BEL) + AnsiChar(ETX);
    FTelnet.SendString(bfr);
end;

procedure TUniscope.OpenPrntrFile;
var
    dir: String;
begin
    FreeAndNil(FPrntrFile);
    dir := PublicDataDir + '\Univac 9030 Emulator\Data';
    if (not DirectoryExists(dir)) then
        ForceDirectories(dir);
    FPrntrFile := TFileStream.Create(dir + '\U9030Cop.txt', fmCreate or fmShareDenyWrite);
end;

procedure TUniscope.Print;
var
    row, col: Integer;
    c: AnsiChar;
    bfr: AnsiString;
begin
    FindSOE(row, col);
    while (row <= FRow) do
    begin
        while (col < FMaxCol) do
        begin
            c := AnsiChar(FCharacters[(row * FMaxCol) + col]);
            if ((c = TAB_CHAR) or (c = START_BLINK_CHAR) or (c = END_BLINK_CHAR)) then
                c := ' ';
            if ((c >= ' ') and (c <> SOE_CHAR)) then
                bfr := bfr + c;
            Inc(col);
        end;
        bfr := bfr + AnsiString(#13#10);
        Inc(row);
        col := 0;
    end;
    if (not Assigned(FPrntrFile)) then
        OpenPrntrFile;
    FPrntrFile.Write(PAnsiChar(bfr)^, Length(bfr));
end;

procedure TUniscope.PrintTransparent;
begin
    { TODO : Implement print functions }
end;

procedure TUniscope.Repaint;
begin
    inherited Repaint;
    Refresh;
end;

procedure TUniscope.Right;
begin
    HideCursor;
    IncCursor;
    DrawStatus;
end;

procedure TUniscope.Refresh;
var
    i, row, col: Integer;
begin
    row := 0;
    col := 0;
    for i := Low(FCharacters) to High(FCharacters) do
    begin
        FCanvas.TextOut(col, row, FCharacters[i]);
        Inc(col, FCharSize.cx);
        if (col >= (FMaxCol * FCharSize.cx)) then
        begin
            Inc(row, FCharSize.cy);
            col := 0;
        end;
    end;
    DrawStatus;
end;

procedure TUniscope.SetArrayLengths;
begin
    SetLength(FCharacters, FMaxRow * FMaxCol);
    SetLength(FAttributes, FMaxRow * FMaxCol);
end;

procedure TUniscope.SetBackColour(const Value: TColor);
begin
    FCanvas.Brush.Color := Value;
end;

procedure TUniscope.SetCharacter(c: Char);
var
    i: Integer;
begin
    i := (FRow * FMaxCol) + FCol;
    FCharacters[i] := c;
    if (FHostControl and FProtected) then
        Include(FAttributes[i], tProtected);

end;

procedure TUniscope.SetSize(const Value: TUniscopeSize);
begin
    FSize := Value;
    case FSize of
      us16x64:
      begin
        FMaxRow := 16;
        FMaxCol := 64;
      end;
      us12x80:
      begin
        FMaxRow := 12;
        FMaxCol := 80;
      end;
      us24x80:
      begin
        FMaxRow := 24;
        FMaxCol := 80;
      end;
    end;
    SetArrayLengths;
end;

procedure TUniscope.SetTelnet(const Value: TIdTelnet);
begin
    FTelnet := Value;
    FTelnet.OnConnected := TelnetConnected;
    FTelnet.OnDisconnected := TelnetDisconnected;
    FTelnet.OnDataAvailable := TelnetDataAvailable;
end;

procedure TUniscope.SetTextColour(const Value: TColor);
begin
    FCanvas.Font.Color := Value;
end;

procedure TUniscope.SkipProtectBackward;
var
    i, fin, origRow, origCol: Integer;
begin
    origRow := FRow;
    origCol := FCol;
    // Skip until beginning of next unprotected field
    i := (FRow * FMaxCol) + FCol;
    fin := 0;
    if (not FHostControl) then
    begin
        while ((i >= fin) and (tProtected in FAttributes[i])) do
            Dec(i);
        if (i >= fin) then
        begin
            FRow := i div FMaxCol;
            FCol := i mod FMaxCol;
        end else
        begin
            FCol := origCol;
            FRow := origRow;
        end;
    end;
end;

procedure TUniscope.SkipProtectForward;
// Skip until beginning of next unprotected field
var
    i, fin, origRow, origCol: Integer;
begin
    origRow := FRow;
    origCol := FCol;
    i := (FRow * FMaxCol) + FCol;
    fin := (FMaxRow * FMaxCol) - 1;
    if (not FHostControl) then
    begin
        while ((i <= fin) and (tProtected in FAttributes[i])) do
            Inc(i);
        if (i <= fin) then
        begin
            FRow := i div FMaxCol;
            FCol := i mod FMaxCol;
        end else
        begin
            FCol := origCol;
            FRow := origRow;
        end;
    end;
end;

procedure TUniscope.StartProtected;
var
    i: Integer;
begin
    i := (FRow * FMaxCol) + FCol;
    Include(FAttributes[i], tProtected);
    FProtected := True;
end;

procedure TUniscope.TelnetConnected(Sender: TObject);
begin
    Clear;
end;

procedure TUniscope.TelnetDataAvailable(Sender: TIdTelnet; const Buffer: TIdBytes);
var
    b, x, y, shiftIn: Byte;
    rid, sid, did: Byte;
    i: Integer;
    mtype: TMessageType;

    procedure SkipToEtx;
    // Skip to first character past ETX.
    begin
        while ((i <= High(Buffer)) and (Buffer[i] <> ETX)) do
            Inc(i);
        Inc(i);
    end;

    procedure DoHeader;
    // Process a Uniscope protocol header.
    begin
        rid := $20;
        sid := $50;
        did := $70;
        mtype := [];
        Inc(i);
        if (i <= High(Buffer)) then
            rid := Buffer[i];
        Inc(i);
        if (i <= High(Buffer)) then
            sid := Buffer[i];
        Inc(i);
        if (i <= High(Buffer)) then
            did := Buffer[i];
        Inc(i);
        { TODO :
This stuff is probably not needed because we won't be receiving polls
and acknowledgements here. }
        while ((i <= High(Buffer)) and (Buffer[i] <> ETX)) do
        begin
            b := Buffer[i];
            case b of
              STX:
              begin
                mtype := [mtText];
                Dec(i);
                Break;
              end;
              BEL:
              begin
                mtype := [mtMsgWait];
                Inc(i);
                Break;
              end;
              ENQ:
              begin
                mtype := mtype + [mtStatusPoll];
                Inc(i);
              end;
              DLE:
              begin
                Inc(i);
                if (i <= High(Buffer)) then
                begin
                    b := Buffer[i];
                    case b of
                      Ord('1'):
                      begin
                        mtype := mtype + [mtAck];
                        Inc(i);
                      end;
                      NAK:
                      begin
                        mtype := [mtRetransmitReq];
                        Inc(i);
                        Break;
                      end;
                    end;
                end;
              end;
            end;
        end;
        // Ignore messages not intended for us
        if (((rid <> $20) and (rid <> FRid)) or
            ((sid <> $50) and (sid <> FSid))) then
        begin
            SkipToEtx;
            Exit;
        end;
        //
        if (mtText in mtype) then
            Exit
        else if (mtMsgWait in mtype) then
            DoBell
        else if (mtRetransmitReq in mtype) then
            DoRetransmit
        else
        begin
            if (mtype = []) then
                DoTrafficPoll;
            if (mtStatusPoll in mtype) then
                DoStatus;
            if (mtAck in mtype) then
                DoAcknowledge;
        end;
        SkipToEtx;
    end;

    procedure DoEscape;
    begin
        if (i >= High(Buffer)) then
            Exit;
        b := Buffer[i + 1];
        case b of
          HT:
          begin
            CharOut(TAB_CHAR);
            IncCursor;
            Inc(i);
          end;
          VT:
          begin
              Inc(i, 2);
              if (i > High(Buffer) - 2) then
                Exit;
              y := Buffer[i];
              x := Buffer[i + 1];
              shiftIn := Buffer[i + 2];
              CursorPosition(y - Ord(' '), x - Ord(' '));
              Inc(i, 2);
          end;
          DC2:
          begin
            PrintTransparent;
          end;
          Ord('a'):
          begin
            EraseDisplay(False);
            Inc(i);
          end;
          Ord('e'):
          begin
            CursorHome;
            Inc(i);
          end;
          Ord('k'):
          begin
            DeleteLine;
            Inc(i);
          end;
          Ord('M'):
          begin
            EraseDisplay(True);
            Inc(i);
          end;
//          else
//          begin
//            ShowMessageFmt('Unimplemented escape code %2.2x-%s', [Ord(b), b]);
//          end;
        end;
    end;

    procedure TraceBuffer;
    var
        word: UInt32;
        hex: AnsiString;
        i: Integer;
    begin
        FTraceFile.Write(PAnsiChar(AnsiString(#13#10#13#10))^, 4);
        word := 0;
        for i := Low(Buffer) to High(Buffer) do
        begin
            if ((i <> 0) and ((i mod 4) = 0)) then
            begin
                hex := AnsiString(Format('%8.8x ', [word]));
                FTraceFile.Write(PAnsiChar(hex)^, 9);
                word := 0;
            end;
            word := (word shl 8) or Buffer[i];
        end;
        if (word <> 0) then
        begin
            hex := AnsiString(Format('%8.8x ', [word]));
            FTraceFile.Write(PAnsiChar(hex)^, 9);
        end;
    end;

begin
    if (Assigned(FTraceFile)) then
        TraceBuffer;

    FHostControl := True;
    HideCursor;
    try
        i := Low(Buffer);
        while(i <= High(Buffer)) do
        begin
            b := Buffer[i];
            case b of
              NUL:
                ;
              LF:
                ;
              TNC_IAC:                              // Get RID/SID assigned by 90/30 emulator
              begin
                if (FModel <> umConsole) then
                begin
                    Inc(i);
                    if (i <= High(Buffer)) then
                        FRid := Buffer[i];
                    Inc(i);
                    if (i <= High(Buffer)) then
                        FSid := Buffer[i];
                    DrawStatus;
                end;
              end;
              CR:
              begin
                FCol := 0;
                Inc(FRow);
                if (FRow >= FMaxRow) then
                    FRow := 0;
                DrawStatus;
              end;
              SOH:
              begin
                DoHeader;
              end;
              STX:
              begin
                FKbdLocked := True;
                FLockKbd := False;
                FProtected := False;
              end;
              ETX:
              begin
                FKbdLocked := FLockKbd;
                FLockKbd := False;
                DrawStatus;
              end;
              SO:
              begin
                StartProtected;
              end;
              SI:
              begin
                EndProtected;
              end;
              ESC:
              begin
                DoEscape;
              end;
              FS:
              begin
                CharOut(START_BLINK_CHAR);
              end;
              GS:
              begin
                CharOut(END_BLINK_CHAR);
              end;
              RS:
              begin
                CharOut(SOE_CHAR);
              end;
              DC2:
              begin
                Print;
              end;
              DC4:
              begin
                FLockKbd := True;
              end;
              $fe:
              begin
                PostMessage(Application.MainFormHandle, WM_CLOSE, 0, 0);
                Exit;
              end;
              else
                if (b >= SPACE) then
                    CharOut(b);
            end;
            Inc(i);
        end;
    finally
        FHostControl := False;
    end;
end;

procedure TUniscope.TelnetDisconnected(Sender: TObject);
begin
    if (Assigned(FCanvas)) then
    begin
        Clear;
        FCanvas.TextOut(0, 0, 'Connection terminated');
    end;
end;

procedure TUniscope.Transmit;
var
    row, col, maxCol, i: Integer;
    inProt: Boolean;
    bfr: String;

    function TrimRight(bfr: String): String;
    // Override TrimRight so that it just looks at spaces, not all
    // control characters
    var
        i: Integer;
    begin
        i := Length(bfr);
        while ((i >= 1) and (bfr[i] = ' ')) do
            Dec(i);
        Result := Copy(bfr, 1, i);
    end;

begin
    if (FKbdLocked) then
        Exit;

    FindSOE(row, col);
    bfr := Chr(ESC) + Chr(VT) + Chr(row + Ord(' ')) + Chr(col + Ord(' ')) + Chr(0) + Chr(SI);
    if (FCharacters[(row * FMaxCol) + col] = SOE_CHAR) then
    begin
        bfr := bfr + Chr(RS);
        Inc(col);
        if (col >= FMaxCol) then
        begin
            col := 0;
            Inc(row);
        end;
    end;
    // Send text
    inProt := False;
    while (row <= FRow) do
    begin
        if (row = FRow) then
            maxCol := FCol
        else
            maxCol := FMaxCol - 1;
        while (col <= maxcol) do
        begin
            i := (row * FMaxCol) + col;
            if ((not inProt) and (tProtected in FAttributes[i])) then
            begin
                inProt := True;
            end else if (inProt and (not (tProtected in FAttributes[i]))) then
            begin
                inProt := False;
                bfr := TrimRight(bfr) + Chr(SUB);
            end;
            if (not inProt) then
            begin
                case FCharacters[i] of
                  TAB_CHAR:     bfr := bfr + Chr(HT);
                  else          bfr := bfr + FCharacters[i];
                end;
            end;
            Inc(col);
        end;
        if (row < FRow) then
        begin
            // On all rows but the last, replace trailing spaces with <CR>
            bfr := TrimRight(bfr) + #13;
        end;
        col := 0;
        Inc(row);
    end;
    bfr := Chr(STX) + bfr + Chr(ETX);
    FTelnet.SendString(bfr);
    FKbdLocked := True;
end;

procedure TUniscope.Up;
begin
    HideCursor;
    if (FRow > 0) then
        Dec(FRow);
    SkipProtectBackward;
    DrawStatus;
end;

end.
