unit U9030ConsoleTestFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, IdBaseComponent, IdComponent, IdCustomTCPServer, IdTelnetServer,
  IdContext, idTelnet;

type
  TU9030ConsoleTestForm = class(TForm)
    GoBtn: TButton;
    Telnet: TIdTelnetServer;
    Memo: TMemo;
    procedure GoBtnClick(Sender: TObject);
    procedure TelnetConnect(AContext: TIdContext);
    procedure TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
      var AAuthenticated: Boolean);
    procedure TelnetListenException(AThread: TIdListenerThread; AException: Exception);
    procedure FormShow(Sender: TObject);
    procedure TelnetExecute(AContext: TIdContext);
  private
    FConsole: TIdContext;
    FBuffer: AnsiString;
    FStxSeen: Boolean;
    procedure Display(sender : String; msg : string);
  public
    { Public declarations }
  end;

var
  U9030ConsoleTestForm: TU9030ConsoleTestForm;

implementation

uses IdGlobal, EmulatorTypes;

const
  // ASCII  control characters
  NUL = 0;
  SOH = 1;
  STX = 2;
  ETX = 3;
  EOT = 4;
  ENQ = 5;
  ACK = 6;
  BEL = 7;
  BS = 8;
  HT = 9;
  LF = 10;
  VT = 11;
  FF = 12;
  CR = 13;
  SO = 14;
  SI = 15;
  DLE = 16;
  DC1 = 17;
  DC2 = 18;
  DC3 = 19;
  DC4 = 20;
  NAK = 21;
  SYN = 22;
  ETB = 23;
  CAN = 24;
  EM = 25;
  SUB = 26;
  ESC = 27;
  FS = 28;
  GS = 29;
  RS = 30;
  US = 31;
  SPACE = 32;


{$R *.dfm}

procedure TU9030ConsoleTestForm.FormShow(Sender: TObject);
begin
    Telnet.Active := True;
end;

procedure TU9030ConsoleTestForm.GoBtnClick(Sender: TObject);
const
    test1: array [1..74] of Byte = (
        $27, $85, $27, $D4, $0F, $C9, $D7, $D3, $40, $E3, $D6,
        $40, $D3, $D6, $C1, $C4, $40, $E2, $E3, $C1, $D5, $C4,
        $C1, $D9, $C4, $40, $E2, $E4, $D7, $C5, $D9, $E5, $C9,
        $E2, $D6, $D9, $40, $E4, $D5, $D3, $C5, $E2, $E2, $40,
        $D5, $C5, $E6, $40, $D5, $C1, $D4, $C5, $40, $D2, $C5,
        $E8, $C5, $C4, $40, $40, $1E, $6D, $6D, $6D, $6D, $6D,
        $6D, $6B, $6D, $27, $0B, $40, $E7, $0F
    );

    test2: array[1..35] of Byte = (
        ESC, VT, Ord('/'), SPACE, SI, RS, SO,
        FS, Ord('L'), Ord('I'), Ord('N'), Ord('E'), SPACE, Ord('1'), Ord('6'), GS,
        ESC, Ord('e'), ESC, Ord('k'),
        ESC, VT, Ord('/'), SPACE, SI, RS,
        Ord('L'), Ord('I'), Ord('N'), Ord('E'), SPACE, Ord('1'), Ord('6'), Ord('A'),
        SI
    );

var
    b: Byte;
    s: AnsiString;
begin
//    s := '';
//    for b in test1 do
//    begin
//        s := s + TCodeTranslator.EbcdicToAscii(b);
//    end;
//    FConsole.Connection.IOHandler.Write(String(s));
    s := '';
    for b in test2 do
    begin
        s := s + Chr(b);
    end;
    FConsole.Connection.IOHandler.Write(String(s));
end;

procedure TU9030ConsoleTestForm.TelnetAuthentication(AContext: TIdContext; const AUsername, APassword: string;
  var AAuthenticated: Boolean);
begin
    AAuthenticated := True;
end;

procedure TU9030ConsoleTestForm.TelnetConnect(AContext: TIdContext);
const
   cmd: array [0..1] of Byte = ( TNC_IAC, Ord(' ') );
var
    ip: String;
    port: Integer;
    peerIP: String;
    peerPort: Integer;
begin
    ip        := AContext.Binding.IP;
    port      := AContext.Binding.Port;
    peerIP    := AContext.Binding.PeerIP;
    peerPort  := AContext.Binding.PeerPort;
    Display('SERVER', 'Client Connected!');
    Display('SERVER', 'Port=' + IntToStr(Port)
                      + ' '   + '(PeerIP=' + PeerIP
                      + ' - ' + 'PeerPort=' + IntToStr(PeerPort) + ')'
           );
    FConsole := AContext;
    FConsole.Connection.IOHandler.Write(cmd[0]);
    FConsole.Connection.IOHandler.Write(cmd[1]);
end;

procedure TU9030ConsoleTestForm.TelnetExecute(AContext: TIdContext);
var
    bfr: TIdBytes;
    i: Integer;
    b: Byte;
    etxSeen: Boolean;
begin
    AContext.Connection.IOHandler.ReadTimeout := 10;
    AContext.Connection.IOHandler.ReadBytes(bfr, -1, False);
    etxSeen := False;
    i := Low(bfr);
    while ((not FStxSeen) and (i <= High(bfr))) do
    begin
        FStxSeen := (bfr[i] = STX);
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
                FBuffer := FBuffer + AnsiChar(bfr[i]);
                Inc(i);
            end;
        end;
    end;
    if (etxSeen) then
    begin
        ShowMessage(FBuffer);
        FStxSeen := False;
        FBuffer := '';
    end;
end;

procedure TU9030ConsoleTestForm.TelnetListenException(AThread: TIdListenerThread; AException: Exception);
begin
    ShowMessage('OOPS!');
end;

procedure TU9030ConsoleTestForm.Display(sender : String; msg : string);
begin
    TThread.Queue(nil, procedure
                       begin
                           Memo.Lines.Add('[' + sender + '] - ' + msg);
                       end
                 );

end;

end.
