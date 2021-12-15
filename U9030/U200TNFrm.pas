unit U200TNFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdTelnet, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal,
  Vcl.ExtCtrls, Vcl.StdCtrls, Uniscope;

const
  START_MSG = WM_USER + 1;

type
  TU200TNForm = class(TForm)
    Telnet: TIdTelnet;
    Timer: TTimer;
    KeyTimer: TTimer;
    procedure TelnetTelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TelnetStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyTimerTimer(Sender: TObject);
  private
    FDestroying: Boolean;
    FDisplay: TUniscope;
    FKeyDown: Boolean;
    FCurKey: Word;
    FCurShift: TShiftState;
    FKeyDelay: Integer;
    procedure StartMsg(var Message: TMessage); message START_MSG;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U200TNForm: TU200TNForm;

implementation

{$R *.dfm}
constructor TU200TNForm.Create(AOwner: TComponent);
begin
    inherited;
    FDisplay := TUniscope.Create(Self, umU200, us24x80);
    FDisplay.Parent := Self;
    FDisplay.Align := alClient;
    FDisplay.TextColour := clGreen;
    FDisplay.BackColour := clBlack;
    FDisplay.Font.Name := 'Courier New';
    FDisplay.Font.Size := 12;
    FDisplay.Telnet := Telnet;
    ClientHeight := FDisplay.DisplaySize.cy;
    ClientWidth := FDisplay.DisplaySize.cx;
end;

procedure TU200TNForm.FormDestroy(Sender: TObject);
begin
    FDestroying := True;
end;

procedure TU200TNForm.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
// Record which key is pressed so that it can be repeated until the key is released.
begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or
        (Key = VK_UP) or (Key = VK_DOWN)) then
    begin
        if (not FKeyDown) then
        begin
            FKeyDown := True;
            FCurKey := Key;
            FCurShift := Shift;
            FKeyDelay := 10;
            FDisplay.KeyUp(Sender, Key, Shift);
        end;
    end else
        FDisplay.KeyUp(Sender, Key, Shift);
end;

procedure TU200TNForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    FDisplay.KeyPress(Key);
end;

procedure TU200TNForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    FKeyDown := False;
    // TAB is not sent on KeyDown so we need to do it here.
    if ((Key = VK_TAB) and (not (ssCtrl in Shift)))  then
        FDisplay.KeyUp(Sender, Key, Shift);
end;

procedure TU200TNForm.FormPaint(Sender: TObject);
begin
    FDisplay.Repaint;
end;

procedure TU200TNForm.FormShow(Sender: TObject);
begin
    PostMessage(Handle, START_MSG, 0, 0);
end;

procedure TU200TNForm.KeyTimerTimer(Sender: TObject);
begin
    if (FKeyDown) then
    begin
        Dec(FKeyDelay);
        if (FKeyDelay <= 0) then
            FDisplay.KeyUp(nil, FCurKey, FCurShift);
    end;
end;

procedure TU200TNForm.StartMsg(var Message: TMessage);
begin
    Timer.Enabled := True;
end;

procedure TU200TNForm.TelnetStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
begin
    if (not FDestroying) then
    begin
        case AStatus of
          hsResolving: ;
          hsConnecting: ;
          hsConnected: ;
          hsDisconnecting: ;
          hsDisconnected:
          begin
            FDisplay.Clear;
            FDisplay.Canvas.TextOut(0, 0, 'Connection terminated');
          end;
          hsStatusText: ;
          ftpTransfer: ;
          ftpReady: ;
          ftpAborted: ;
        end;
    end;
end;

procedure TU200TNForm.TelnetTelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
begin
    case Status of
      tncNoLocalEcho:   ShowMessage('NoLocalEcho');
      tncLocalEcho:     ShowMessage('LocalEcho');
      tncEcho:          ShowMessage('Echo');
      else              ShowMessage('Other');
    end;
end;

procedure TU200TNForm.TimerTimer(Sender: TObject);
begin
    if (not Telnet.Connected) then
    begin
        FDisplay.Canvas.TextOut(0, 0, 'Connecting ...');
        try
            Telnet.Connect;
        except
          on E: Exception do
          begin
            FDisplay.Canvas.TextOut(0, 0, E.Message);
          end;
        end;
    end;
end;

end.
