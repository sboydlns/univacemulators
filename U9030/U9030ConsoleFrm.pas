﻿unit U9030ConsoleFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdTelnet, IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdGlobal,
  Vcl.ExtCtrls, Vcl.StdCtrls, Uniscope;

const
  START_MSG = WM_USER + 1;

type
  TU9030ConsoleForm = class(TForm)
    Telnet: TIdTelnet;
    Timer: TTimer;
    procedure TelnetTelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TelnetStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormPaint(Sender: TObject);
  private
    FDestroying: Boolean;
    FDisplay: TUniscope;
    procedure StartMsg(var Message: TMessage); message START_MSG;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U9030ConsoleForm: TU9030ConsoleForm;

implementation

{$R *.dfm}
constructor TU9030ConsoleForm.Create(AOwner: TComponent);
begin
    inherited;
    FDisplay := TUniscope.Create(Self, umConsole, us16x64);
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

procedure TU9030ConsoleForm.FormDestroy(Sender: TObject);
begin
    FDestroying := True;
end;

procedure TU9030ConsoleForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
    FDisplay.KeyPress(Key);
end;

procedure TU9030ConsoleForm.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    FDisplay.KeyUp(Sender, Key, Shift);
end;

procedure TU9030ConsoleForm.FormPaint(Sender: TObject);
begin
    FDisplay.Repaint;
end;

procedure TU9030ConsoleForm.FormShow(Sender: TObject);
begin
    PostMessage(Handle, START_MSG, 0, 0);
end;

procedure TU9030ConsoleForm.StartMsg(var Message: TMessage);
begin
    Timer.Enabled := True;
end;

procedure TU9030ConsoleForm.TelnetStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
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

procedure TU9030ConsoleForm.TelnetTelnetCommand(Sender: TIdTelnet; Status: TIdTelnetCommand);
begin
    case Status of
      tncNoLocalEcho:   ShowMessage('NoLocalEcho');
      tncLocalEcho:     ShowMessage('LocalEcho');
      tncEcho:          ShowMessage('Echo');
      else              ShowMessage('Other');
    end;
end;

procedure TU9030ConsoleForm.TimerTimer(Sender: TObject);
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
