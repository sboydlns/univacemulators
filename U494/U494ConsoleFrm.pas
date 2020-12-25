unit U494ConsoleFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

const
  START_MSG = WM_USER + 1;

type
  TU494ConsoleForm = class(TForm)
    Timer: TTimer;
    Printer: TMemo;
    procedure TimerTimer(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormShow(Sender: TObject);
  private
    FPipe: THandle;
    FKeyBfr: AnsiString;
    FInTimer: Boolean;
    FMaxLines: Integer;
    procedure StartMsg(var Message: TMessage); message START_MSG;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  U494ConsoleForm: TU494ConsoleForm;

implementation

{$R *.dfm}

uses U494Util, EmulatorTypes;

{ TForm4 }

constructor TU494ConsoleForm.Create(AOwner: TComponent);
begin
    inherited;
    PostMessage(Handle, START_MSG, 0, 0);
end;

destructor TU494ConsoleForm.Destroy;
begin
    CloseHandle(FPipe);
    inherited Destroy;
end;

procedure TU494ConsoleForm.FormKeyPress(Sender: TObject; var Key: Char);
var
    ac: AnsiChar;
begin
    if ((Ord(Key) > 0 ) and (Ord(Key) <= 127)) then
    begin
        // A couple of special codes that are specific to the console
        // that aren't in the translate table.
        case Key of
          #8:
          begin
            FKeyBfr := FKeyBfr + #$3f;
            Key := Chr(0);
          end;
          #10:
          begin
            FKeyBfr := FKeyBfr + #3;
            Key := Chr(0);
          end;
          #13:
          begin
            FKeyBfr := FKeyBfr + #4;
            Key := Chr(0);
          end;
          else
          begin
            if ((Key >= ' ') and (Key <= '~')) then
            begin
                ac := TCodeTranslator.AsciiToFieldata(AnsiChar(Key));
                FKeyBfr := FKeyBfr + ac;
            end;
            Key := Chr(0);
          end;
        end;
    end else
    begin
        Key := Chr(0);
        Beep;
    end;
end;

procedure TU494ConsoleForm.FormShow(Sender: TObject);
begin
    FMaxLines := Printer.ClientHeight div Abs(Printer.Font.Height);
end;

procedure TU494ConsoleForm.StartMsg(var Message: TMessage);
var
    msg: String;
    mode: Cardinal;
    i: Integer;
begin
    for i := 1 to FMaxLines do
        Printer.Lines.Add('');
    FPipe := CreateFile('\\.\pipe\U494Console',
                        GENERIC_READ  or GENERIC_WRITE,
                        0,
                        nil,
                        OPEN_EXISTING,
                        0,
                        0);
    if (FPipe = INVALID_HANDLE_VALUE) then
    begin
        msg := WinError;
        raise Exception.CreateFmt('Could not open pipe. %s', [msg]);
    end;
    mode := PIPE_READMODE_BYTE or PIPE_NOWAIT;
    if (not SetNamedPipeHandleState(FPipe, mode, nil, nil)) then
    begin
        msg := WinError;
        raise Exception.CreateFmt('Could not set pipe mode. %s', [msg]);
    end;
    Timer.Enabled := True;
end;

procedure TU494ConsoleForm.TimerTimer(Sender: TObject);
var
    bytesWritten: Cardinal;
    msg, line: String;
    bfr: array [1..80] of AnsiChar;
    i, l: Integer;
    errno: Cardinal;
    bytesRead: Cardinal;
begin
    if (FInTimer) then
        Exit;

    FInTimer := True;
    try
        if (FKeyBfr <> '') then
        begin
            while (Length(FKeyBfr) > 0) do
            begin
                if (not WriteFile(FPipe, PAnsiChar(FKeyBfr)^, Length(FKeyBfr), bytesWritten, nil)) then
                begin
                    msg := WinError;
                    raise Exception.CreateFmt('Could not write to pipe. %s', [msg]);
                end;
                FKeyBfr := Copy(FKeyBfr, bytesWritten + 1);
            end;
        end;
        if (not ReadFile(FPipe, bfr[1], SizeOf(bfr), bytesRead, nil)) then
        begin
            errno := GetLastError;
            if (errno <> ERROR_NO_DATA) then
            begin
                // Did the server close the pipe?
                if (errno = ERROR_BROKEN_PIPE) then
                begin
                    PostMessage(Handle, WM_CLOSE, 0, 0);
                    Exit;
                end;
                msg := WinError;
                raise Exception.CreateFmt('Pipe read error! %s', [msg]);
            end;
        end else
        begin
            if (Pos('$$$shutdown$$$', String(AnsiString(bfr))) <> 0) then
            begin
                PostMessage(Handle, WM_CLOSE, 0, 0);
                Exit;
            end;
            l := FMaxLines - 1;
            line := Printer.Lines[l];
            for i := 1 to bytesRead do
            begin
                case bfr[i] of
                  #3,
                  #4:
                  begin
                    Printer.Lines[l] := line;
                    Printer.Lines.Delete(0);
                    Printer.Lines.Add('');
                    line := '';
                  end;
                  #$3f:
                  begin
                    line := Copy(line, 1, Length(line) - 1);
                  end
                  else
                  begin
                    line := line + Char(TCodeTranslator.FieldataToAscii(Byte(bfr[i])));
                  end;
                end;
            end;
            Printer.Lines[l] := line;
        end;
    finally
        FInTimer := False;
    end;
end;

end.
