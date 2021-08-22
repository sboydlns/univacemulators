unit CommAdapter;

interface

uses SysUtils, Classes, IPC;

type
  TCommAdapter = class(TIPCDevice)
  private
    FSense: array [0..1] of Byte;
    FBCW: TIPCBCW;
    FCommand: Byte;
  protected
    procedure DoClear;
    procedure DoNop;
    procedure ProcessCommand; override;
  public
    constructor Create(num: Byte); override;
    destructor Destroy; override;
    procedure SIO; override;
  end;

implementation

uses Memory, U9030Types, Channels;

{ TCommAdapter }

constructor TCommAdapter.Create(num: Byte);
begin
    inherited;
    FCommand := 0;
    FBCW := TIPCBCW.Create(CA0_BCW0 + ((FDeviceNum - 4) * 16));
end;

destructor TCommAdapter.Destroy;
begin
    if (not Terminated) then
    begin
        Terminate;
        WaitFor;
    end;
    FreeAndNil(FBCW);
    inherited;
end;

procedure TCommAdapter.DoClear;
// Line adapater clear. In an emulated environment I don't think we
// need to do anything but present status.
begin
    FChannel.QueueStatus(MakeStatus(DEVICE_END, 0));
end;

procedure TCommAdapter.DoNop;
// No-op. Do nothing. Do not even return status.
begin
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
          $0f:
          begin
            DoClear;
          end;
        end;
    end;
    FBusy := False;
    FCommand := 0;
end;

procedure TCommAdapter.SIO;
begin
    if (FBusy) then
        // Since this is checked at the channel level,
        // this should never happer.
        Exit;
    FBusy := True;
    FCommand := FBCW.Command;
    FCmdRecvd.SetEvent;
end;

end.
