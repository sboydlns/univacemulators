unit U9200TapeFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons,
  U9200Tape, Vcl.ExtCtrls;

type
  TU9200TapeFrame = class(TFrame)
    LocalBtn: TSpeedButton;
    LocalLbl: TLabel;
    RemoteBtn: TSpeedButton;
    RemoteLbl: TLabel;
    ForwardBtn: TSpeedButton;
    ForwardLbl: TLabel;
    UnloadBtn: TSpeedButton;
    UnloadLbl: TLabel;
    WriteEnableBtn: TSpeedButton;
    WriteEnableLbl: TLabel;
    RewindLbl: TLabel;
    RewindBtn: TSpeedButton;
    LoadLbl: TLabel;
    LoadBtn: TSpeedButton;
    OnLbl: TLabel;
    OnBtn: TSpeedButton;
    OffBtn: TSpeedButton;
    OffLbl: TLabel;
    StatusLbl: TLabel;
    UnitLbl: TLabel;
    Timer: TTimer;
    procedure LocalBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LocalBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RemoteBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RemoteBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForwardBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ForwardBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OnBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OffBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure OffBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LoadBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LoadBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LoadBtnClick(Sender: TObject);
    procedure UnloadBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UnloadBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure UnloadBtnClick(Sender: TObject);
    procedure OffBtnClick(Sender: TObject);
    procedure RewindBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RewindBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RewindBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FPowerOn: Boolean;
    FGreenOffUp: TBitmap;
    FGreenOnUp: TBitmap;
    FGreenDown: TBitmap;
    FRedOffUp: TBitmap;
    FRedOnUp: TBitmap;
    FRedDown: TBitmap;
    FWhiteOffUp: TBitmap;
    FWhiteOnUp: TBitmap;
    FWhiteDown: TBitmap;
    FYellowOffUp: TBitmap;
    FYellowOnUp: TBitmap;
    FYellowDown: TBitmap;
    FTapeDevice: TU92Tape;
    procedure RefreshButtons;
  public
    constructor Create(AOwner: TComponent; dev: TU92Tape); reintroduce;
  end;

implementation

{$R *.dfm}

uses MountTapeFrm;

{ TU9200TapeFrame }

constructor TU9200TapeFrame.Create(AOwner: TComponent; dev: TU92Tape);
begin
    inherited Create(AOwner);
    FTapeDevice := dev;
    UnitLbl.Caption := Format('Unit # %d', [FTapeDevice.Address]);
    // Create button glyphs
    FGreenOffUp := TBitmap.Create;
    FGreenOffUp.LoadFromResourceName(hInstance, 'GREEN_TAPE_OFF_UP');
    FGreenOnUp := TBitmap.Create;
    FGreenOnUp.LoadFromResourceName(hInstance, 'GREEN_TAPE_ON_UP');
    FGreenDown := TBitmap.Create;
    FGreenDown.LoadFromResourceName(hInstance, 'GREEN_TAPE_DOWN');
    FRedOffUp := TBitmap.Create;
    FRedOffUp.LoadFromResourceName(hInstance, 'RED_TAPE_OFF_UP');
    FRedOnUp := TBitmap.Create;
    FRedOnUp.LoadFromResourceName(hInstance, 'RED_TAPE_ON_UP');
    FRedDown := TBitmap.Create;
    FRedDown.LoadFromResourceName(hInstance, 'RED_TAPE_DOWN');
    FWhiteOffUp := TBitmap.Create;
    FWhiteOffUp.LoadFromResourceName(hInstance, 'WHITE_TAPE_OFF_UP');
    FWhiteOnUp := TBitmap.Create;
    FWhiteOnUp.LoadFromResourceName(hInstance, 'WHITE_TAPE_ON_UP');
    FWhiteDown := TBitmap.Create;
    FWhiteDown.LoadFromResourceName(hInstance, 'WHITE_TAPE_DOWN');
    FYellowOffUp := TBitmap.Create;
    FYellowOffUp.LoadFromResourceName(hInstance, 'YELLOW_TAPE_OFF_UP');
    FYellowOnUp := TBitmap.Create;
    FYellowOnUp.LoadFromResourceName(hInstance, 'YELLOW_TAPE_ON_UP');
    FYellowDown := TBitmap.Create;
    FYellowDown.LoadFromResourceName(hInstance, 'YELLOW_TAPE_DOWN');
    StatusLbl.Caption := '';
end;

procedure TU9200TapeFrame.ForwardBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    ForwardBtn.Glyph.Assign(FGreenDown);
end;

procedure TU9200TapeFrame.ForwardBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RefreshButtons;
end;

procedure TU9200TapeFrame.LoadBtnClick(Sender: TObject);
begin
    if ((not FPowerOn) or (MountTapeForm.ShowModal <> MrOk)) then
        Exit;
    FTapeDevice.Mount(MountTapeForm.FileName, MountTapeForm.WriteEnable);
    StatusLbl.Caption := Format('Mounted %s Position %d of %d',
                                [ExtractFileName(MountTapeForm.FileName),
                                 FTapeDevice.FilePosition,
                                 FTapeDevice.FileSize]);
    RefreshButtons;
end;

procedure TU9200TapeFrame.LoadBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    LoadBtn.Glyph.Assign(FWhiteDown);
end;

procedure TU9200TapeFrame.LoadBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RefreshButtons;
end;

procedure TU9200TapeFrame.LocalBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    LocalBtn.Glyph.Assign(FWhiteDown);
end;

procedure TU9200TapeFrame.LocalBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        FTapeDevice.Online := False;
    RefreshButtons;
end;

procedure TU9200TapeFrame.OffBtnClick(Sender: TObject);
begin
    FTapeDevice.Unmount;
    StatusLbl.Caption := '';
    RefreshButtons;
end;

procedure TU9200TapeFrame.OffBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    OffBtn.Glyph.Assign(FRedDown);
end;

procedure TU9200TapeFrame.OffBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    FPowerOn := False;
    RefreshButtons;
end;

procedure TU9200TapeFrame.OnBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    OnBtn.Glyph.Assign(FGreenDown);
end;

procedure TU9200TapeFrame.OnBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (not FPowerOn) then
        FPowerOn := True;
    RefreshButtons;
end;

procedure TU9200TapeFrame.RefreshButtons;
begin
    if (FPowerOn) then
    begin
        if (FTapeDevice.Online) then
        begin
            LocalBtn.Glyph.Assign(FWhiteOffUp);
            RemoteBtn.Glyph.Assign(FGreenOnUp);
        end else
        begin
            LocalBtn.Glyph.Assign(FWhiteOnUp);
            RemoteBtn.Glyph.Assign(FGreenOffUp);
        end;
        ForwardBtn.Glyph.Assign(FGreenOffUp);
        UnloadBtn.Glyph.Assign(FRedOffUp);
        if (FTapeDevice.WriteEnable) then
            WriteEnableBtn.Glyph.Assign(FYellowOnUp)
        else
            WriteEnableBtn.Glyph.Assign(FYellowOffUp);
        RewindBtn.Glyph.Assign(FWhiteOffUp);
        if (FTapeDevice.LoadPoint) then
            LoadBtn.Glyph.Assign(FWhiteOnUp)
        else
            LoadBtn.Glyph.Assign(FWhiteOffUp);
        OnBtn.Glyph.Assign(FGreenOnUp);
        OffBtn.Glyph.Assign(FRedOffUp);
        if (FTapeDevice.FileName = '') then
            StatusLbl.Caption := ''
        else
            StatusLbl.Caption := Format('Mounted %s Position %d of %d',
                                        [ExtractFileName(FTapeDevice.FileName),
                                         FTapeDevice.FilePosition,
                                         FTapeDevice.FileSize]);
    end else
    begin
        LocalBtn.Glyph.Assign(FWhiteOffUp);
        RemoteBtn.Glyph.Assign(FGreenOffUp);
        ForwardBtn.Glyph.Assign(FGreenOffUp);
        UnloadBtn.Glyph.Assign(FRedOffUp);
        WriteEnableBtn.Glyph.Assign(FYellowOffUp);
        RewindBtn.Glyph.Assign(FWhiteOffUp);
        LoadBtn.Glyph.Assign(FWhiteOffUp);
        OnBtn.Glyph.Assign(FGreenOffUp);
        OffBtn.Glyph.Assign(FRedOffUp);
        StatusLbl.Caption := '';
    end;
end;

procedure TU9200TapeFrame.RemoteBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RemoteBtn.Glyph.Assign(FGreenDown);
end;

procedure TU9200TapeFrame.RemoteBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if ((FPowerOn) and FTapeDevice.Mounted) then
        FTapeDevice.OnLine := True;
    RefreshButtons;
end;

procedure TU9200TapeFrame.RewindBtnClick(Sender: TObject);
begin
    if ((not FPowerOn) or (FTapeDevice.Online)) then
        Exit;
    FTapeDevice.ManualRewind;
    RefreshButtons;
end;

procedure TU9200TapeFrame.RewindBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RewindBtn.Glyph.Assign(FWhiteDown);
end;

procedure TU9200TapeFrame.RewindBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RefreshButtons;
end;

procedure TU9200TapeFrame.TimerTimer(Sender: TObject);
begin
    RefreshButtons;
end;

procedure TU9200TapeFrame.UnloadBtnClick(Sender: TObject);
begin
    if ((not FPowerOn) or (FTapeDevice.OnLine)) then
        Exit;
    FTapeDevice.Unmount;
    StatusLbl.Caption := '';
    RefreshButtons;
end;

procedure TU9200TapeFrame.UnloadBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    UnloadBtn.Glyph.Assign(FRedDown);
end;

procedure TU9200TapeFrame.UnloadBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    RefreshButtons;
end;

end.
