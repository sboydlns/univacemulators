unit U1005PunchFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Gauges, U1005Punch;

type
  TU1005PunchFrame = class(TFrame)
    Label63: TLabel;
    PunchInput: TGauge;
    PunchOutput1: TGauge;
    Label66: TLabel;
    Label67: TLabel;
    PunchLoadBtn: TButton;
    PunchEmptyBtn: TButton;
    ReadStation: TGauge;
    PunchOutput2: TGauge;
    PunchStation: TGauge;
    SaveHopper1Btn: TButton;
    SaveHopper2Btn: TButton;
    SaveDlg: TSaveDialog;
    Punchoutput1Lbl: TLabel;
    PunchOutput2Lbl: TLabel;
    PunchInputLbl: TLabel;
    procedure PunchEmptyBtnClick(Sender: TObject);
    procedure PunchLoadBtnClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SaveHopper1BtnClick(Sender: TObject);
    procedure SaveHopper2BtnClick(Sender: TObject);
  private
    FPunch: T1005Punch;
    procedure DoPunchFeed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent;  pun: T1005Punch); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses U1005Types, EmulatorTypes, CardFile, LoadCardsFrm;

{ TU9200PunchFrame }

constructor TU1005PunchFrame.Create(AOwner: TComponent; pun: T1005Punch);
begin
    inherited Create(Aowner);
    FPunch := pun;
    FPunch.OnFeed := DoPunchFeed;
    SaveDlg.InitialDir := CardFileDir;
end;

destructor TU1005PunchFrame.Destroy;
begin
    inherited;
end;

procedure TU1005PunchFrame.DoPunchFeed(Sender: TObject);
begin
    PunchInput.Progress := FPunch.InputCount;
    PunchOutput1.Progress := FPunch.OutputCount1;
    PunchOutput2.Progress := FPunch.OutputCount2;
    PunchInputLbl.Caption := IntToStr(FPunch.InputCount);
    PunchOutput1Lbl.Caption := IntToStr(FPunch.OutputCount1);
    PunchOutput2Lbl.Caption := IntToStr(FPunch.OutputCount2);
    if (FPunch.ReadStationLoaded) then
        ReadStation.Progress := 1
    else
        ReadStation.Progress := 0;
    if (FPunch.PunchStationLoaded) then
        PunchStation.Progress := 1
    else
        PunchStation.Progress := 0;
end;

procedure TU1005PunchFrame.FrameResize(Sender: TObject);
begin
    PunchInput.Height := ClientHeight - PunchInput.Top - 5;
    PunchOutput1.Height := (PunchInput.Height div 2) - 5;
    PunchOutput2.Top := PunchOutput1.Top + PunchOutput1.Height + 10;
    PunchOutput2.Height := PunchOutput1.Height;
    PunchOutput2Lbl.Top := PunchOutput2.Top + 2;
end;

procedure TU1005PunchFrame.PunchEmptyBtnClick(Sender: TObject);
begin
    FPunch.EmptyHoppers;
    PunchInput.Progress := 0;
    PunchOutput1.Progress := 0;
    PunchOutput2.Progress := 0;
    PunchInputLbl.Caption := '0';
    PunchOutput1Lbl.Caption := '0';
    PunchOutput2Lbl.Caption := '0';
end;

procedure TU1005PunchFrame.PunchLoadBtnClick(Sender: TObject);
begin
    if (LoadCardsForm.ShowModal <> mrOk) then
        Exit;

    if (LoadCardsForm.FileName <> '') then
        FPunch.AddFile(LoadCardsForm.FileName)
    else
        FPunch.AddBlankCards(LoadCardsForm.NumCards);
    PunchInput.Progress := FPunch.InputCount;
    PunchInputLbl.Caption := IntToStr(FPunch.InputCount);
end;

procedure TU1005PunchFrame.SaveHopper1BtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
    begin
        FPunch.SaveHopper(1, SaveDlg.FileName);
        FPunch.EmptyHopper(1);
        PunchOutput1.Progress := 0;
        PunchOutput1Lbl.Caption := '0';
    end;
end;

procedure TU1005PunchFrame.SaveHopper2BtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
    begin
        FPunch.SaveHopper(2, SaveDlg.FileName);
        FPunch.EmptyHopper(2);
        PunchOutput2.Progress := 0;
        PunchOutput2Lbl.Caption := '0';
    end;
end;

end.
