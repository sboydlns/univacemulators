unit U494PunchFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Gauges, U494Reader;

type
  TU494PunchFrame = class(TFrame)
    Label63: TLabel;
    PunchInput: TGauge;
    Stacker0: TGauge;
    Label66: TLabel;
    StkrHdr0Lbl: TLabel;
    PunchLoadBtn: TButton;
    PunchEmptyBtn: TButton;
    ReadStation: TGauge;
    Stacker1: TGauge;
    PunchStation: TGauge;
    SaveHopper1Btn: TButton;
    SaveHopper2Btn: TButton;
    SaveDlg: TSaveDialog;
    Stacker0Lbl: TLabel;
    Stacker1Lbl: TLabel;
    PunchInputLbl: TLabel;
    Stkr1HdrLbl: TLabel;
    procedure PunchEmptyBtnClick(Sender: TObject);
    procedure PunchLoadBtnClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure SaveHopper1BtnClick(Sender: TObject);
    procedure SaveHopper2BtnClick(Sender: TObject);
  private
    FPunch: T494Punch;
    procedure DoPunchFeed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent;  pun: T494Punch); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses EmulatorTypes, CardFile, LoadCardsFrm;

{ TU9200PunchFrame }

constructor TU494PunchFrame.Create(AOwner: TComponent; pun: T494Punch);
begin
    inherited Create(Aowner);
    FPunch := pun;
    FPunch.OnFeed := DoPunchFeed;
    SaveDlg.InitialDir := CardFileDir;
end;

destructor TU494PunchFrame.Destroy;
begin
    inherited;
end;

procedure TU494PunchFrame.DoPunchFeed(Sender: TObject);
begin
    PunchInput.Progress := FPunch.InputCount;
    Stacker0.Progress := FPunch.Stacker0Count;
    Stacker1.Progress := FPunch.Stacker1Count;
    PunchInputLbl.Caption := IntToStr(FPunch.InputCount);
    Stacker0Lbl.Caption := IntToStr(FPunch.Stacker0Count);
    Stacker1Lbl.Caption := IntToStr(FPunch.Stacker1Count);
    if (FPunch.ReadStationLoaded) then
        ReadStation.Progress := 1
    else
        ReadStation.Progress := 0;
    if (FPunch.PunchStationLoaded) then
        PunchStation.Progress := 1
    else
        PunchStation.Progress := 0;
end;

procedure TU494PunchFrame.FrameResize(Sender: TObject);
begin
    PunchInput.Height := ClientHeight - PunchInput.Top - 5;
    Stacker0.Height := ((PunchInput.Height - Stkr1HdrLbl.Height) div 2) - 5;
    Stacker0Lbl.Top := Stacker0.Top + (Stacker0.Height div 2) - (Stacker0Lbl.Height div 2);
    Stkr1HdrLbl.Top := Stacker0.Top + Stacker0.Height + 8;
    Stacker1.Top := Stacker0.Top + Stacker0.Height + Stkr1HdrLbl.Height + 10;
    Stacker1.Height := Stacker0.Height;
    Stacker1Lbl.Top := Stacker1.Top + (Stacker1.Height div 2) - (Stacker1Lbl.Height div 2);
end;

procedure TU494PunchFrame.PunchEmptyBtnClick(Sender: TObject);
begin
    FPunch.EmptyHoppers;
    PunchInput.Progress := 0;
    Stacker0.Progress := 0;
    Stacker1.Progress := 0;
    PunchInputLbl.Caption := '0';
    Stacker0Lbl.Caption := '0';
    Stacker1Lbl.Caption := '0';
end;

procedure TU494PunchFrame.PunchLoadBtnClick(Sender: TObject);
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

procedure TU494PunchFrame.SaveHopper1BtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
    begin
        FPunch.SaveHopper(1, SaveDlg.FileName);
        FPunch.EmptyHopper(1);
        Stacker0.Progress := 0;
        Stacker0Lbl.Caption := '0';
    end;
end;

procedure TU494PunchFrame.SaveHopper2BtnClick(Sender: TObject);
begin
    if (SaveDlg.Execute) then
    begin
        FPunch.SaveHopper(2, SaveDlg.FileName);
        FPunch.EmptyHopper(2);
        Stacker1.Progress := 0;
        Stacker1Lbl.Caption := '0';
    end;
end;

end.
