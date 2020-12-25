unit U1005ReaderFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, U1005Reader, Vcl.StdCtrls, Vcl.Samples.Gauges;

type
  TU1005ReaderFrame = class(TFrame)
    Label63: TLabel;
    ReaderInput: TGauge;
    ReaderOutput: TGauge;
    Label66: TLabel;
    Label67: TLabel;
    ReadStation: TGauge;
    ReaderOutputLbl: TLabel;
    ReaderInputLbl: TLabel;
    ReaderLoadBtn: TButton;
    ReaderEmptyBtn: TButton;
    LoadCardsDlg: TOpenDialog;
    procedure ReaderLoadBtnClick(Sender: TObject);
    procedure ReaderEmptyBtnClick(Sender: TObject);
  private
    FReader: T1005Reader;
    procedure DoReaderFeed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; rdr: T1005Reader); reintroduce;
  end;

implementation

uses EmulatorTypes;

{$R *.dfm}

constructor TU1005ReaderFrame.Create(AOwner: TComponent; rdr: T1005Reader);
begin
    inherited Create(Aowner);
    FReader := rdr;
    FReader.OnFeed := DoReaderFeed;
    LoadCardsDlg.InitialDir := CardFileDir
end;

procedure TU1005ReaderFrame.DoReaderFeed(Sender: TObject);
begin
    ReaderInput.Progress := FReader.InputCount;
    ReaderOutput.Progress := FReader.OutputCount;
    ReaderInputLbl.Caption := IntToStr(FReader.InputCount);
    ReaderOutputLbl.Caption := IntToStr(FReader.OutputCount);
    if (FReader.ReadStationLoaded) then
        ReadStation.Progress := 1
    else
        ReadStation.Progress := 0;
end;

procedure TU1005ReaderFrame.ReaderEmptyBtnClick(Sender: TObject);
begin
    FReader.EmptyHopper;
    ReaderInput.Progress := 0;
    ReaderOutput.Progress := 0;
    ReaderInputLbl.Caption := '0';
    ReaderOutputLbl.Caption := '0';
end;

procedure TU1005ReaderFrame.ReaderLoadBtnClick(Sender: TObject);
begin
    if (LoadCardsDlg.Execute) then
        FReader.AddFile(LoadCardsDlg.FileName);
    ReaderInput.Progress := FReader.InputCount;
    ReaderInputLbl.Caption := IntToStr(FReader.InputCount);
end;

end.
