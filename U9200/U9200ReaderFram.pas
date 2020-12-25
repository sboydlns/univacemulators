unit U9200ReaderFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Gauges, U9200Reader;

type
  TU9200ReaderFrame = class(TFrame)
    Label63: TLabel;
    ReaderInput: TGauge;
    ReaderOutput: TGauge;
    Label66: TLabel;
    Label67: TLabel;
    ReaderLoadBtn: TButton;
    ReaderEmptyBtn: TButton;
    LoadCardsDlg: TOpenDialog;
    ReadStation: TGauge;
    ReaderOutputLbl: TLabel;
    ReaderInputLbl: TLabel;
    procedure ReaderEmptyBtnClick(Sender: TObject);
    procedure ReaderLoadBtnClick(Sender: TObject);
  private
    FReader: TU92Reader;
    procedure DoReaderFeed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; rdr: TU92Reader); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses CardFile, U9200Types, EmulatorTypes;

{ TU9200ReaderFrame }

constructor TU9200ReaderFrame.Create(AOwner: TComponent;  rdr: TU92Reader);
begin
    inherited Create(Aowner);
    FReader := rdr;
    FReader.OnFeed := DoReaderFeed;
    LoadCardsDlg.InitialDir := CardFileDir
end;

destructor TU9200ReaderFrame.Destroy;
begin
    inherited;
end;

procedure TU9200ReaderFrame.DoReaderFeed(Sender: TObject);
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

procedure TU9200ReaderFrame.ReaderEmptyBtnClick(Sender: TObject);
begin
    FReader.EmptyHopper;
    ReaderInput.Progress := 0;
    ReaderOutput.Progress := 0;
    ReaderInputLbl.Caption := '0';
    ReaderOutputLbl.Caption := '0';
end;

procedure TU9200ReaderFrame.ReaderLoadBtnClick(Sender: TObject);
begin
    if (LoadCardsDlg.Execute) then
        FReader.AddFile(LoadCardsDlg.FileName);
    ReaderInput.Progress := FReader.InputCount;
    ReaderInputLbl.Caption := IntToStr(FReader.InputCount);
end;

end.
