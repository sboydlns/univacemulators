unit U494ReaderFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Gauges, U494Reader;

type
  TU494ReaderFrame = class(TFrame)
    Label63: TLabel;
    ReaderInput: TGauge;
    Stacker0: TGauge;
    Label66: TLabel;
    Label67: TLabel;
    ReaderLoadBtn: TButton;
    ReaderEmptyBtn: TButton;
    LoadCardsDlg: TOpenDialog;
    ReadStation: TGauge;
    Stacker0Lbl: TLabel;
    ReaderInputLbl: TLabel;
    procedure ReaderEmptyBtnClick(Sender: TObject);
    procedure ReaderLoadBtnClick(Sender: TObject);
  private
    FReader: T494Reader;
    procedure DoReaderFeed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; rdr: T494Reader); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

uses CardFile, EmulatorTypes;

{ TU9200ReaderFrame }

constructor TU494ReaderFrame.Create(AOwner: TComponent;  rdr: T494Reader);
begin
    inherited Create(Aowner);
    FReader := rdr;
    FReader.OnFeed := DoReaderFeed;
    LoadCardsDlg.InitialDir := CardFileDir
end;

destructor TU494ReaderFrame.Destroy;
begin
    inherited;
end;

procedure TU494ReaderFrame.DoReaderFeed(Sender: TObject);
begin
    ReaderInput.Progress := FReader.InputCount;
    Stacker0.Progress := FReader.Stacker0Count;
    ReaderInputLbl.Caption := IntToStr(FReader.InputCount);
    Stacker0Lbl.Caption := IntToStr(FReader.Stacker0Count);
    if (FReader.ReadStationLoaded) then
        ReadStation.Progress := 1
    else
        ReadStation.Progress := 0;
end;

procedure TU494ReaderFrame.ReaderEmptyBtnClick(Sender: TObject);
begin
    FReader.EmptyHopper;
    ReaderInput.Progress := 0;
    Stacker0.Progress := 0;
    ReaderInputLbl.Caption := '0';
    Stacker0Lbl.Caption := '0';
end;

procedure TU494ReaderFrame.ReaderLoadBtnClick(Sender: TObject);
begin
    if (LoadCardsDlg.Execute) then
        FReader.AddFile(LoadCardsDlg.FileName);
    ReaderInput.Progress := FReader.InputCount;
    ReaderInputLbl.Caption := IntToStr(FReader.InputCount);
end;

end.
