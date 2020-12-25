unit LoadCardsFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Samples.Spin, Vcl.StdCtrls;

type
  TLoadCardsForm = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    FileEdt: TEdit;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    BlankCardsEdt: TSpinEdit;
    OkBtn: TButton;
    CancelBtn: TButton;
    LoadCardsDlg: TOpenDialog;
    procedure Button1Click(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    function GetFileName: String;
    function GetNumCards: Integer;
    { Private declarations }
  public
    property FileName: String read GetFileName;
    property NumCards: Integer read GetNumCards;
  end;

var
  LoadCardsForm: TLoadCardsForm;

implementation

{$R *.dfm}

procedure TLoadCardsForm.Button1Click(Sender: TObject);
begin
    if (LoadCardsDlg.Execute) then
        FileEdt.Text := LoadCardsDlg.FileName;
end;

function TLoadCardsForm.GetFileName: String;
begin
    Result := FileEdt.Text;
end;

function TLoadCardsForm.GetNumCards: Integer;
begin
    Result := BlankCardsEdt.Value;
end;

procedure TLoadCardsForm.OkBtnClick(Sender: TObject);
begin
    FileEdt.Text := Trim(FileEdt.Text);
    if ((FileEdt.Text = '') and (BlankCardsEdt.Value = 0)) then
    begin
        FileEdt.SetFocus;
        raise Exception.Create('Please select the file name or the number of blank cards to be loaded.');
    end;
    if ((FileEdt.Text <> '') and (BlankCardsEdt.Value <> 0)) then
    begin
        FileEdt.SetFocus;
        raise Exception.Create('Please select either the file name or the number of blank cards ' +
                               'to be loaded but not both.');
    end;
    ModalResult := mrOk;
end;

end.
