unit U9200CardCvtFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm3 = class(TForm)
    OpenDlg: TOpenDialog;
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    ConvertBtn: TButton;
    CancelBtn: TButton;
    CodeBtn: TRadioGroup;
    procedure CancelBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure ConvertBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.dfm}

uses CardFile, U9200Types;

procedure TForm3.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TForm3.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TForm3.ConvertBtnClick(Sender: TObject);
var
    fin: TCardFileStream;
    fout: TFileStream;
    bfr: TCardRec;
    b: Byte;
    i: Integer;
begin
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the path to the raw card file');
    fin := TCardFileStream.Create(FileNameEdt.Text, fmOpenRead);
    fout := TFileStream.Create(FileNameEdt.Text + '.crd', fmCreate);
    try
        while (fin.ReadTranslate(bfr) > 0) do
        begin
            if (CodeBtn.ItemIndex = 1) then
            begin
                for i := 1 to 80 do
                    bfr.Columns[i] := Ord(TCodeTranslator.EbcdicToAscii(bfr.Columns[i]));
            end;
            fout.Write(bfr.Columns, 80);
        end;
    finally
        fin.Free;
        fout.Free;
    end;
end;

end.
