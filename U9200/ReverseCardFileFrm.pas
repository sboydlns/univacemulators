unit ReverseCardFileFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Generics.Collections;

type
  TReverseCardFileForm = class(TForm)
    OpenDlg: TOpenDialog;
    SaveDlg: TSaveDialog;
    Label1: TLabel;
    Label2: TLabel;
    InputFileEdt: TEdit;
    OutputFileEdt: TEdit;
    InputFileBtn: TButton;
    OutputFileBtn: TButton;
    OkBtn: TButton;
    CancelBtn: TButton;
    Label3: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure InputFileBtnClick(Sender: TObject);
    procedure OutputFileBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ReverseCardFileForm: TReverseCardFileForm;

implementation

{$R *.dfm}

uses CardFile;

procedure TReverseCardFileForm.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TReverseCardFileForm.InputFileBtnClick(Sender: TObject);
begin
    if (not OpenDlg.Execute) then
        Exit;
    InputFileEdt.Text := OpenDlg.FileName;
end;

procedure TReverseCardFileForm.OkBtnClick(Sender: TObject);
var
    inFile: TCardFileStream;
    outFile: TFileStream;
    inRec, outRec: TCardRec;
    outRecs: TList<TCardRec>;
    i, j: Integer;
begin
    outRecs := TLIst<TCardRec>.Create;
    inFile := TCardFileStream.Create(InputFileEdt.Text, fmOpenRead);
    try
        outFile := TFileStream.Create(OutputFileEdt.Text, fmCreate);
        try
            while (not inFile.Eof) do
            begin
                inFile.ReadRaw(inRec);
                i := 1;
                j := 159;
                while (i < 160) do
                begin
                    outRec.Columns[j] := inRec.Columns[i];
                    outRec.Columns[j + 1] := inRec.Columns[i + 1];
                    Inc(i, 2);
                    Dec(j, 2);
                end;
                outRecs.Add(outRec);
            end;
            for i := outRecs.Count - 1 downto 0 do
            begin
                outRec := outRecs[i];
                outFile.Write(outRec.Columns, 160);
            end;
        finally
            outFile.Free;
        end;
    finally
        outRecs.Free;
        inFile.Free;
    end;
end;

procedure TReverseCardFileForm.OutputFileBtnClick(Sender: TObject);
begin
    if (not SaveDlg.Execute) then
        Exit;
    OutputFileEdt.Text := SaveDlg.FileName;
end;

end.
