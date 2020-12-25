unit TapeTestFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, TapeFile;

type
  TTapeTestForm = class(TForm)
    Label1: TLabel;
    FileNameEdt: TEdit;
    ReadFwdBtn: TButton;
    ProgressLbl: TLabel;
    Label2: TLabel;
    SkipEdt: TEdit;
    FileFwdBtn: TButton;
    OpenBtn: TButton;
    ReadBkwdBtn: TButton;
    FileBkwdBtn: TButton;
    procedure ReadFwdBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure FileFwdBtnClick(Sender: TObject);
    procedure ReadBkwdBtnClick(Sender: TObject);
    procedure FileBkwdBtnClick(Sender: TObject);
  private
    FTapeFile: TTapeFile;
  public
    { Public declarations }
  end;

var
  TapeTestForm: TTapeTestForm;

implementation

{$R *.dfm}

uses Math, EmulatorTypes, U9200Types;

procedure TTapeTestForm.FileBkwdBtnClick(Sender: TObject);
var
    filesToSkip: Integer;
begin
    if (not TryStrToInt(SkipEdt.Text, filesToSkip)) then
        filesToSkip := 1;
    while (filesToSkip > 0) do
    begin
        FTapeFile.BackwardSpaceFile;
        Dec(filesToSkip);
    end;
end;

procedure TTapeTestForm.FileFwdBtnClick(Sender: TObject);
var
    filesToSkip: Integer;
begin
    if (not TryStrToInt(SkipEdt.Text, filesToSkip)) then
        filesToSkip := 1;
    while (filesToSkip > 0) do
    begin
        FTapeFile.ForwardSpaceFile;
        Dec(filesToSkip);
    end;
end;

procedure TTapeTestForm.FormShow(Sender: TObject);
begin
    ProgressLbl.Caption := '';
end;

procedure TTapeTestForm.OpenBtnClick(Sender: TObject);
begin
    FreeAndNil(FTapeFile);
    if (FileExists(FileNameEdt.Text)) then
        FTapeFile := TTapeFile.Create(FileNameEdt.Text, fmOpenReadWrite or fmExclusive)
    else
        FTapeFile := TTapeFile.Create(FileNameEdt.Text, fmCreate or fmExclusive);
end;

procedure TTapeTestForm.ReadBkwdBtnClick(Sender: TObject);
var
    fout: TFileStream;
    bfr: array[0..65535] of Byte;
    c: AnsiChar;
    count: Integer;
    i: Integer;
    j: Integer;
    crnl: AnsiString;
begin
    crnl := #13#10;
    fout := TFileStream.Create(Format('c:\temp\AWS\file%d.txt', [FTapeFile.FileNum]), fmCreate);
    try
        count := FTapeFile.ReadBackward(@bfr, SizeOf(bfr));
        while (not FTapeFile.Eof) do
        begin
            for i := High(bfr) downto High(bfr) - count + 1 do
            begin
                c := TCodeTranslator.EbcdicToAscii(bfr[i]);
                bfr[i] := Byte(c);
            end;
            i := High(bfr) - count + 1;
            while (i <= High(bfr)) do
            begin
                j := Min(count, 80);
                fout.Write((@bfr[i])^, j);
                fout.Write(PAnsiChar(crnl)^, 2);
                Inc(i, 80);
            end;
            count := FTapeFile.ReadBackward(@bfr, SizeOf(bfr));
        end;
    finally
        fout.Free;
    end;
end;

procedure TTapeTestForm.ReadFwdBtnClick(Sender: TObject);
var
    fout: TFileStream;
    bfr: array[0..65535] of Byte;
    c: AnsiChar;
    count: Integer;
    i: Integer;
    j: Integer;
    crnl: AnsiString;
begin
    crnl := #13#10;
    fout := TFileStream.Create(Format('c:\temp\AWS\file%d.txt', [FTapeFile.FileNum]), fmCreate);
    try
        count := FTapeFile.ReadForward(@bfr, SizeOf(bfr));
        while (not FTapeFile.Eof) do
        begin
            for i := 0 to count - 1 do
            begin
                c := TCodeTranslator.EbcdicToAscii(bfr[i]);
                bfr[i] := Byte(c);
            end;
            i := 0;
            while (i < count) do
            begin
                j := Min(count - i, 80);
                fout.Write((@bfr[i])^, j);
                fout.Write(PAnsiChar(crnl)^, 2);
                Inc(i, 80);
            end;
            count := FTapeFile.ReadForward(@bfr, SizeOf(bfr));
        end;
    finally
        fout.Free;
    end;
end;

end.
