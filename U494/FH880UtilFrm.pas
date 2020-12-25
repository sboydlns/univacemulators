unit FH880UtilFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;
const
    AUTO_START_MSG = WM_USER;

type
  TFH880UtilForm = class(TForm)
    Label1: TLabel;
    LocationEdt: TEdit;
    BrowseBtn: TButton;
    CreateBtn: TButton;
    ProgressLbl: TLabel;
    DumpBtn: TButton;
    OpenDrumDlg: TOpenDialog;
    FileNameEdt: TEdit;
    Label2: TLabel;
    LoadTextBtn: TButton;
    Label3: TLabel;
    ImportEdt: TEdit;
    ImportBrowseBtn: TButton;
    OpenDataDlg: TOpenDialog;
    LoadPgmBtn: TButton;
    MfdMemo: TMemo;
    MfdBtn: TButton;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CreateBtnClick(Sender: TObject);
    procedure DumpBtnClick(Sender: TObject);
    procedure LoadTextBtnClick(Sender: TObject);
    procedure ImportBrowseBtnClick(Sender: TObject);
    procedure LoadPgmBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MfdBtnClick(Sender: TObject);
  private
    FAutoLoadText: Boolean;
    FAutoLoadPgm: Boolean;
    FAutoCreate: Boolean;
    procedure AutoStart(var Message: TMessage); message AUTO_START_MSG;
    procedure EditFileNames;
    procedure EditLocation;
    procedure LoadPgmFile;
    procedure LoadTextFile;
    procedure ParseCmdLine;
  public
    { Public declarations }
  end;

var
  FH880UtilForm: TFH880UtilForm;

implementation

uses FileCtrl, FH880File, FH880DumpFrm, SrcFile, EmulatorTypes;

{$R *.dfm}

procedure TFH880UtilForm.AutoStart(var Message: TMessage);
begin
    if (FAutoLoadText) then
        LoadTextBtnClick(nil)
    else if (FAutoLoadPgm) then
        LoadPgmBtnClick(nil)
    else if (FAutoCreate) then
        CreateBtnClick(nil);
end;

procedure TFH880UtilForm.BrowseBtnClick(Sender: TObject);
begin
    if (not OpenDrumDlg.Execute) then
        Exit;
    LocationEdt.Text := OpenDrumDlg.FileName;
end;

procedure TFH880UtilForm.CreateBtnClick(Sender: TObject);
var
    fout: TFH880File;
    fname, vol: String;
    i: Integer;
begin
    fout := nil;
    EditLocation;
    ProgressLbl.Visible := True;
    fname := LocationEdt.Text;
    try
        if (FileExists(fname)) then
        begin
            ShowMessage('File already exists');
            Exit;
        end;
        ProgressLbl.Caption := 'Creating file ...';
        Application.ProcessMessages;
        fout := TFH880File.Create(fname, fmCreate);
        try
            ProgressLbl.Caption := 'Formatting ...';
            Application.ProcessMessages;
            vol := ExtractFileName(LocationEdt.Text);
            i := LastDelimiter('.', vol);
            if (i > 0) then
                vol := Copy(vol, 1, i - 1);
            fout.Format(AnsiString(UpperCase(vol)));
        except
            FreeAndNil(fout);
            DeleteFile(fname);
        end;
    finally
        FreeAndNil(fout);
        ProgressLbl.Visible := False;
    end;
    if (FAutoCreate) then
        PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TFH880UtilForm.DumpBtnClick(Sender: TObject);
begin
    EditLocation;
    FH880DumpForm.Show(LocationEdt.Text);
end;

procedure TFH880UtilForm.EditFileNames;
begin
    ImportEdt.Text := Trim(ImportEdt.Text);
    if (ImportEdt.Text = '') then
        raise Exception.Create('Please enter the import file name');
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the drum file name');
end;

procedure TFH880UtilForm.EditLocation;
begin
    LocationEdt.Text := Trim(LocationEdt.Text);
    if (LocationEdt.Text = '') then
        raise Exception.Create('Please enter the Location');
end;

procedure TFH880UtilForm.FormShow(Sender: TObject);
begin
    ParseCmdLine;
    if (FAutoLoadText or FAutoLoadPgm or FAutoCreate) then
    begin
        PostMessage(Handle, AUTO_START_MSG, 0, 0);
        Exit;
    end;
    if (LocationEdt.Text = '') then
    begin
        if (FileExists('..\..\Drums\sysvol.drum')) then
            LocationEdt.Text := '..\..\Drums\sysvol.drum'
        else if (FileExists('..\Drums\sysvol.drum')) then
            LocationEdt.Text := '..\Drums\sysvol.drum';
    end;
end;

procedure TFH880UtilForm.ImportBrowseBtnClick(Sender: TObject);
begin
    if (not OpenDataDlg.Execute) then
        Exit;
    ImportEdt.Text := OpenDataDlg.FileName;
end;

procedure TFH880UtilForm.LoadPgmBtnClick(Sender: TObject);
begin
    EditLocation;
    EditFileNames;
    LoadPgmFile;
end;

procedure TFH880UtilForm.LoadPgmFile;
var
    drum: TFH880File;
    pgm: TFileStream;
    fout: TDrumFile;
    fcb: TFileControlBlock;
    stat, bytesRead: Integer;
    bfr: array [0..255] of UInt32;
begin
    drum := TFH880File.Create(LocationEdt.Text, fmOpenReadWrite or fmExclusive);
    try
        if (drum.FileExists(AnsiString(FileNameEdt.Text), fcb)) then
        begin
            if (not FAutoLoadPgm) then
            begin
                stat := MessageBox(Handle, 'File already exists. Overwrite?', '',
                                   MB_APPLMODAL or MB_ICONQUESTION or MB_YESNO);
                if (stat <> IDYES) then
                    Exit;
            end;
            drum.DeleteFile(AnsiString(FileNameEdt.Text));
        end;
        pgm := TFileStream.Create(ImportEdt.Text, fmOpenRead or fmShareDenyNone);
        try
            fout := TDrumFile.Create(drum, AnsiString(FileNameEdt.Text), dfmCreate);
            try
                bytesRead := pgm.Read(bfr[0], 256*SizeOf(UInt32));
                while (bytesRead > 0) do
                begin
                    fout.Write(bfr, bytesRead div SizeOf(UInt32));
                    bytesRead := pgm.Read(bfr[0], 256*SizeOf(UInt32));
                end;
            finally
                fout.Free;
            end;
        finally
            pgm.Free;
        end;
    finally
        drum.Free;
    end;
    if (FAutoLoadPgm) then
        PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TFH880UtilForm.LoadTextBtnClick(Sender: TObject);
begin
    EditLocation;
    EditFileNames;
    LoadTextFile;
end;

procedure TFH880UtilForm.LoadTextFile;
var
    drum: TFH880File;
    text: TSrcFileStream;
    fout: TDrumFile;
    fcb: TFileControlBlock;
    line: AnsiString;
    stat: Integer;
begin
    drum := TFH880File.Create(LocationEdt.Text, fmOpenReadWrite or fmExclusive);
    try
        if (drum.FileExists(AnsiString(FileNameEdt.Text), fcb)) then
        begin
            if (not FAutoLoadText) then
            begin
                stat := MessageBox(Handle, 'File already exists. Overwrite?', '',
                                   MB_APPLMODAL or MB_ICONQUESTION or MB_YESNO);
                if (stat <> IDYES) then
                    Exit;
            end;
            drum.DeleteFile(AnsiString(FileNameEdt.Text));
        end;
        text := TSrcFileStream.Create(ImportEdt.Text);
        try
            fout := TDrumFile.Create(drum, AnsiString(FileNameEdt.Text), dfmCreate);
            try
                while (not text.Eof) do
                begin
                    line := text.ReadLine;
                    fout.WriteText(line);
                end;
            finally
                fout.Free;
            end;
        finally
            text.Free;
        end;
    finally
        drum.Free;
    end;
    if (FAutoLoadText) then
        PostMessage(Handle, WM_CLOSE, 0, 0);
end;

procedure TFH880UtilForm.MfdBtnClick(Sender: TObject);
var
    drum: TFH880File;
    mfd: TMasterFileDirectory;
    fcb: TFileControlBlock;
    stat: Integer;
    fname: AnsiString;
begin
    EditLocation;
    drum := TFH880File.Create(LocationEdt.Text, fmOpenRead or fmExclusive);
    try
        mfd := TMasterFileDirectory.Create(drum);
        try
            MfdMemo.Lines.Clear;
            MfdMemo.Lines.Add('           Size Start');
            stat := mfd.FirstFile(fcb);
            while (stat >= 0) do
            begin
                fname := TCodeTranslator.FieldataToAscii(fcb.Name[0] shr 24) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[0] shr 18) and $3f) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[0] shr 12) and $3f) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[0] shr 6) and $3f) +
                         TCodeTranslator.FieldataToAscii(fcb.Name[0] and $3f) +
                         TCodeTranslator.FieldataToAscii(fcb.Name[1] shr 24) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[1] shr 18) and $3f) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[1] shr 12) and $3f) +
                         TCodeTranslator.FieldataToAscii((fcb.Name[1] shr 6) and $3f) +
                         TCodeTranslator.FieldataToAscii(fcb.Name[1] and $3f);
                MfdMemo.Lines.Add(Format('%-10.10s %4d %4d', [fname, fcb.Size, fcb.Start]));
                stat := mfd.NextFile(stat, fcb);
            end;
        finally
            mfd.Free;
        end;
    finally
        drum.Free;
    end;

end;

procedure TFH880UtilForm.ParseCmdLine;
var
    i: Integer;
    p: String;
begin
    i := 1;
    while (i <= ParamCount) do
    begin
        p := ParamStr(i);
        if (p = '-d') then
        begin
            Inc(i);
            LocationEdt.Text := ParamStr(i);
        end else if (p = '-i') then
        begin
            Inc(i);
            ImportEdt.Text := ParamStr(i);
        end else if (p = '-f') then
        begin
            Inc(i);
            FileNameEdt.Text := ParamStr(i);
        end else if (p = '-lt') then
            FAutoLoadText := True
        else if (p = '-lp') then
            FAutoLoadPgm := True
        else if (p = '-c') then
            FAutoCreate := True;
        Inc(i);
    end;
end;

end.
