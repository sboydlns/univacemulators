unit U8418UtilFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, U8418, Vcl.ExtCtrls,
  OS3Files;

type
  TU8418UtilForm = class(TForm)
    Pages: TPageControl;
    VTOCPage: TTabSheet;
    ListVTOCBtn: TButton;
    VTOCMemo: TMemo;
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    OpenDlg: TOpenDialog;
    OpenBtn: TButton;
    LibsPage: TTabSheet;
    LibsList: TListBox;
    LibsDumpBtn: TButton;
    LibsTrackEdt: TLabeledEdit;
    LibsOpenBtn: TButton;
    LibsDirBtn: TButton;
    LibsPartEdt: TLabeledEdit;
    LibsMemo: TMemo;
    LibsSourceBtn: TButton;
    LibsSourceEdt: TLabeledEdit;
    LibsExportBtn: TButton;
    DiskPage: TTabSheet;
    DiskCylEdt: TLabeledEdit;
    DiskHeadEdt: TLabeledEdit;
    DiskRecEdt: TLabeledEdit;
    DiskNumRecsEdt: TLabeledEdit;
    DiskDumpBtn: TButton;
    DiskMemo: TMemo;
    DiskExportBtn: TButton;
    procedure BrowseBtnClick(Sender: TObject);
    procedure ListVTOCBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PagesChange(Sender: TObject);
    procedure LibsOpenBtnClick(Sender: TObject);
    procedure LibsDumpBtnClick(Sender: TObject);
    procedure LibsDirBtnClick(Sender: TObject);
    procedure LibsSourceBtnClick(Sender: TObject);
    procedure LibsExportBtnClick(Sender: TObject);
    procedure DiskDumpBtnClick(Sender: TObject);
    procedure DiskExportBtnClick(Sender: TObject);
  private
    FDisk: T8418Disk;
    FLibsFile: TLibsFile;
    procedure CloseDisk;
    function DumpBlock(recNo: Integer; bfr: array of Byte): String;
    procedure DumpLibsHdr;
    procedure InitLibsPage;
    procedure OpenDisk;
  public
    { Public declarations }
  end;

var
  U8418UtilForm: TU8418UtilForm;

implementation

uses VTOC, EmulatorTypes;

{$R *.dfm}

procedure TU8418UtilForm.CloseDisk;
begin
    FreeAndNil(FDisk);
end;

procedure TU8418UtilForm.DiskDumpBtnClick(Sender: TObject);
var
    cyl, head, rec, numRecs, count: Integer;
    bfr: array [0..255] of Byte;
    txt: String;
begin
    if (DiskCylEdt.Text = '') then
        raise Exception.Create('Please enter the starting cylinder #');
    if (not TryStrToInt(DiskCylEdt.Text, cyl)) then
        raise Exception.Create('Invalid cylinder #');
    if (DiskHeadEdt.Text = '') then
        raise Exception.Create('Please enter the starting head #');
    if (not TryStrToInt(DiskHeadEdt.Text, head)) then
        raise Exception.Create('Invalid head #');
    if (DiskRecEdt.Text = '') then
        raise Exception.Create('Please enter the starting record #');
    if (not TryStrToInt(DiskRecEdt.Text, rec)) then
        raise Exception.Create('Invalid record #');
    if (DiskNumRecsEdt.Text = '') then
        raise Exception.Create('Please enter the # of records');
    if (not TryStrToInt(DiskNumRecsEdt.Text, numRecs)) then
        raise Exception.Create('Invalid # of records');

    if (not Assigned(FDisk)) then
        OpenDisk;

    DiskMemo.Clear;
    for count := 1 to numRecs do
    begin
        FDisk.ReadSector(cyl, head, rec, PByte(@bfr));
        txt := DumpBlock(rec, bfr);
        DiskMemo.Text := DiskMemo.Text + txt + #13#10;
        Inc(rec);
        if (rec > 40) then
        begin
            Inc(head);
            if (head > 6) then
                Inc(cyl);
        end;
    end;
end;

procedure TU8418UtilForm.DiskExportBtnClick(Sender: TObject);
var
    cyl, head, rec, numRecs, count: Integer;
    fname: String;
    bfr: array [0..255] of Byte;
    fout: TFileStream;
begin
    if (DiskCylEdt.Text = '') then
        raise Exception.Create('Please enter the starting cylinder #');
    if (not TryStrToInt(DiskCylEdt.Text, cyl)) then
        raise Exception.Create('Invalid cylinder #');
    if (DiskHeadEdt.Text = '') then
        raise Exception.Create('Please enter the starting head #');
    if (not TryStrToInt(DiskHeadEdt.Text, head)) then
        raise Exception.Create('Invalid head #');
    if (DiskRecEdt.Text = '') then
        raise Exception.Create('Please enter the starting record #');
    if (not TryStrToInt(DiskRecEdt.Text, rec)) then
        raise Exception.Create('Invalid record #');
    if (DiskNumRecsEdt.Text = '') then
        raise Exception.Create('Please enter the # of records');
    if (not TryStrToInt(DiskNumRecsEdt.Text, numRecs)) then
        raise Exception.Create('Invalid # of records');

    if (not OpenDlg.Execute) then
        Exit;
    fname := OpenDlg.FileName;

    if (not Assigned(FDisk)) then
        OpenDisk;

    fout := TFileStream.Create(fname, fmCreate);
    try
        for count := 1 to numRecs do
        begin
            FDisk.ReadSector(cyl, head, rec, PByte(@bfr));
            fout.Write(bfr, 256);
            Inc(rec);
            if (rec > 40) then
            begin
                Inc(head);
                if (head > 6) then
                    Inc(cyl);
            end;
        end;
    finally
        fout.Free;
    end;
end;

function TU8418UtilForm.DumpBlock(recNo: Integer; bfr: array of Byte): String;
var
    count, lineStart: Integer;
    hex, asc, txt: String;
    a: AnsiChar;
begin
    txt := Format('** Record # %d'#13#10, [recNo]);
    lineStart := 0;
    count := 0;
    hex := '';
    asc := '';
    while (count <= 255) do
    begin
        if (hex <> '') then
        begin
            if ((count mod 4) = 0) then
            begin
                hex := hex + ' ';
                asc := asc + ' ';
            end;
            if ((count mod 16) = 0) then
            begin
                hex := Format('%4.4x %s', [lineStart, hex]);
                asc := '     ' + asc;
                txt := txt + hex + #13#10 + asc +#13#10;
                hex := '';
                asc := '';
                lineStart := count;
            end;
        end;
        hex := Format('%s%2.2x', [hex, bfr[count]]);
        a := TCodeTranslator.EbcdicToAscii(bfr[count]);
        if ((a < ' ') or (a > 'Z')) then
            a := ' ';
        asc := Format('%s %s', [asc, a]);
        Inc(count);
    end;
    if (hex <> '') then
    begin
        hex := Format('%4.4x %s', [lineStart, hex]);
        asc := '     ' + asc;
        txt := txt + hex + #13#10 + asc +#13#10;
    end;
    Result := txt;
end;

procedure TU8418UtilForm.DumpLibsHdr;
var
    i, j: Integer;
    p: TPartition;
    pe: TPartitionExtent;
begin
    LibsMemo.Clear;
    LibsMemo.Text := LibsMemo.Text +
        Format('**** %s ****'#13#10, [FLibsFile.FileName]);
    for i := 0 to FLibsFile.PartitionCount - 1 do
    begin
        p := FLibsFile.Partitions[i];
        LibsMemo.Text := LibsMemo.Text +
            Format('Partition %d:'#13#10, [i]) +
            Format('  Block = %d Record = %d Lace = %d Adjust = %d'#13#10,
                   [p.BlockSize, p.RecordSize, p.LaceFactor, p.RotnAdjust]) +
            '  Start Length'#13#10;
        for j := 0 to p.ExtentCount - 1 do
        begin
            pe := p.Extents[j];
            LibsMemo.Text := LibsMemo.Text +
                Format('  %5d %7d'#13#10, [pe.FirstTrack, pe.NumTracks]);
        end;
    end;
    LibsMemo.Text := LibsMemo.Text + #13#10;
end;

procedure TU8418UtilForm.FormShow(Sender: TObject);
begin
    Pages.ActivePageIndex := 0;
end;

procedure TU8418UtilForm.InitLibsPage;
var
    vtoc: TVtoc;
    fil: TVtocFile;
begin
    if (not Assigned(FDisk)) then
        OpenDisk;

    LibsList.Items.Clear;
    vtoc := TVtoc.Create;
    try
        vtoc.Open(FDisk);
        fil := vtoc.FirstFile;
        while (fil.FileFound) do
        begin
            // A library is a 3 partition SAT file
            if ((fil.Fmt1.DL_FT1= $02) and (fil.Fmt1.DL_PC1 = 3)) then
                LibsList.Items.Add(TrimRight(String(fil.Fmt1.DL_KEY1)));
            fil := vtoc.NextFile;
        end;
    finally
        vtoc.Free;
    end;
end;

procedure TU8418UtilForm.LibsDirBtnClick(Sender: TObject);
var
    typ: String;
    dir: TLibsDirEntry;
begin
    if (not Assigned(FLibsFile)) then
        LibsOpenBtnClick(nil);

    LibsMemo.Text := '';

    dir := FLibsFile.FirstDir;
    while ((dir.EntryType <> 0) and (dir.EntryType <> $a1)) do
    begin
        case dir.EntryType of
          $04:  typ := 'ENTRY Name';
          $08:  typ := 'CSECT Name';
          $80:  typ := 'OBJECT';
          $90:  typ := 'Phase';
          $a0:  typ := 'GROUP Start';
          $a1:  typ := 'EOF';
          $a2:  typ := 'PROC Name';
          $a3:  typ := 'PROC';
          $a4:  typ := 'SOURCE';
          $a8:  typ := 'GROUP End';
          $b0:  typ := 'BLOCK';
          else  typ := 'Unknown';
        end;
        LibsMemo.Text := LibsMemo.Text +
                            Format('%s %-12.12s Block = %5d Record = %3d'#13#10,
                                   [dir.Name, typ, dir.BlockNum, dir.RecordNum]);
        dir := FLibsFile.NextDir;
    end;
end;

procedure TU8418UtilForm.LibsDumpBtnClick(Sender: TObject);
var
    block, part, rec: Integer;
    bfr: array [0..255] of Byte;
    txt: String;
begin
    if (not TryStrToInt(LibsPartEdt.Text, part)) then
        raise Exception.Create('Please enter a partition #');
    if (not TryStrToInt(LibsTrackEdt.Text, block)) then
        raise Exception.Create('Please enter a block #');

    if (not Assigned(FLibsFile)) then
        LibsOpenBtnClick(nil);

    DumpLibsHdr;

    for rec := 1 to FDisk.MaxSector do
    begin
        FLibsFile.ReadBlock(part, block, @bfr);
        Inc(block);
        txt := DumpBlock(rec, bfr);
        LibsMemo.Text := LibsMemo.Text + txt + #13#10;
    end;

end;

procedure TU8418UtilForm.LibsExportBtnClick(Sender: TObject);
var
    listIdx: Integer;
    dir: TLibsDirEntry;

    procedure ExportSource;
    var
        fname: String;
        fil: TFileStream;
        s: AnsiString;
    begin
        if (TrimRight(String(dir.Name)) = 'CU$3REL') then
            { TODO : It looks like CU$3REL contains embedded object code. }
            Exit;
        LibsMemo.Lines.Add('   ' + String(dir.Name));
        fname := Format('c:\tmp\%s', [LibsList.Items[listIdx]]);
        ForceDirectories(fname);
        fname := Format('%s\%s.txt', [fname, TrimRight(String(dir.Name))]);
        fil := TFileStream.Create(fname, fmCreate);
        try
            FLibsFile.OpenElement(String(dir.Name), dir.EntryType);
            s := FLibsFile.ReadSource;
            while (not FLibsFile.Eof) do
            begin
                s := s + #13#10;
                fil.Write(PAnsiChar(s)^, Length(s));
                s := FLibsFile.ReadSource;
            end;
        finally
            fil.Free;
        end;
    end;

begin
    LibsMemo.Clear;
    for listIdx := 0 to LibsList.Items.Count - 1 do
    begin
        if (LibsList.Items[listIdx] <> '$Y$OBJ') then
        begin
            { TODO : There is a problem with $Y$OBJ that I haven't figured out yet. }
            FreeAndNil(FLibsFile);
            FLibsFile := TLibsFile.Create(FDisk, LibsList.Items[listIdx]);
            FLibsFile.Open;
            LibsMemo.Lines.Add('** ' + LibsList.Items[listIdx]);
            dir := FLibsFile.FirstDir;
            while ((dir.EntryType <> 0) and (dir.EntryType <> $a1)) do
            begin
                if ((dir.EntryType = $a3) or (dir.EntryType = $a4)) then
                    ExportSource;
                dir := FLibsFile.NextDir;
            end;
        end;
    end;

end;

procedure TU8418UtilForm.LibsOpenBtnClick(Sender: TObject);
begin
    if (LibsList.ItemIndex = -1) then
        raise Exception.Create('Please select a library');

    FreeAndNil(FLibsFile);
    FLibsFile := TLibsFile.Create(FDisk, LibsList.Items[LibsList.ItemIndex]);
    FLibsFile.Open;
    DumpLibsHdr;
end;

procedure TU8418UtilForm.LibsSourceBtnClick(Sender: TObject);
var
    src: AnsiString;
begin
    if (not Assigned(FLibsFile)) then
        LibsOpenBtnClick(nil);

    LibsSourceEdt.Text := Trim(LibsSourceEdt.Text);
    if (LibsSourceEdt.Text = '') then
        raise Exception.Create('Please enter a source element name');

    LibsMemo.Clear;
    try
        FLibsFile.OpenElement(LibsSourceEdt.Text, $a4);         // try SOURCE
    except
        FLibsFile.OpenElement(LibsSourceEdt.Text, $a3);         // try PROC
    end;
    while (not FLibsFile.Eof) do
    begin
        src := FLibsFile.ReadSource;
        if (not FLibsFile.Eof) then
            LibsMemo.Text := LibsMemo.Text + String(src) + #13#10;
    end;
end;

procedure TU8418UtilForm.ListVTOCBtnClick(Sender: TObject);
var
    vtoc: TVtoc;
    cyl, head, rec, y, d, i, j: Integer;
    lowCyl, lowHead, lowRec: Integer;
    highCyl, highHead: Integer;
    ext, trk, numTrk: Integer;
    stemp: String;
    fil: TVtocFile;
begin
    if (not Assigned(FDisk)) then
        OpenDisk;

    vtoc := TVtoc.Create;
    try
        vtoc.Open(FDisk);

        DecodeVtocCCHHR(vtoc.Vol1.DL_VTC, cyl, head, rec);
        VTOCMemo.Text := Format('**** VTOC for volume %s ****'#13#10#13#10, [String(vtoc.Vol1.DL_VSN)]) +
                         Format('  Format 4 label @ %d, %d, %d'#13#10, [cyl, head, rec]);

        DecodeVtocCCHHR(vtoc.Fmt4.DL_LF4, cyl, head, rec);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    Last format 1 @ %d, %d, %d'#13#10, [cyl, head, rec]) +
                         Format('    Unused VTOC records %d'#13#10, [vtoc.Fmt4.DL_AF4]);
        DecodeVtocCCHH(vtoc.Fmt4.DL_HA4, cyl, head);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    Highest alt. track %d, %d'#13#10, [cyl, head]) +
                         Format('    # alt. tracks %d'#13#10, [vtoc.Fmt4.DL_AT4]);
        DecodeVtocCCHH(vtoc.Fmt4.DL_DS4, cyl, head);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    Device size %d, %d'#13#10, [cyl, head]) +
                         Format('    Track length %d'#13#10, [vtoc.Fmt4.DL_tl4]) +
                         Format('    Labels / track %d'#13#10, [vtoc.Fmt4.DL_LT4]) +
                         Format('    Blocks / track %d'#13#10, [vtoc.Fmt4.DL_BK4]);
        DecodeVtocCCHHR(vtoc.Fmt4.DL_F04, cyl, head, rec);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    Format 0 addr. %d, %d, %d'#13#10, [cyl, head, rec]);
        DecodeVtocCCHHR(vtoc.Fmt4.DL_F64, cyl, head, rec);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    Format 6 addr. %d, %d, %d'#13#10, [cyl, head, rec]) +
                         Format('    VTOC Extent type %d'#13#10, [vtoc.Fmt4.DL_XT4]) +
                         Format('    VTOC Extent seq. %d'#13#10, [vtoc.Fmt4.DL_XS4]);
        DecodeVtocCCHH(vtoc.Fmt4.DL_XL4, lowCyl, lowHead);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    VTOC Extent lower %d, %d'#13#10, [lowCyl, lowHead]);
        DecodeVtocCCHH(vtoc.Fmt4.DL_XU4, highCyl, highHead);
        VTOCMemo.Text := VTOCMemo.Text +
                         Format('    VTOC Extent upper %d, %d'#13#10, [highCyl, highHead]);

        fil := vtoc.FirstFile;
        while (fil.FileFound) do
        begin
            VTOCMemo.Text := VTOCMemo.Text +
                             Format(#13#10'%s on %s(%d)'#13#10, [String(fil.Fmt1.DL_KEY1),
                                                                 String(fil.Fmt1.DL_FS1),
                                                                 fil.Fmt1.DL_VS1]);
            DecodeVtocDate(fil.Fmt1.DL_CD1, y, d);
            VTOCMemo.Text := VTOCMemo.Text +
                             Format('  Creation date %2.2d%3.3d'#13#10, [y, d]);
            DecodeVtocDate(fil.Fmt1.DL_ED1, y, d);
            VTOCMemo.Text := VTOCMemo.Text +
                             Format('  Expiration date %2.2d%3.3d'#13#10, [y, d]);
            case fil.Fmt1.DL_FT1 of
              $20:  stemp := 'Sequential';
              $40:  stemp := 'Direct';
              $60:  stemp := 'Non indexed';
              $80:  stemp := 'Indexed sequential';
              $90:  stemp := 'MIRAM';
              $02:  stemp := 'SAT';
              else  stemp := 'Undefined';
            end;
            VTOCMemo.Text := VTOCMemo.Text +
                             Format('  File type %s'#13#10, [stemp]);

            VTOCMemo.Text := VTOCMemo.Text + #13#10;
            stemp := 'Extents';
            for i := 1 to 3 do
            begin
                if (fil.Fmt1.DL_XT1[i] <> 0) then
                begin
                    DecodeVtocCCHH(fil.Fmt1.DL_XL1[i], lowCyl, lowHead);
                    DecodeVtocCCHH(fil.Fmt1.DL_XU1[i], highCyl, highHead);
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format('  %-8.8s Start %d, %d End %d, %d'#13#10,
                                            [stemp, lowCyl, lowHead, highCyl, highHead]);
                    stemp := '';
                end;
            end;
            for i := 1 to 13 do
            begin
                if (fil.Fmt3.DL_XT3[i] <> 0) then
                begin
                    DecodeVtocCCHH(fil.Fmt3.DL_XL3[i], lowCyl, lowHead);
                    DecodeVtocCCHH(fil.Fmt3.DL_XU3[i], highCyl, highHead);
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format('  %-8.8s Start %d, %d End %d, %d'#13#10,
                                            [stemp, lowCyl, lowHead, highCyl, highHead]);
                end;
            end;

            VTOCMemo.Text := VTOCMemo.Text + #13#10;
            stemp := 'Partitions';
            for i := 1 to fil.Fmt1.DL_PC1 do
            begin
                stemp := Format('  %-12.12s %d: BLen %d RLen %d',
                                [stemp, i, fil.Fmt1.DL_BL1[i], fil.Fmt1.DL_RL1[i]]);
                for j := 1 to 40 do
                begin
                    DecodeVtocPExtent(fil.Fmt2.DL_SXAR2[J], ext, trk, numTrk);
                    if (ext = i) then
                    begin
                        VTOCMemo.Text := VTOCMemo.Text +
                                         Format('%s 1st track %d Num tracks %d'#13#10,
                                                [stemp, trk, numTrk]);
                        stemp := StringOfChar(' ', Length(stemp));
                    end;
                end;
                if (Trim(stemp) <> '') then
                    VTOCMemo.Text := VTOCMemo.Text + stemp + #13#10;
                stemp := '';
            end;

            if (fil.Fmt1.DL_FT1 = $02) then
            begin
                VTOCMemo.Text := VTOCMemo.Text +
                                 #13#10'  Library File Information'#13#10 +
                                 Format('    Dir Lace = %d Dir Lace Adjust = %d'#13#10,
                                        [fil.Fmt2.DL_DIRL2, fil.Fmt2.DL_DIRF2]) +
                                 Format('    Txt Lace = %d Txt Lace Adjust = %d'#13#10,
                                        [fil.Fmt2.DL_TXTL2, fil.Fmt2.DL_TXTF2]);
            end;


            VTOCMemo.Text := VTOCMemo.Text + #13#10;
            DecodeVtocCCHHR(fil.Fmt1.DL_CP1, lowCyl, lowHead, lowRec);
            VTOCMemo.Text := VTOCMemo.Text +
                             Format('  Format 2 addr %d, %d, %d'#13#10, [lowCyl, lowHead, lowRec]);
            fil := vtoc.NextFile;
        end;
    finally
        vtoc.Free;
    end;
end;

procedure TU8418UtilForm.OpenBtnClick(Sender: TObject);
begin
    OpenDisk;
end;

procedure TU8418UtilForm.OpenDisk;
begin
    FreeAndNil(FDisk);
    if (ExtractFileExt(OpenDlg.FileName) = '.8416') then
        FDisk := T8416Disk.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone)
    else
        FDisk := T8418Disk.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
end;

procedure TU8418UtilForm.PagesChange(Sender: TObject);
begin
    if (Pages.ActivePage = LibsPage) then
        InitLibsPage;
end;

procedure TU8418UtilForm.BrowseBtnClick(Sender: TObject);
begin
    if (not OpenDlg.Execute) then
        Exit;

    FileNameEdt.Text := OpenDlg.FileName;
    CloseDisk;
end;

end.
