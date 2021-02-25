unit DmpRstFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, EmulatorTypes, U8418;

type
  TDmpRstForm = class(TForm)
    FileNameEdt: TLabeledEdit;
    DumpBtn: TButton;
    DataMemo: TMemo;
    HdrList: TListBox;
    RestoreBtn: TButton;
    OpenDlg: TOpenDialog;
    procedure DumpBtnClick(Sender: TObject);
    procedure HdrListClick(Sender: TObject);
    procedure RestoreBtnClick(Sender: TObject);
  private
    FEof: Boolean;
    FTape: TFileStream;
    FDisk: T8418File;
    procedure DumpBlock(blen: UInt32; blkNum: UInt32);
    function ReadLength: UInt32;
    procedure ReadLengths(blkNum: UInt32; var prevLen, nextLen: UInt32);
    procedure RestoreBlock(tlen: UInt32);
  public
    { Public declarations }
  end;

var
  DmpRstForm: TDmpRstForm;

implementation

{$R *.dfm}

type
  TDmpRstHeader = packed record
  private
    FBuffer: array [0..49] of Byte;
    function GetBlockLength: Uint32;
    function GetSectors: UInt32;
    function GetTrack: UInt32;
    function GetCylinder: UInt32;

  public
    property BlockLength: UInt32 read GetBlockLength;   // Bytes 0-1 = block length (may span multiple tape blocks)
    property Sectors: UInt32 read GetSectors;           // Byte 7 = # disk sectors this block
    property Track: UInt32 read GetTrack;               // Byte 10 = track #
    property Cylinder: UInt32 read GetCylinder;         // Byte 12-13 = cylinder #
  end;

  PDmpRstHeader = ^TDmpRstHeader;

procedure TDmpRstForm.DumpBlock(blen: UInt32; blkNum: UInt32);
var
    hex, asc, txt: String;
    a: AnsiChar;
    count, lineStart: Integer;
    bfr: PByte;
    bytesRead: Cardinal;
    hdr: PDmpRstHeader;
begin
    DataMemo.Text := '';
    if (blen = 0) then
        Exit;
    count := 0;
    lineStart := 0;
    hex := '';
    asc:= '';
    GetMem(bfr, blen);
    try
        bytesRead := FTape.Read(bfr^, blen);
        if (bytesRead <> blen) then
        begin
            HdrList.Items.Add(Format('Block size mismatch. Expected %d got %d', [blen, bytesRead]));
            Exit;
        end;
        hdr := PDmpRstHeader(bfr);
        txt := Format('----- Block %d - Length %d - Sectors %d - Track %d - Cylinder %d -----'#13#10,
                      [blkNum, hdr.BlockLength, hdr.Sectors, hdr.Track, hdr.Cylinder]);
        while (count < blen) do
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
            hex := Format('%s%2.2x', [hex, (bfr + count)^]);
            a := TCodeTranslator.EbcdicToAscii((bfr + count)^);
            if (a = #0) then
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
        DataMemo.Text := DataMemo.Text + txt;
    finally
        FreeMem(bfr);
    end;
end;

procedure TDmpRstForm.HdrListClick(Sender: TObject);
var
    blkNum, count, nextBlkLen, prevBlkLen : UInt32;
begin
    blkNum := HdrList.ItemIndex;
    FEof := False;
    FTape.Position := 0;
    count := 0;
    while (count < blkNum) do
    begin
        ReadLengths(count, prevBlkLen, nextBlkLen);
        if (FEof) then
            Exit;
        FTape.Position := FTape.Position + nextBlkLen;
        Inc(count);
    end;
    ReadLengths(blkNum, prevBlkLen, nextBlkLen);
    DumpBlock(nextBlkLen, blkNum);
end;

function TDmpRstForm.ReadLength: UInt32;
var
    bytes4: array [1..4] of Byte;
begin
    if (FTape.Read(bytes4, 4) = 4) then
    begin
        Result := (bytes4[4] shl 24) or
                  (bytes4[3] shl 16) or
                  (bytes4[2] shl 8) or
                  bytes4[1];
    end else
    begin
        FEof := True;
        Result := 0;
    end;
end;

procedure TDmpRstForm.ReadLengths(blkNum: UInt32; var prevLen, nextLen: UInt32);
begin
    if (blkNum = 0) then
    begin
        prevLen := 0;
        nextLen := ReadLength;
    end else
    begin
        prevLen := ReadLength;
        nextLen := ReadLength;
    end;
end;

procedure TDmpRstForm.RestoreBlock(tlen: UInt32);
var
    bfr: PByte;
    plen: UInt32;
    bytesRead, doffset: Cardinal;
    blen, boffset: Integer;
    hdr: TDmpRstHeader;
begin
    // Read block header
    bytesRead := FTape.Read(hdr, SizeOf(hdr));
    if (bytesRead <> SizeOf(hdr)) then
        raise Exception.Create('RestoreBlock: Tape error reading block header');

    blen := hdr.BlockLength - SizeOf(hdr);
    Dec(tlen, SizeOf(hdr));
    GetMem(bfr, blen);
    try
        boffset := 0;
        // Read remainder of block from tape
        repeat
            bytesRead := FTape.Read((bfr + boffset)^, tlen);
            if (bytesRead <> tlen) then
                raise Exception.Create('RestoreBlock: Tape error reading block data');
            Inc(boffset, tlen);
            if (boffset < blen) then
                ReadLengths(1, plen, tlen);
            if ((tlen = 0) or FEof) then
                raise Exception.Create('RestoreBlock: Tape error reading block data');
        until boffset >= blen;
        // Write to simulated disk
        doffset := ((hdr.Cylinder * FDisk.MaxTrack) + hdr.Track) * FDisk.MaxSector * FDisk.SectorSize;
        FDisk.Position := doffset;
        FDisk.Write(bfr^, hdr.Sectors * FDisk.SectorSize);
    finally
        FreeMem(bfr);
    end;
end;

procedure TDmpRstForm.RestoreBtnClick(Sender: TObject);
var
    nextBlkLen, prevBlkLen, blkNum: UInt32;
    i, stat: Integer;
begin
    if (not OpenDlg.Execute) then
        Exit;

    if (FileExists(OpenDlg.FileName)) then
    begin
        stat := MessageBox(Handle, 'File exists. Overwrite?', '', MB_YESNO or MB_APPLMODAL);
        if (stat <> IDYES) then
            Exit;
    end;

    if (ExtractFileExt(OpenDlg.FileName) = '.8416') then
        FDisk := T8416File.Create(OpenDlg.FileName, fmCreate)
    else
        FDisk := T8418File.Create(OpenDlg.FileName, fmCreate);
    try
        // Create an empty disk
        FDisk.Format;
        // Load the tape file onto the disk
        blkNum := 0;
        FEof := False;
        FreeAndNil(FTape);
        FTape := TFileStream.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
        for i := 1 to 2 do
        begin
            // Skip tape labels
            ReadLengths(blkNum, prevBlkLen, nextBlkLen);
            FTape.Position := FTape.Position + nextBlkLen;
            Inc(blkNum);
        end;
        while (not FEof) do
        begin
            ReadLengths(blkNum, prevBlkLen, nextBlkLen);
            if (FEof or (nextBlkLen = 0)) then
            begin
                HdrList.Items.Add('**** EOF ****');
                Break;
            end;

            HdrList.Items.Add(Format('Block %d PrevLen = %d NextLen = %d', [blkNum, prevBlkLen, nextBlkLen]));
            RestoreBlock(nextBlkLen);
        end;
    finally
        FDisk.Free;
    end;
end;

procedure TDmpRstForm.DumpBtnClick(Sender: TObject);
var
    nextBlkLen, prevBlkLen, blkNum: UInt32;
begin
    blkNum := 0;
    FEof := False;
    FreeAndNil(FTape);
    FTape := TFileStream.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
    while (not FEof) do
    begin
        ReadLengths(blkNum, prevBlkLen, nextBlkLen);
        if (FEof or (nextBlkLen = 0)) then
        begin
            HdrList.Items.Add('**** EOF ****');
            Break;
        end;

        HdrList.Items.Add(Format('Block %d PrevLen = %d NextLen = %d', [blkNum, prevBlkLen, nextBlkLen]));
        FTape.Position := FTape.Position + nextBlkLen;
        Inc(blkNum);
    end;
end;

{ TDmpRstHeader }

function TDmpRstHeader.GetBlockLength: Uint32;
begin
    Result := (FBuffer[0] shl 8) or FBuffer[1];
end;

function TDmpRstHeader.GetCylinder: UInt32;
begin
    Result := (FBuffer[12] shl 8) or FBuffer[13];
end;

function TDmpRstHeader.GetSectors: UInt32;
begin
    Result := FBuffer[7];
end;

function TDmpRstHeader.GetTrack: UInt32;
begin
    Result := FBuffer[10];
end;

end.
