unit U8418UtilFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, U8418;

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
    procedure BrowseBtnClick(Sender: TObject);
    procedure ListVTOCBtnClick(Sender: TObject);
    procedure OpenBtnClick(Sender: TObject);
  private
    FDisk: T8418File;
  public
    { Public declarations }
  end;

var
  U8418UtilForm: TU8418UtilForm;

implementation

uses VTOC;

{$R *.dfm}

procedure TU8418UtilForm.ListVTOCBtnClick(Sender: TObject);
var
    bfr: array [0..255] of Byte;
    vol1: PVtocVol1;
    fmt1: PVtocFmt1;
    fmt4: PVtocFmt4;
    cyl, trk, sec, y, d, i: Integer;
    lowCyl, lowTrk, lowSec: Integer;
    highCyl, highTrk: Integer;
    stemp: String;
begin
    if (not Assigned(FDisk)) then
        raise Exception.Create('Please open a disk file');
    // Read VTOC VOL1 record from cyl 0, trk 0, sec 2
    FDisk.ReadSector(0, 0, 3, @bfr);
    vol1 := PVtocVol1(@bfr);
    if (vol1.DL_VL <> 'VOL1') then
        raise Exception.Create('VOL1 label not found');
    DecodeVtocAddr(vol1.DL_VTC, cyl, trk, sec);
    VTOCMemo.Text := Format('**** VTOC for volume %s ****'#13#10#13#10, [String(vol1.DL_VSN)]) +
                     Format('  Format 4 label @ %d, %d, %d'#13#10, [cyl, trk, sec]);
    // Read the first format 4 label
    FDisk.ReadSector(cyl, trk, sec, @bfr);
    fmt4 := PVtocFmt4(@bfr);
    if (fmt4.DL_ID4 <> '4') then
        raise Exception.Create('FMT4 label not found');
    DecodeVtocAddr(fmt4.DL_LF4, cyl, trk, sec);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    Last format 1 @ %d, %d, %d'#13#10, [cyl, trk, sec]) +
                     Format('    Unused VTOC records %d'#13#10, [fmt4.DL_AF4]);
    DecodeVtocCCHH(fmt4.DL_HA4, cyl, trk);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    Highest alt. track %d, %d'#13#10, [cyl, trk]) +
                     Format('    # alt. tracks %d'#13#10, [fmt4.DL_AT4]);
    DecodeVtocCCHH(fmt4.DL_DS4, cyl, trk);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    Device size %d, %d'#13#10, [cyl, trk]) +
                     Format('    Track length %d'#13#10, [fmt4.DL_tl4]) +
                     Format('    Labels / track %d'#13#10, [fmt4.DL_LT4]) +
                     Format('    Blocks / track %d'#13#10, [fmt4.DL_BK4]);
    DecodeVtocAddr(fmt4.DL_F04, cyl, trk, sec);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    Format 0 addr. %d, %d, %d'#13#10, [cyl, trk, sec]);
    DecodeVtocAddr(fmt4.DL_F64, cyl, trk, sec);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    Format 6 addr. %d, %d, %d'#13#10, [cyl, trk, sec]) +
                     Format('    VTOC Extent type %d'#13#10, [fmt4.DL_XT4]) +
                     Format('    VTOC Extent seq. %d'#13#10, [fmt4.DL_XS4]);
    DecodeVtocCCHH(fmt4.DL_XL4, lowCyl, lowTrk);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    VTOC Extent lower %d, %d'#13#10, [lowCyl, lowTrk]);
    DecodeVtocCCHH(fmt4.DL_XU4, highCyl, highTrk);
    VTOCMemo.Text := VTOCMemo.Text +
                     Format('    VTOC Extent upper %d, %d'#13#10, [highCyl, highTrk]);
    // Read all fmt1 labels to display list of file names
    fmt1 := PVtocFmt1(@bfr);
    for cyl := lowCyl to highCyl do
        for trk := lowTrk to highTrk do
            for sec := 1 to FDisk.MaxSector do
            begin
                FDisk.ReadSector(cyl, trk, sec, @bfr);
                if (fmt1.DL_ID1 = '1') then
                begin
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format(#13#10'%s on %s(%d)'#13#10, [String(fmt1.DL_KEY1),
                                                                   String(fmt1.DL_FS1),
                                                                   fmt1.DL_VS1]);
                    DecodeVtocDate(fmt1.DL_CD1, y, d);
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format('  Creation date %2.2d%3.3d'#13#10, [y, d]);
                    DecodeVtocDate(fmt1.DL_ED1, y, d);
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format('  Expiration date %2.2d%3.3d'#13#10, [y, d]);
                    case fmt1.DL_FT1 of
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

                    stemp := 'Partitions';
                    for i := 1 to fmt1.DL_PC1 do
                    begin
                        VTOCMemo.Text := VTOCMemo.Text +
                                         Format('  %-12.12s Block length %d Record length %d'#13#10,
                                                [stemp, fmt1.DL_BL1[i], fmt1.DL_RL1[i]]);
                        stemp := '';
                    end;

                    stemp := 'Extents';
                    for i := 1 to 3 do
                    begin
                        if (fmt1.DL_XT1[i] <> 0) then
                        begin
                            DecodeVtocCCHH(fmt1.DL_XL1[i], lowCyl, lowTrk);
                            DecodeVtocCCHH(fmt1.DL_XU1[i], highCyl, highTrk);
                            VTOCMemo.Text := VTOCMemo.Text +
                                             Format('  %-8.8s Start %d, %d End %d, %d'#13#10,
                                                    [stemp, lowCyl, lowTrk, highCyl, highTrk]);
                            stemp := '';
                        end;
                    end;
                    { TODO :
Need to process format 3 records here to be able to list more than
3 extents. }

                    DecodeVtocAddr(fmt1.DL_CP1, lowCyl, lowTrk, lowSec);
                    VTOCMemo.Text := VTOCMemo.Text +
                                     Format('  Format 2 addr %d, %d, %d'#13#10, [lowCyl, lowTrk, lowSec]);
                end;
            end;
end;

procedure TU8418UtilForm.OpenBtnClick(Sender: TObject);
begin
    FreeAndNil(FDisk);
    if (ExtractFileExt(OpenDlg.FileName) = '.8416') then
        FDisk := T8416File.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone)
    else
        FDisk := T8418File.Create(FileNameEdt.Text, fmOpenRead or fmShareDenyNone);
end;

procedure TU8418UtilForm.BrowseBtnClick(Sender: TObject);
begin
    if (not OpenDlg.Execute) then
        Exit;

    FileNameEdt.Text := OpenDlg.FileName;
end;

end.
