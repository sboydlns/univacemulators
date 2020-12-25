unit U9200CardViewFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids,
  CardFile, U9200Files;

type
  TDataType = ( dtUnknown, dtObj, dtExe, dtMacro );

  TU9200CardViewForm = class(TForm)
    FileNamePanel: TPanel;
    Label1: TLabel;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    OpenDlg: TOpenDialog;
    ShowBtn: TButton;
    CancelBtn: TButton;
    Pages: TPageControl;
    ObjFilePage: TTabSheet;
    ExeFilePage: TTabSheet;
    ObjAGrid: TStringGrid;
    ObjHGrid: TStringGrid;
    ObjJGrid: TStringGrid;
    ObjKGrid: TStringGrid;
    ObjQGrid: TStringGrid;
    ObjYGrid: TStringGrid;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ExeQGrid: TStringGrid;
    ExeYGrid: TStringGrid;
    MacroFilePage: TTabSheet;
    MacroBGrid: TStringGrid;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    MacroCGrid: TStringGrid;
    MacroZGrid: TStringGrid;
    UnknownFilePage: TTabSheet;
    UnknownGrid: TStringGrid;
    FileTypeBtn: TRadioGroup;
    SaveBtn: TButton;
    SaveDlg: TSaveDialog;
    procedure FormShow(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure BrowseBtnClick(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FFileName: String;
    procedure ClearGrid(grid: TStringGrid);
    function GetFileType(fname: String): TDataType;
    procedure HideAllPages;
    procedure Show1005File(fname: String);
    procedure Show494File(fname: String);
    procedure ShowARec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowBRec(fin: TU92MacroFile; grid: TStringGrid);
    procedure ShowCRec(fin: TU92MacroFile; grid: TStringGrid);
    procedure ShowHRec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowJRec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowKRec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowQRec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowYRec(fin: TU92ObjectFile; grid: TStringGrid);
    procedure ShowZRec(fin: TU92MacroFile; grid: TStringGrid);
    procedure ShowExeFile(fname: String);
    procedure ShowMacroFile(fname: String);
    procedure ShowObjectFile(fname: String);
    procedure ShowUnknownFile(fname: String);
    procedure Split(grid: TStringGrid);
  public
    { Public declarations }
  end;

var
  U9200CardViewForm: TU9200CardViewForm;

implementation

uses EmulatorTypes, U9200Types;

{$R *.dfm}

procedure TU9200CardViewForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9200CardViewForm.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TU9200CardViewForm.ClearGrid(grid: TStringGrid);
var
    i, j: Integer;
begin
    for i := 1 to grid.RowCount - 1 do
        for j := 1 to grid.ColCount - 1 do
            grid.Cells[j, i] := '';
    grid.RowCount := 2;
end;

procedure TU9200CardViewForm.FormShow(Sender: TObject);
begin
    HideAllPages;
    OpenDlg.InitialDir := CardFileDir;
    with ObjAGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Abs';
        Cells[5, 0] := 'Cksum';
        Cells[6, 0] := 'ESID';
        Cells[7, 0] := 'Start';
        Cells[8, 0] := 'Name';
        Cells[9, 0] := 'CLen';
    end;
    with ObjHGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Cksum';
        Cells[5, 0] := 'RLD Len';
        Cells[6, 0] := 'Last RLD';
        Cells[7, 0] := 'Start';
        Cells[8, 0] := 'Name';
        Cells[9, 0] := 'RLD';
    end;
    with ObjJGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Cksum';
        Cells[5, 0] := 'ESID';
        Cells[6, 0] := 'Start';
        Cells[7, 0] := 'Name';
    end;
    with ObjKGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Cksum';
        Cells[5, 0] := 'ESID';
        Cells[6, 0] := 'Name';
    end;
    with ObjQGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Load';
        Cells[5, 0] := 'Cksum';
        Cells[6, 0] := 'ESID';
        Cells[7, 0] := 'RLD Len';
        Cells[8, 0] := 'Text';
        Cells[9, 0] := 'RLD'
    end;
    with ObjYGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Cksum';
        Cells[5, 0] := 'RLD Len';
        Cells[6, 0] := 'Last RLD';
        Cells[7, 0] := 'Card Cnt';
        Cells[8, 0] := 'Start';
        Cells[9, 0] := 'RLD'
    end;
    with ExeQGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Load';
        Cells[5, 0] := 'Cksum';
        Cells[6, 0] := 'ESID';
        Cells[7, 0] := 'RLD Len';
        Cells[8, 0] := 'Text';
        Cells[9, 0] := 'RLD'
    end;
    with ExeYGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Len';
        Cells[4, 0] := 'Cksum';
        Cells[5, 0] := 'RLD Len';
        Cells[6, 0] := 'Last RLD';
        Cells[7, 0] := 'Card Cnt';
        Cells[8, 0] := 'Start';
        Cells[9, 0] := 'RLD'
    end;
    with MacroBGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Cksum';
        Cells[4, 0] := 'Data Len';
        Cells[5, 0] := 'Data';
    end;
    with MacroCGrid do
    begin
        ColWidths[0] := 30;
        Cells[1, 0] := 'Key';
        Cells[2, 0] := 'Type';
        Cells[3, 0] := 'Cksum';
        Cells[4, 0] := 'Count';
        Cells[5, 0] := 'Data';
    end;
    with UnknownGrid do
    begin
        ColWidths[0] := 30;
    end;
end;

function TU9200CardViewForm.GetFileType(fname: String): TDataType;
var
    fin: TU92ObjectFile;
begin
    Result := dtUnknown;
    fin := TU92ObjectFile.Create(fname, fmOpenRead);
    try
        fin.Read;
        if (fin.Key = $d1) then
        begin
            if (fin.CardType = 'A') then
                Result := dtObj
            else if (fin.CardType = 'Q') then
                Result := dtExe;
        end else if (fin.Key = $91) then
            Result := dtMacro;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.HideAllPages;
var
    i: Integer;
begin
    for i := 0 to Pages.PageCount - 1 do
    begin
        Pages.Pages[i].Visible := False;
        Pages.Pages[i].TabVisible := False;
    end;
    SaveBtn.Enabled := False;
end;

procedure TU9200CardViewForm.SaveBtnClick(Sender: TObject);
var
    sel: TGridRect;
    fin: TCardFileStream;
    fout: TCardFileStream;
    start: Integer;
    endd: Integer;
    rec: TCardRec;
begin
    sel := UnknownGrid.Selection;
    if (sel.Top < 1) then
        Exit;
    if (not SaveDlg.Execute) then
        Exit;

    start := sel.Top;
    endd := sel.Bottom;
    fin := TCardFileStream.Create(FFileName, fmOpenRead);
    fout := TCardFileStream.Create(SaveDlg.FileName, fmCreate);
    try
        // Position input file to first record to be saved
        while ((not fin.Eof) and (fin.RecNumber < start)) do
            fin.ReadTranslate(rec);
        // Save the selected records
        repeat
            fout.WriteTranslate(rec);
        until ((fin.ReadTranslate(rec) = 0) or (fin.RecNumber > endd));
    finally
        fin.Free;
        fout.Free;
    end;
end;

procedure TU9200CardViewForm.Show1005File(fname: String);
var
    fin: TCardFileStream;
    bfr: TCardRec;
    i: Integer;
    stemp: AnsiString;
begin
    HideAllPages;
    UnknownFilePage.Visible := True;
    UnknownFilePage.TabVisible := True;
    SaveBtn.Enabled := True;
    ClearGrid(UnknownGrid);
    fin := TCardFileStream.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.ReadXS3(bfr);
            stemp := '';
            for i := 1 to bfr.Count do
                stemp := stemp + TCodeTranslator.XS3ToAscii(bfr.Columns[i]);
            UnknownGrid.RowCount := UnknownGrid.RowCount + 1;
            UnknownGrid.Cells[0, UnknownGrid.RowCount - 2] := IntToStr(fin.RecNumber);
            UnknownGrid.Cells[1, UnknownGrid.RowCount - 2] := String(stemp);
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.Show494File(fname: String);
var
    fin: TCardFileStream;
    bfr: TCardRec;
    i: Integer;
    stemp: AnsiString;
begin
    HideAllPages;
    UnknownFilePage.Visible := True;
    UnknownFilePage.TabVisible := True;
    SaveBtn.Enabled := True;
    ClearGrid(UnknownGrid);
    fin := TCardFileStream.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.ReadFieldata(bfr);
            stemp := '';
            for i := 1 to bfr.Count do
                stemp := stemp + TCodeTranslator.FieldataToAscii(bfr.Columns[i]);
            UnknownGrid.RowCount := UnknownGrid.RowCount + 1;
            UnknownGrid.Cells[0, UnknownGrid.RowCount - 2] := IntToStr(fin.RecNumber);
            UnknownGrid.Cells[1, UnknownGrid.RowCount - 2] := String(stemp);
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.ShowARec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    if (fin.IsAbsolute) then
        grid.Cells[4, row] := 'Y'
    else
        grid.Cells[4, row] := 'N';
    grid.Cells[5, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[6, row] := Format('%d', [fin.ExtSymID]);
    grid.Cells[7, row] := Format('%4.4x', [fin.StartAddr]);
    grid.Cells[8, row] := String(fin.SymName);
    grid.Cells[9, row] := Format('%d', [fin.CodeLength]);
end;

procedure TU9200CardViewForm.ShowBRec(fin: TU92MacroFile; grid: TStringGrid);
var
    row: Integer;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[4, row] := Format('%d', [fin.DataLength]);
    grid.Cells[5, row] := String(Format('%s', [fin.Hex[5, fin.DataLength]]));
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    grid.Cells[5, row] := String(fin.Ebcdic[5, fin.DataLength]);
end;

procedure TU9200CardViewForm.ShowBtnClick(Sender: TObject);
var
    ft: TDataType;
begin
    FFileName := FileNameEdt.Text;
    if (FFileName = '') then
        raise Exception.Create('Please enter the file name');
    case FileTypeBtn.ItemIndex of
      0:                                        // auto
      begin
        ft := GetFileType(FileNameEdt.Text);
        case ft of
          dtUnknown:    ShowUnknownFile(FFileName);
          dtObj:        ShowObjectFile(FFileName);
          dtExe:        ShowExeFile(FFileName);
          dtMacro:      ShowMacroFile(FFileName);
        end;
      end;
      1:
      begin
        ShowObjectFile(FFileName);
      end;
      2:
      begin
        ShowExeFile(FFileName);
      end;
      3:
      begin
        ShowMacroFile(FFileName);
      end;
      4:
      begin
        Show1005File(FFileName);
      end;
      5:
      begin
        Show494File(FFileName);
      end;
      6:
      begin
        ShowUnknownFile(FFileName);
      end;
    end;
end;

procedure TU9200CardViewForm.ShowCRec(fin: TU92MacroFile; grid: TStringGrid);
var
    row: Integer;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[4, row] := Format('%d', [fin.DataLength]);
    grid.Cells[5, row] := String(Format('%s', [fin.Hex[5, fin.DataLength * 4]]));
end;

procedure TU9200CardViewForm.ShowExeFile(fname: String);
var
    fin: TU92ObjectFile;
    lastType: AnsiChar;
begin
    lastType := ' ';
    HideAllPages;
    ExeFilePage.Visible := True;
    ExeFilePage.TabVisible := True;
    SaveBtn.Enabled := False;
    ClearGrid(ExeQGrid);
    ClearGrid(ExeYGrid);
    fin := TU92ObjectFile.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.Read;
            if (fin.CardType < lastType) then
            begin
                Split(ExeQGrid);
                Split(ExeYGrid);
            end;
            lastType := fin.CardType;
            case fin.CardType of
              'Q':  ShowQRec(fin, ExeQGrid);
              'Y':  ShowYRec(fin, ExeYGrid);
            end;
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.ShowHRec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    grid.Cells[4, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[5, row] := Format('%d', [fin.RelDataLength]);
    grid.Cells[6, row] := Format('%d', [fin.LastRelData]);
    grid.Cells[7, row] := Format('%4.4x', [fin.SymAddr]);
    grid.Cells[8, row] := String(fin.SymName);
    grid.Cells[9, row] := Format('%6.6x', [fin.RelData]);
end;

procedure TU9200CardViewForm.ShowJRec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    grid.Cells[4, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[5, row] := Format('%d', [fin.ExtSymID]);
    grid.Cells[6, row] := Format('%4.4x', [fin.SymAddr]);
    grid.Cells[7, row] := String(fin.SymName);
end;

procedure TU9200CardViewForm.ShowKRec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    grid.Cells[4, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[5, row] := Format('%d', [fin.ExtSymID]);
    grid.Cells[6, row] := String(fin.SymName);
end;

procedure TU9200CardViewForm.ShowMacroFile(fname: String);
var
    fin: TU92MacroFile;
    lastType: AnsiChar;
begin
    lastType := ' ';
    HideAllPages;
    MacroFilePage.Visible := True;
    MacroFilePage.TabVisible := True;
    SaveBtn.Enabled := False;
    ClearGrid(MacroBGrid);
    ClearGrid(MacroCGrid);
    ClearGrid(MacroZGrid);
    fin := TU92MacroFile.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.Read;
            if (fin.CardType < lastType) then
            begin
                Split(MacroBGrid);
                Split(MacroCGrid);
                Split(MacroZGrid);
            end;
            lastType := fin.CardType;
            case fin.CardType of
              'B':  ShowBRec(fin, MacroBGrid);
              'C':  ShowCRec(fin, MacroCGrid);
              'Z':  ShowZRec(fin, MacroZGrid);
            end;
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.ShowObjectFile(fname: String);
var
    fin: TU92ObjectFile;
    lastType: AnsiChar;
begin
    lastType := ' ';
    HideAllPages;
    ObjFilePage.Visible := True;
    ObjFilePage.TabVisible := True;
    SaveBtn.Enabled := False;
    ClearGrid(ObjAGrid);
    ClearGrid(ObjHGrid);
    ClearGrid(ObjJGrid);
    ClearGrid(ObjKGrid);
    ClearGrid(ObjQGrid);
    ClearGrid(ObjYGrid);
    fin := TU92ObjectFile.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.Read;
            if (fin.CardType < lastType) then
            begin
                Split(ObjAGrid);
                Split(ObjHGrid);
                Split(ObjJGrid);
                Split(ObjKGrid);
                Split(ObjQGrid);
                Split(ObjYGrid);
            end;
            lastType := fin.CardType;
            case fin.CardType of
              'A':  ShowARec(fin, ObjAGrid);
              'H':  ShowHRec(fin, ObjHGrid);
              'J':  ShowJRec(fin, ObjJGrid);
              'K':  ShowKRec(fin, ObjKGrid);
              'Q':  ShowQRec(fin, ObjQGrid);
              'Y':  ShowYRec(fin, ObjYGrid);
            end;
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.ShowQRec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    grid.Cells[4, row] := Format('%4.4x', [fin.LoadAddr]);
    grid.Cells[5, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[6, row] := Format('%d', [fin.ExtSymID]);
    grid.Cells[7, row] := Format('%d', [fin.RelDataLength]);
    grid.Cells[8, row] := String(fin.Hex[11, fin.Length]);
    grid.Cells[9, row] := String(fin.Hex[73 - fin.RelDataLength, fin.RelDataLength]);
end;

procedure TU9200CardViewForm.ShowUnknownFile(fname: String);
var
    fin: TCardFileStream;
    bfr: TCardRec;
    i: Integer;
    stemp: AnsiString;
begin
    HideAllPages;
    UnknownFilePage.Visible := True;
    UnknownFilePage.TabVisible := True;
    SaveBtn.Enabled := True;
    ClearGrid(UnknownGrid);
    fin := TCardFileStream.Create(fname, fmOpenRead);
    try
        while (not fin.Eof) do
        begin
            fin.ReadTranslate(bfr);
            stemp := '';
            for i := 1 to bfr.Count do
                stemp := stemp + TCodeTranslator.Hollerith8ToAscii(bfr.Columns[i]);
            UnknownGrid.RowCount := UnknownGrid.RowCount + 1;
            UnknownGrid.Cells[0, UnknownGrid.RowCount - 2] := IntToStr(fin.RecNumber);
            UnknownGrid.Cells[1, UnknownGrid.RowCount - 2] := String(stemp);
        end;
    finally
        fin.Free;
    end;
end;

procedure TU9200CardViewForm.ShowYRec(fin: TU92ObjectFile; grid: TStringGrid);
var
    row: Integer;
    star: String;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    if (fin.CalcHoleCount = fin.HoleCount) then
        star := ''
    else
        star := '*';
    grid.Cells[0, row] := Format('%s%d', [star, fin.RecNumber]);
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.Length]);
    grid.Cells[4, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[5, row] := Format('%d', [fin.RelDataLength]);
    grid.Cells[6, row] := Format('%d', [fin.LastRelData]);
    grid.Cells[7, row] := Format('%d', [fin.CardCount]);
    grid.Cells[8, row] := Format('%4.4x', [fin.StartAddr]);
    grid.Cells[9, row] := Format('%6.6x', [fin.RelData]);
end;

procedure TU9200CardViewForm.ShowZRec(fin: TU92MacroFile; grid: TStringGrid);
var
    row: Integer;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    grid.Cells[1, row] := Format('%2.2x', [fin.Key]);
    grid.Cells[2, row] := String(fin.CardType);
    grid.Cells[3, row] := Format('%d', [fin.HoleCount]);
    grid.Cells[4, row] := Format('%d', [fin.DataLength]);
    grid.Cells[5, row] := String(Format('%s', [fin.Hex[5, fin.DataLength * 4]]));
end;

procedure TU9200CardViewForm.Split(grid: TStringGrid);
var
    row: Integer;
begin
    grid.RowCount := grid.RowCount + 1;
    row := grid.RowCount - 2;
    grid.Cells[1, row] := '**';
end;

end.
