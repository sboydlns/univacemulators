unit U9200ObjCardViewFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, Vcl.StdCtrls;

type
  TU9200ObjCardViewForm = class(TForm)
    Label1: TLabel;
    OpenDlg: TOpenDialog;
    FileNameEdt: TEdit;
    BrowseBtn: TButton;
    ShowBtn: TButton;
    CancelBtn: TButton;
    CardGrid: TStringGrid;
    procedure BrowseBtnClick(Sender: TObject);
    procedure CancelBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ShowBtnClick(Sender: TObject);
  private
    procedure DumpA(row: Integer; bfr: array of Byte);
    procedure DumpH(row: Integer; bfr: array of Byte);
    procedure DumpJ(row: Integer; bfr: array of Byte);
    procedure DumpK(row: Integer; bfr: array of Byte);
    procedure DumpQ(row: Integer; bfr: array of Byte);
    procedure DumpUnknown(row: Integer; bfr: array of Byte);
    procedure DumpY(row: Integer; bfr: array of Byte);
    function ToHex(b: Byte): String;
    function ToInteger(b1, b2, b3, b4: Byte): Integer;
    function ToSmallint(b1, b2: Byte): Smallint;
  public
    { Public declarations }
  end;

var
  U9200ObjCardViewForm: TU9200ObjCardViewForm;

implementation

uses U9200Types, CardFile;

const
  COL_1 = 1;
  CARD_TYPE = 2;
  CARD_LEN = 3;
  HOLE_COUNT = 4;
  CARD_TEXT = 5;
{$R *.dfm}

procedure TU9200ObjCardViewForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9200ObjCardViewForm.CancelBtnClick(Sender: TObject);
begin
    Close;
end;

procedure TU9200ObjCardViewForm.DumpA(row: Integer; bfr: array of Byte);
var
    len: Byte;
    text: String;
    i: Integer;
    nam: AnsiString;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    len := bfr[2];
    CardGrid.Cells[CARD_LEN, row] := IntToStr(len);
    text := '';
    if (bfr[5] = 0) then
        text := 'Rel=Y';
    text := text + Format(' ESID=%d', [bfr[7]]);
    text := text + Format(' StartAddr=%d ', [ToInteger(bfr[12], bfr[13], bfr[14], bfr[15])]);
    nam := '';
    for i := 16 to 23 do
        nam := nam + TCodeTranslator.EbcdicToAscii(bfr[i]);
    text := text + Format(' Name=%s', [nam]);
    text := text + Format(' Len=%d ', [ToInteger(bfr[32], bfr[33], bfr[34], bfr[35])]);
    CardGrid.Cells[CARD_TEXT, row] := text;
end;

procedure TU9200ObjCardViewForm.DumpH(row: Integer; bfr: array of Byte);
var
    len: Byte;
    text: String;
    i: Smallint;
    nam: AnsiString;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    len := bfr[2];
    CardGrid.Cells[CARD_LEN, row] := IntToStr(len);
    text := '';
    text := text + Format(' RLD Len=%d', [bfr[8]]);
    text := text + Format(' Last RLD=%d', [bfr[9]]);
    text := text + Format(' SymAddr=%d ', [ToInteger(bfr[12], bfr[13], bfr[14], bfr[15])]);
    nam := '';
    for i := 16 to 23 do
        nam := nam + TCodeTranslator.EbcdicToAscii(bfr[i]);
    text := text + Format(' Name=%s', [nam]);
    CardGrid.Cells[CARD_TEXT, row] := text;
end;

procedure TU9200ObjCardViewForm.DumpJ(row: Integer; bfr: array of Byte);
var
    len: Byte;
    text: String;
    i: Smallint;
    nam: AnsiString;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    len := bfr[2];
    CardGrid.Cells[CARD_LEN, row] := IntToStr(len);
    text := '';
    text := text + Format(' ESID=%d', [bfr[7]]);
    text := text + Format(' StartAddr=%d ', [ToInteger(bfr[12], bfr[13], bfr[14], bfr[15])]);
    nam := '';
    for i := 16 to 23 do
        nam := nam + TCodeTranslator.EbcdicToAscii(bfr[i]);
    text := text + Format(' Name=%s', [nam]);
    CardGrid.Cells[CARD_TEXT, row] := text;
end;

procedure TU9200ObjCardViewForm.DumpK(row: Integer; bfr: array of Byte);
var
    len: Byte;
    text: String;
    i: Smallint;
    nam: AnsiString;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    len := bfr[2];
    CardGrid.Cells[CARD_LEN, row] := IntToStr(len);
    text := '';
    text := text + Format(' ESID=%d', [bfr[7]]);
    nam := '';
    for i := 16 to 23 do
        nam := nam + TCodeTranslator.EbcdicToAscii(bfr[i]);
    text := text + Format(' Name=%s', [nam]);
    CardGrid.Cells[CARD_TEXT, row] := text;
end;

procedure TU9200ObjCardViewForm.DumpQ(row: Integer; bfr: array of Byte);
var
    len: Byte;
    text: String;
    i: Smallint;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    len := bfr[2];
    CardGrid.Cells[CARD_LEN, row] := IntToStr(len);
    text := Format('Load Addr=%d ', [ToSmallint(bfr[4], bfr[5])]);
    for i := 10 to len + 10 do
        text := text + ToHex(bfr[i]);
    CardGrid.Cells[CARD_TEXT, row] := text;
end;

procedure TU9200ObjCardViewForm.DumpUnknown(row: Integer; bfr: array of Byte);
var
    b: Byte;
    txt: AnsiString;
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := '';
    CardGrid.Cells[CARD_LEN, row] := '';
    txt := '';
    for b in bfr do
        txt := txt +  TCodeTranslator.HollerithToAscii(b);
    CardGrid.Cells[CARD_TEXT, row] := String(txt);
end;

procedure TU9200ObjCardViewForm.DumpY(row: Integer; bfr: array of Byte);
begin
    CardGrid.RowCount := row + 1;
    CardGrid.Cells[COL_1, row] := ToHex(bfr[0]);
    CardGrid.Cells[CARD_TYPE, row] := String(TCodeTranslator.HollerithToAscii(bfr[1]));
    CardGrid.Cells[HOLE_COUNT, row] := ToHex(bfr[6]);
    CardGrid.Cells[CARD_LEN, row] := IntToStr(ToSmallint(bfr[11], bfr[12]));
    CardGrid.Cells[CARD_TEXT, row] := Format('Xfer Addr=%d', [ToSmallint(bfr[14], bfr[15])]);
end;

procedure TU9200ObjCardViewForm.FormShow(Sender: TObject);
begin
    with CardGrid do
    begin
        Cells[COL_1, 0] := 'Col1';
        Cells[CARD_TYPE, 0] := 'Type';
        Cells[CARD_LEN, 0] := 'Len.';
        Cells[HOLE_COUNT, 0] := 'Count';
        Cells[CARD_TEXT, 0] := 'Text';
    end;
end;

procedure TU9200ObjCardViewForm.ShowBtnClick(Sender: TObject);
var
    fin: TCardFileStream;
    row: Integer;
    bfr: TCardRec;
    ct: AnsiChar;
begin
    FileNameEdt.Text := Trim(FileNameEdt.Text);
    if (FileNameEdt.Text = '') then
        raise Exception.Create('Please enter the path to the object file');

    CardGrid.RowCount := 1;
    row := 1;
    fin := TCardFileStream.Create(FileNameEdt.Text, fmOpenRead);
    try
        while (fin.ReadTranslate(bfr) > 0) do
        begin
            ct := TCodeTranslator.HollerithToAscii(bfr.Columns[2]);
            case ct of
              'A':
              begin
                DumpA(row, bfr.Columns);
                Inc(row);
              end;
              'H':
              begin
                DumpH(row, bfr.Columns);
                Inc(row);
              end;
              'J':
              begin
                DumpJ(row, bfr.Columns);
                Inc(row);
              end;
              'K':
              begin
                DumpK(row, bfr.Columns);
                Inc(row);
              end;
              'Q':
              begin
                DumpQ(row, bfr.Columns);
                Inc(row);
              end;
              'Y':
              begin
                DumpY(row, bfr.Columns);
                Inc(row);
              end;
              else
              begin
                DumpUnknown(row, bfr.Columns);
                Inc(row);
              end;
            end;
        end;
    finally
        fin.Free;
    end;
end;

function TU9200ObjCardViewForm.ToHex(b: Byte): String;
begin
    Result := Format('%2.2x', [b]);
end;

function TU9200ObjCardViewForm.ToInteger(b1, b2, b3, b4: Byte): Integer;
begin
    Result := (b1 shl 24) or (b2 shl 16) or (b3 shl 8) or b4;
end;

function TU9200ObjCardViewForm.ToSmallint(b1, b2: Byte): Smallint;
begin
    Result := (b1 shl 8) or b2;
end;

end.
