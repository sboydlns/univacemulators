unit U9030PrintFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

const
  START_MSG = WM_USER + 1;

type
  TU9030PrintForm = class(TForm)
    FileNameEdt: TLabeledEdit;
    PrintBtn: TButton;
    OpenDlg: TOpenDialog;
    PrinterDlg: TPrintDialog;
    BrowseBtn: TButton;
    LpiBtn: TRadioGroup;
    StatusLbl: TLabel;
    procedure BrowseBtnClick(Sender: TObject);
    procedure PrintBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FAutoPrint: Boolean;
    FUngotChar: Byte;
    procedure StartMsg(var Message: TMessage); message START_MSG;
  public
    { Public declarations }
  end;

var
  U9030PrintForm: TU9030PrintForm;

implementation

{$R *.dfm}

uses Printers;

procedure TU9030PrintForm.BrowseBtnClick(Sender: TObject);
begin
    if (OpenDlg.Execute) then
        FileNameEdt.Text := OpenDlg.FileName;
end;

procedure TU9030PrintForm.FormShow(Sender: TObject);
var
    i: Integer;
begin
    // Check command line for file name to auto print
    for i := 1 to ParamCount do
    begin
        if (ParamStr(i) = '-f') then
        begin
            FileNameEdt.Text := ParamStr(i + 1);
            FAutoPrint := True;
            PostMessage(Handle, START_MSG, 0, 0);
        end;
    end;
end;

procedure TU9030PrintForm.PrintBtnClick(Sender: TObject);
var
    fin: TFileStream;
    ppi, lpi, h, lineNum, maxLine: Integer;
    line: String;
    r: TRect;

    function ReadLine(var s: String): Boolean;
    var
        c: Byte;
    begin
        Result := True;
        s := '';

        if (FUngotChar <> 0) then
        begin
            s := Chr(FUngotChar);
            FUngotChar := 0;
        end else if (fin.Position >= fin.Size) then
        begin
            Result := False;
            Exit;
        end;

        while (fin.Read(c, 1) = 1) do
        begin
            case c of
              13:
              begin
                ;
              end;
              10:
              begin
                Break;
              end;
              12:
              begin
                if (s = '') then
                begin
                    s := s + Chr(c);
                end else
                begin
                    FUngotChar := c;
                    Break;
                end;
              end
              else
              begin
                s := s + Chr(c);
              end;
            end;
        end;
    end;
begin
    StatusLbl.Visible := False;

    if (LpiBtn.ItemIndex = 1) then
    begin
        lpi := 8;
        maxLine := 88;
    end else
    begin
        lpi := 6;
        maxLine := 66;
    end;

    Printer.Orientation := poLandscape;
    if (not PrinterDlg.Execute) then
        Exit;

    fin := TFileStream.Create(FileNameEdt.Text, fmOpenRead, fmShareDenyWrite);
    Printer.Orientation := poLandscape;
    Printer.BeginDoc;
    try
        ppi := Printer.PageHeight div 11;
        Printer.Canvas.Brush.Style := bsClear;
        Printer.Canvas.Font.Name := 'Courier New';
        Printer.Canvas.Font.Style := [fsBold];
        h := ppi div lpi;
        Printer.Canvas.Font.Height := h;
        Printer.NewPage;
        lineNum := 0;

        while (ReadLine(line)) do
        begin
            if ((Length(line) > 0) and (line[1] = Char(#12))) then
            begin
                line := Copy(line, 2);
                Printer.NewPage;
                lineNum := 0;
            end;
            r := Rect(0, lineNum * h, Printer.PageWidth, ((lineNum + 1) * h) - 1);
            Printer.Canvas.TextRect(r, line, [tfNoPrefix]);
            Inc(lineNum);
            if (lineNum >= maxLine) then
            begin
                Printer.NewPage;
                lineNum := 0;
            end;
        end;
        StatusLbl.Visible := True;
        if (FAutoPrint) then
            PostMessage(Handle, WM_CLOSE, 0, 0);
    finally
        Printer.EndDoc;
        fin.Free;
    end;
end;

procedure TU9030PrintForm.StartMsg(var Message: TMessage);
begin
    PrintBtnClick(nil);
end;

end.
