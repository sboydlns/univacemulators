unit U1005PrinterFram;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  U1005Printer, EmulatorTypes;

type
  TU1005PrinterFrame = class(TFrame)
    Label65: TLabel;
    LPIBtn: TRadioGroup;
    ScrollBox1: TScrollBox;
    Page: TImage;
    Label3: TLabel;
    PrintBtn: TCheckBox;
    PrintDlg: TPrintDialog;
    SingleSpcBtn: TCheckBox;
    procedure PrintBtnClick(Sender: TObject);
    procedure LPIBtnClick(Sender: TObject);
    procedure SingleSpcBtnClick(Sender: TObject);
  private
    FPrinter: T1005Printer;
    FBitmap: TPrinterBitmap;
    procedure DoOnHome(Sender: TObject);
    procedure DoOnPrint(Sender: TObject; lineNum: Integer; text: AnsiString);
  public
    constructor Create(AOwner: TComponent; prt: T1005Printer); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TU1005PrinterFrame }

constructor TU1005PrinterFrame.Create(AOwner: TComponent; prt: T1005Printer);
begin
    inherited Create(AOwner);
    FPrinter := prt;
    FPrinter.OnHome := DoOnHome;
    FPrinter.OnPrint := DoOnPrint;
    FBitmap := TPrinterBitmap.Create(FPrinter.LPI, Screen.PixelsPerInch);
    Page.Picture.Bitmap := FBitmap;
end;

destructor TU1005PrinterFrame.Destroy;
begin
    inherited;
    FreeAndNil(FBitmap);
end;

procedure TU1005PrinterFrame.DoOnHome(Sender: TObject);
begin
    FBitmap.Clear;
    Page.Picture.Bitmap := FBitmap;
    Application.ProcessMessages;
end;

procedure TU1005PrinterFrame.DoOnPrint(Sender: TObject; lineNum: Integer; text: AnsiString);
begin
    FBitmap.Print(lineNum, text);
    Page.Picture.Bitmap := FBitmap;
    Application.ProcessMessages;
end;

procedure TU1005PrinterFrame.LPIBtnClick(Sender: TObject);
begin
    if (LPIBtn.ItemIndex = 1) then
        FPrinter.LPI := 8
    else
        FPrinter.LPI := 6;
end;

procedure TU1005PrinterFrame.PrintBtnClick(Sender: TObject);
begin
    if (PrintBtn.Checked) then
    begin
        if (not PrintDlg.Execute) then
            PrintBtn.Checked := False;
    end;
    FPrinter.SendToPrint := PrintBtn.Checked;
end;

procedure TU1005PrinterFrame.SingleSpcBtnClick(Sender: TObject);
begin
    FPrinter.SingleSpace := SingleSpcBtn.Checked;
end;

end.
