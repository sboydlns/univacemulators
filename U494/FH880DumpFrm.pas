unit FH880DumpFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, FH880File;

type
  TFH880DumpForm = class(TForm)
    Panel1: TPanel;
    AddressEdt: TEdit;
    Label1: TLabel;
    GoBtn: TButton;
    Memo: TMemo;
    procedure GoBtnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FDrum: TFH880File;
  public
    procedure Dump;
    procedure Show(fname: String); reintroduce;
  end;

var
  FH880DumpForm: TFH880DumpForm;

implementation

{$R *.dfm}

uses EmulatorTypes, U494Util;

{ TFH880DumpForm }

procedure TFH880DumpForm.Dump;
var
    addr: Integer;
    l, w: Integer;
    word: UInt32;
    oct, fd: String;
begin
    if (not TryStrToInt(Trim(AddressEdt.Text), addr)) then
        addr := 0;
    Memo.Lines.Clear;
    for l := 1 to 12 do
    begin
        oct := '         ';
        fd := Format('%7.7d: ', [addr]);
        for w := 1 to 8 do
        begin
            word := FDrum.ReadWord(addr);
            oct := Format('%s %s ', [oct, FormatOctal(word)]);
            fd := fd + ' ' +
                  Char(TCodeTranslator.FieldataToAscii((word shr 24) and $3f)) + ' ' +
                  Char(TCodeTranslator.FieldataToAscii((word shr 18) and $3f)) + ' ' +
                  Char(TCodeTranslator.FieldataToAscii((word shr 12) and $3f)) + ' ' +
                  Char(TCodeTranslator.FieldataToAscii((word shr 6) and $3f)) + ' ' +
                  Char(TCodeTranslator.FieldataToAscii((word and $3f))) + '  ';
            Inc(addr);
        end;
        Memo.Lines.Add(fd);
        Memo.Lines.Add(oct);
    end;
    AddressEdt.Text := IntToStr(addr);
end;

procedure TFH880DumpForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    FreeAndNil(FDrum);
end;

procedure TFH880DumpForm.GoBtnClick(Sender: TObject);
begin
    Dump;
end;

procedure TFH880DumpForm.Show(fname: String);
begin
    inherited Show;
    FreeAndNil(FDrum);
    FDrum := TFH880File.Create(fname, fmOpenRead or fmShareDenyNone);
    Dump;
end;

end.
