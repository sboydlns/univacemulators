unit H2BFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TH2BForm = class(TForm)
    B12: TCheckBox;
    B11: TCheckBox;
    b0: TCheckBox;
    b1: TCheckBox;
    b2: TCheckBox;
    b3: TCheckBox;
    b4: TCheckBox;
    b5: TCheckBox;
    b6: TCheckBox;
    b7: TCheckBox;
    b8: TCheckBox;
    b9: TCheckBox;
    XlateBtn: TButton;
    CloseBtn: TButton;
    ResultLbl: TLabel;
    procedure FormShow(Sender: TObject);
    procedure XlateBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  H2BForm: TH2BForm;

implementation

{$R *.dfm}

procedure TH2BForm.FormShow(Sender: TObject);
begin
    ResultLbl.Caption := '';
end;

procedure TH2BForm.XlateBtnClick(Sender: TObject);
var
    b: Byte;
begin
    b := 0;
    if (b12.Checked) then            // 12 punch
        b := b or $01;
    if (b11.Checked) then            // 11 punch
        b := b or $02;
    if (b0.Checked) then            // 0 punch
        b := b or $04;
    if (b1.Checked) then            // 1 punch
        b := b or $30;
    if (b2.Checked) then            // 2 punch
        b := b or $50;
    if (b3.Checked) then            // 3 punch
        b := b or $10;
    if (b4.Checked) then            // 4 punch
        b := b or $20;
    if (b5.Checked) then            // 5 punch
        b := b or $40;
    if (b6.Checked) then            // 6 punch
        b := b or $70;
    if (b7.Checked) then            // 7 punch
        b := b or $60;
    if (b8.Checked) then            // 8 punch
        b := b or $08;
    if (b9.Checked) then            // 9 punch
        b := b or $80;
    ResultLbl.Caption := Format('%2.2x', [b]);
end;

end.
