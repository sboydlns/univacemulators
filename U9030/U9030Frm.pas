unit U9030Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Memory;

type
  TU9030Form = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    FMemory: TMemory;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U9030Form: TU9030Form;

implementation

{$R *.dfm}

uses U9030Types;

{ TU9030Form }

procedure TU9030Form.Button1Click(Sender: TObject);
var
    b1, b2: Byte;
    hw1, hw2: THalfWord;
    w1, w2: TWord;
    dw1, dw2: TDblWord;
begin
    b1 := $12;
    hw1 := $1234;
    w1 := $12345678;
    dw1 := $123456789abcdef0;
    FMemory.StoreByte(0, $12);
    FMemory.StoreByte(2, $12);
    FMemory.StoreByte(3, $34);
    FMemory.StoreByte(4, $12);
    FMemory.StoreByte(5, $34);
    FMemory.StoreByte(6, $56);
    FMemory.StoreByte(7, $78);
    FMemory.StoreByte(8, $12);
    FMemory.StoreByte(9, $34);
    FMemory.StoreByte(10, $56);
    FMemory.StoreByte(11, $78);
    FMemory.StoreByte(12, $9a);
    FMemory.StoreByte(13, $bc);
    FMemory.StoreByte(14, $de);
    FMemory.StoreByte(15, $f0);
    b2 := FMemory.FetchByte(0);
    hw2 := FMemory.FetchHalfWord(2);
    w2 := FMemory.FetchWord(4);
    dw2 := FMemory.FetchDblWord(8);
end;

constructor TU9030Form.Create(AOwner: TComponent);
begin
    inherited;
    FMemory := TMemory.Create;
end;

end.
