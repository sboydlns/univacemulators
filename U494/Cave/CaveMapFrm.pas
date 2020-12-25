unit CaveMapFrm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TCaveMapForm = class(TForm)
    Map: TMemo;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  CaveMapForm: TCaveMapForm;

implementation

{$R *.dfm}

end.
