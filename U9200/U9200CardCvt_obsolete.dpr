program U9200CardCvt;

uses
  Vcl.Forms,
  U9200CardCvtFrm in 'U9200CardCvtFrm.pas' {Form3},
  CardFile in 'CardFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
