program U1710;

uses
  Vcl.Forms,
  U1710Frm in 'U1710Frm.pas' {U1710Form},
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  LoadCardsFrm in '..\Common\LoadCardsFrm.pas' {LoadCardsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU1710Form, U1710Form);
  Application.CreateForm(TLoadCardsForm, LoadCardsForm);
  Application.Run;
end.
