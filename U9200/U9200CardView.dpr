program U9200CardView;

uses
  Vcl.Forms,
  U9200CardViewFrm in 'U9200CardViewFrm.pas' {U9200CardViewForm},
  U9200Files in 'U9200Files.pas',
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9200CardViewForm, U9200CardViewForm);
  Application.Run;
end.
