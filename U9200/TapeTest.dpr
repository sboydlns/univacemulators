program TapeTest;

uses
  Vcl.Forms,
  TapeTestFrm in 'TapeTestFrm.pas' {TapeTestForm},
  U9200Types in 'U9200Types.pas',
  TapeFile in '..\Common\TapeFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TTapeTestForm, TapeTestForm);
  Application.Run;
end.
