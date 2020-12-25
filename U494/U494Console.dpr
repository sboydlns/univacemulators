program U494Console;

uses
  Vcl.Forms,
  U494ConsoleFrm in 'U494ConsoleFrm.pas' {U494ConsoleForm},
  U494Util in 'U494Util.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU494ConsoleForm, U494ConsoleForm);
  Application.Run;
end.
