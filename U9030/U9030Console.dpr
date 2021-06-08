program U9030Console;

uses
  Vcl.Forms,
  U9030ConsoleFrm in 'U9030ConsoleFrm.pas' {U9030ConsoleForm},
  Uniscope in '..\Uniscope\Uniscope.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030ConsoleForm, U9030ConsoleForm);
  Application.Run;
end.
