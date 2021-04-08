program U9030ConsoleTest;

uses
  Vcl.Forms,
  U9030ConsoleTestFrm in 'U9030ConsoleTestFrm.pas' {U9030ConsoleTestForm},
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030ConsoleTestForm, U9030ConsoleTestForm);
  Application.Run;
end.
