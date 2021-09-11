program U200TN;

uses
  Vcl.Forms,
  U200TNFrm in 'U200TNFrm.pas' {U9030ConsoleForm},
  Uniscope in '..\Uniscope\Uniscope.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU200TNForm, U200TNForm);
  Application.Run;
end.
