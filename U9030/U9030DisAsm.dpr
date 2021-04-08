program U9030DisAsm;

uses
  Vcl.Forms,
  U9030DisAsmFrm in 'U9030DisAsmFrm.pas' {U9030DisAsmForm},
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U9030Types in 'U9030Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030DisAsmForm, U9030DisAsmForm);
  Application.Run;
end.
