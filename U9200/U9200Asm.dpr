program U9200Asm;

uses
  Vcl.Forms,
  U9200AsmFrm in 'U9200AsmFrm.pas' {U9200AsmForm},
  U9200Types in 'U9200Types.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9200AsmForm, U9200AsmForm);
  Application.Run;
end.
