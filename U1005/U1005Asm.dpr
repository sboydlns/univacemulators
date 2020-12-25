program U1005Asm;

uses
  Vcl.Forms,
  U1005AsmFrm in 'U1005AsmFrm.pas' {U1005AsmForm},
  U1005Types in 'U1005Types.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U1005Memory in 'U1005Memory.pas',
  CardFile in '..\Common\CardFile.pas',
  Bcd in '..\Common\Bcd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU1005AsmForm, U1005AsmForm);
  Application.Run;
end.
