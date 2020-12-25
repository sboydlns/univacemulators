program U1005DisAsm;

uses
  Vcl.Forms,
  U1005DisAsmFrm in 'U1005DisAsmFrm.pas' {U1005DisAsmForm},
  U1005Types in 'U1005Types.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U1005Files in 'U1005Files.pas',
  CardFile in '..\Common\CardFile.pas',
  U1005Memory in 'U1005Memory.pas',
  Bcd in '..\Common\Bcd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU1005DisAsmForm, U1005DisAsmForm);
  Application.Run;
end.
