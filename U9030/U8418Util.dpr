program U8418Util;

uses
  Vcl.Forms,
  U8418UtilFrm in 'U8418UtilFrm.pas' {U8418UtilForm},
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U8418 in 'U8418.pas',
  VTOC in 'VTOC.pas',
  OS3Files in 'OS3Files.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU8418UtilForm, U8418UtilForm);
  Application.Run;
end.
