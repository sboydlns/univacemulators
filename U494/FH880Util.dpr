program FH880Util;

uses
  Vcl.Forms,
  FH880UtilFrm in 'FH880UtilFrm.pas' {FH880UtilForm},
  FH880File in 'FH880File.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  FH880DumpFrm in 'FH880DumpFrm.pas' {FH880DumpForm},
  U494Util in 'U494Util.pas',
  SrcFile in 'SrcFile.pas',
  Bcd in '..\Common\Bcd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFH880UtilForm, FH880UtilForm);
  Application.CreateForm(TFH880DumpForm, FH880DumpForm);
  Application.Run;
end.
