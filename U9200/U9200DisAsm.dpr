program U9200DisAsm;

uses
  Vcl.Forms,
  U9200DisAsmFrm in 'U9200DisAsmFrm.pas' {U9200DisAsmForm},
  U9200Files in 'U9200Files.pas',
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9200DisAsmForm, U9200DisAsmForm);
  Application.Run;
end.
