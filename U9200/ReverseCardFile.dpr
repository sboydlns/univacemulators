program ReverseCardFile;

uses
  Vcl.Forms,
  ReverseCardFileFrm in 'ReverseCardFileFrm.pas' {ReverseCardFileForm},
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TReverseCardFileForm, ReverseCardFileForm);
  Application.Run;
end.
