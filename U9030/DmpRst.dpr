program DmpRst;

uses
  Vcl.Forms,
  DmpRstFrm in 'DmpRstFrm.pas' {DmpRstForm},
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U8418 in 'U8418.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDmpRstForm, DmpRstForm);
  Application.Run;
end.
