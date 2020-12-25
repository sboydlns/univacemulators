program CaveUtil;

uses
  Vcl.Forms,
  CaveUtilFrm in 'CaveUtilFrm.pas' {CaveUtilForm},
  SrcFile in '..\SrcFile.pas',
  CaveMapFrm in 'CaveMapFrm.pas' {CaveMapForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TCaveUtilForm, CaveUtilForm);
  Application.CreateForm(TCaveMapForm, CaveMapForm);
  Application.Run;
end.
