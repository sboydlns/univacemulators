program U9030Print;

uses
  Vcl.Forms,
  U9030PrintFrm in 'U9030PrintFrm.pas' {U9030PrintForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030PrintForm, U9030PrintForm);
  Application.Run;
end.
