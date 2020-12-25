program H2B;

uses
  Vcl.Forms,
  H2BFrm in 'H2BFrm.pas' {H2BForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TH2BForm, H2BForm);
  Application.Run;
end.
