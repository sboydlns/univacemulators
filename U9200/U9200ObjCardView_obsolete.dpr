program U9200ObjCardView;

uses
  Vcl.Forms,
  U9200ObjCardViewFrm in 'U9200ObjCardViewFrm.pas' {U9200ObjCardViewForm},
  U9200Types in 'U9200Types.pas',
  CardFile in 'CardFile.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9200ObjCardViewForm, U9200ObjCardViewForm);
  Application.Run;
end.
