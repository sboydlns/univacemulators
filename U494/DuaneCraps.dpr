program DuaneCraps;

uses
  Vcl.Forms,
  DuaneCrapsFrm in 'DuaneCrapsFrm.pas' {Form3},
  SrcFile in 'SrcFile.pas',
  ObjFile in 'ObjFile.pas',
  U494Util in 'U494Util.pas',
  Bcd in '..\Common\Bcd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
