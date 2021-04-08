program U9030;

uses
  Vcl.Forms,
  U9030Frm in 'U9030Frm.pas' {U9030Form},
  Memory in 'Memory.pas',
  Cpu in 'Cpu.pas',
  Channels in 'Channels.pas',
  U9030Types in 'U9030Types.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030Form, U9030Form);
  Application.Run;
end.
