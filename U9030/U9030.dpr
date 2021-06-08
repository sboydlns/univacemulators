program U9030;



uses
  Vcl.Forms,
  U9030Frm in 'U9030Frm.pas' {U9030Form},
  Memory in 'Memory.pas',
  Cpu in 'Cpu.pas',
  Channels in 'Channels.pas',
  U9030Types in 'U9030Types.pas',
  Globals in 'Globals.pas',
  IDA in 'IDA.pas',
  U8418 in 'U8418.pas',
  IPC in 'IPC.pas',
  Console in 'Console.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  DebuggerFrm in 'DebuggerFrm.pas' {DebuggerForm},
  Bcd in '..\Common\Bcd.pas',
  Trace in 'Trace.pas',
  U0773 in 'U0773.pas',
  CpuTestFrm in 'CpuTestFrm.pas' {CpuTestForm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU9030Form, U9030Form);
  Application.CreateForm(TDebuggerForm, DebuggerForm);
  Application.CreateForm(TCpuTestForm, CpuTestForm);
  Application.Run;
end.
