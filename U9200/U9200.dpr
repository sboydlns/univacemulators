program U9200;

uses
  Vcl.Forms,
  ControlPanel in 'ControlPanel.pas' {ControlPanelForm},
  U9200System in 'U9200System.pas',
  U9200Types in 'U9200Types.pas',
  U9200Memory in 'U9200Memory.pas',
  U9200CPU in 'U9200CPU.pas',
  U9200ReaderFram in 'U9200ReaderFram.pas' {U9200ReaderFrame: TFrame},
  U9200IPC in 'U9200IPC.pas',
  U9200Reader in 'U9200Reader.pas',
  U9200Device in 'U9200Device.pas',
  U9200DebuggerFrm in 'U9200DebuggerFrm.pas' {U92Debugger},
  U9200PrinterFram in 'U9200PrinterFram.pas' {U9200PrinterFrame: TFrame},
  U9200Printer in 'U9200Printer.pas',
  U9200PunchFram in 'U9200PunchFram.pas' {U9200PunchFrame: TFrame},
  U9200Punch in 'U9200Punch.pas',
  U9200OpReq in 'U9200OpReq.pas',
  U9200TapeFram in 'U9200TapeFram.pas' {U9200TapeFrame: TFrame},
  MountTapeFrm in 'MountTapeFrm.pas' {MountTapeForm},
  U9200Mux in 'U9200Mux.pas',
  U9200TapeController in 'U9200TapeController.pas',
  U9200Tape in 'U9200Tape.pas',
  U9200Config in 'U9200Config.pas',
  CardFile in '..\Common\CardFile.pas',
  TapeFile in '..\Common\TapeFile.pas',
  U9200Timer in 'U9200Timer.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  LoadCardsFrm in '..\Common\LoadCardsFrm.pas' {LoadCardsForm},
  Bcd in '..\Common\Bcd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TControlPanelForm, ControlPanelForm);
  Application.CreateForm(TMountTapeForm, MountTapeForm);
  Application.CreateForm(TLoadCardsForm, LoadCardsForm);
  Application.Run;
end.
