program U1005;

uses
  Vcl.Forms,
  ControlPanel in 'ControlPanel.pas' {ControlPanelFrm},
  U1005System in 'U1005System.pas',
  U1005Memory in 'U1005Memory.pas',
  U1005Types in 'U1005Types.pas',
  CardFile in '..\Common\CardFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U1005Reader in 'U1005Reader.pas',
  U1005Printer in 'U1005Printer.pas',
  U1005Punch in 'U1005Punch.pas',
  U1005Device in 'U1005Device.pas',
  U1005ReaderFram in 'U1005ReaderFram.pas' {U1005ReaderFrame: TFrame},
  Bcd in '..\Common\Bcd.pas',
  U1005PrinterFram in 'U1005PrinterFram.pas' {U9200PrinterFrame: TFrame},
  U1005PunchFram in 'U1005PunchFram.pas' {U1005PunchFrame: TFrame},
  LoadCardsFrm in '..\Common\LoadCardsFrm.pas' {LoadCardsForm},
  U1005CPU in 'U1005CPU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TControlPanelFrm, ControlPanelFrm);
  Application.CreateForm(TLoadCardsForm, LoadCardsForm);
  Application.Run;
end.
