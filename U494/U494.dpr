program U494;

uses
  Vcl.Forms,
  U494Panel in 'U494Panel.pas' {U494PanelFrm},
  U494Memory in 'U494Memory.pas',
  U494System in 'U494System.pas',
  U494Util in 'U494Util.pas',
  U494Cpu in 'U494Cpu.pas',
  U494Opcodes in 'U494Opcodes.pas',
  ObjFile in 'ObjFile.pas',
  EmulatorTypes in '..\Common\EmulatorTypes.pas',
  U494Interrupts in 'U494Interrupts.pas',
  U494ConsDevice in 'U494ConsDevice.pas',
  FH880Device in 'FH880Device.pas',
  FH880File in 'FH880File.pas',
  Bcd in '..\Common\Bcd.pas',
  CardFile in '..\Common\CardFile.pas',
  U494ReaderFram in 'U494ReaderFram.pas' {U494ReaderFrame: TFrame},
  U494Reader in 'U494Reader.pas',
  U494PunchFram in 'U494PunchFram.pas' {U494PunchFrame: TFrame},
  LoadCardsFrm in '..\Common\LoadCardsFrm.pas' {LoadCardsForm},
  U494Printer in 'U494Printer.pas',
  U494PrinterFram in 'U494PrinterFram.pas' {U494PrinterFrame: TFrame},
  U494Config in 'U494Config.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TU494PanelFrm, U494PanelFrm);
  Application.CreateForm(TLoadCardsForm, LoadCardsForm);
  Application.Run;
end.
