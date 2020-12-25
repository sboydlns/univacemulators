unit ControlPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.ExtCtrls, U9200Types,
  U9200System, Vcl.Samples.Gauges, U9200ReaderFram, U9200PrinterFram, U9200DebuggerFrm,
  U9200PunchFram, Vcl.ComCtrls, U9200TapeFram, Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc;

type
  TControlPanelForm = class(TForm)
    Panel1: TPanel;
    PanelImg: TImage;
    RefreshTimer: TTimer;
    LoadPgmDlg: TOpenDialog;
    Addr3Btn: TSpeedButton;
    Addr2Btn: TSpeedButton;
    Addr1Btn: TSpeedButton;
    Addr0Btn: TSpeedButton;
    Addr4Btn: TSpeedButton;
    Addr5Btn: TSpeedButton;
    Addr6Btn: TSpeedButton;
    Addr7Btn: TSpeedButton;
    Addr8Btn: TSpeedButton;
    Addr9Btn: TSpeedButton;
    Addr10Btn: TSpeedButton;
    Addr11Btn: TSpeedButton;
    Label1: TLabel;
    Addr12Btn: TSpeedButton;
    Addr13Btn: TSpeedButton;
    Addr14Btn: TSpeedButton;
    SpeedButton20: TSpeedButton;
    SpeedButton21: TSpeedButton;
    SpeedButton22: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ChannelClearBtn: TSpeedButton;
    Label5: TLabel;
    ClearBtn: TSpeedButton;
    Label6: TLabel;
    Data3Btn: TSpeedButton;
    Data2Btn: TSpeedButton;
    Data1Btn: TSpeedButton;
    Data0Btn: TSpeedButton;
    Data4Btn: TSpeedButton;
    Data5Btn: TSpeedButton;
    Data6Btn: TSpeedButton;
    Data7Btn: TSpeedButton;
    Label7: TLabel;
    AlterBtn: TSpeedButton;
    Label8: TLabel;
    DisplayBtn: TSpeedButton;
    Label9: TLabel;
    BBtn: TSpeedButton;
    CBtn: TSpeedButton;
    DBtn: TSpeedButton;
    EBtn: TSpeedButton;
    ABtn: TSpeedButton;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    ProcBtn: TSpeedButton;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    OpReqBtn: TSpeedButton;
    StartBtn: TSpeedButton;
    InstBtn: TSpeedButton;
    CycleBtn: TSpeedButton;
    LoadBtn: TSpeedButton;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label20: TLabel;
    PunchOfflineBtn: TSpeedButton;
    PunchClearBtn: TSpeedButton;
    PunchFeedBtn: TSpeedButton;
    PunchContinuousBtn: TSpeedButton;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    ReaderOfflineBtn: TSpeedButton;
    ReaderClrBtn: TSpeedButton;
    ReaderFeedBtn: TSpeedButton;
    ReaderContinuousBtn: TSpeedButton;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label29: TLabel;
    Label30: TLabel;
    PrinterOfflineBtn: TSpeedButton;
    PrinterClearBtn: TSpeedButton;
    PrinterHomeBtn: TSpeedButton;
    PrinterSpaceBtn: TSpeedButton;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    OnBtn: TSpeedButton;
    OffBtn: TSpeedButton;
    Label36: TLabel;
    Label37: TLabel;
    ProcAbnLed: TImage;
    Label38: TLabel;
    Label39: TLabel;
    TestLed: TImage;
    Label40: TLabel;
    RunLed: TImage;
    Label41: TLabel;
    StopLed: TImage;
    Label42: TLabel;
    OpReqLed: TImage;
    ALed: TImage;
    BLed: TImage;
    CLed: TImage;
    DLed: TImage;
    ELed: TImage;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Display3Led: TImage;
    Display2Led: TImage;
    Display1Led: TImage;
    Display0Led: TImage;
    Display7Led: TImage;
    Display6Led: TImage;
    Display5Led: TImage;
    Display4Led: TImage;
    Display11Led: TImage;
    Display10Led: TImage;
    Display9Led: TImage;
    Display8Led: TImage;
    Display15Led: TImage;
    Display14Led: TImage;
    Display13Led: TImage;
    Display12Led: TImage;
    Indicator0Led: TImage;
    Indicator1Led: TImage;
    Indicator2Led: TImage;
    Indicator3Led: TImage;
    Label49: TLabel;
    Label50: TLabel;
    PunchAbnLed: TImage;
    ReaderAbnLed: TImage;
    Label51: TLabel;
    PrinterAbnLed: TImage;
    Label52: TLabel;
    PowerLed: TImage;
    Label53: TLabel;
    Label54: TLabel;
    Label55: TLabel;
    Label56: TLabel;
    Label57: TLabel;
    Label58: TLabel;
    Label59: TLabel;
    Label60: TLabel;
    Label61: TLabel;
    Label62: TLabel;
    CPUErrorLbl: TLabel;
    PeripheralPages: TPageControl;
    CardPeriphPage: TTabSheet;
    TapePeriphPage: TTabSheet;
    TapePanel8: TPanel;
    TapePanel7: TPanel;
    TapePanel6: TPanel;
    TapePanel5: TPanel;
    TapePanel4: TPanel;
    TapePanel3: TPanel;
    TapePanel2: TPanel;
    TapePanel1: TPanel;
    PrinterPage: TTabSheet;
    PrinterPanel: TPanel;
    Panel2: TPanel;
    ReaderPanel: TPanel;
    PunchPanel: TPanel;
    Config: TXMLDocument;
    procedure BtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OnBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure OnBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure OffBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure OffBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure InstBtnClick(Sender: TObject);
    procedure AlterBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure AlterBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DisplayBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DisplayBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure AddrClick(Sender: TObject);
    procedure DataClick(Sender: TObject);
    procedure StartBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure StartBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure LoadBtnClick(Sender: TObject);
    procedure ABtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ABtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure BBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure CBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure CBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure DBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure EBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ReaderOfflineBtnClick(Sender: TObject);
    procedure ReaderClrBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ReaderClrBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ReaderFeedBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ReaderFeedBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ReaderContinuousBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ReaderContinuousBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure CycleBtnClick(Sender: TObject);
    procedure PrinterOfflineBtnClick(Sender: TObject);
    procedure PrinterSpaceBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PrinterSpaceBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PunchFeedBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PunchFeedBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PunchOfflineBtnClick(Sender: TObject);
    procedure PunchClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PunchClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PunchContinuousBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PunchContinuousBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure PrinterHomeBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure PrinterHomeBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure OpReqBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure OpReqBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
      Y: Integer);
    procedure ChannelClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ChannelClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ProcBtnClick(Sender: TObject);
    procedure PrinterClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PrinterClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FGreenLedOff: TBitmap;
    FGreenLedOn: TBitmap;
    FRedLedOff: TBitmap;
    FRedLedOn: TBitmap;
    FSwitchOff: TBitmap;
    FSwitchOn: TBitmap;
    FWhiteLedOff: TBitmap;
    FWhiteLedOn: TBitmap;
    FSystem: TU9200;
    FClearDown: Boolean;
    FLoadDown: Boolean;
    FDisplaySelect: Char;
    FDataRegister: Integer;
    FDisplayRegister: Integer;
    FInstOn: Boolean;
    FReaderOfflineDown: Boolean;
    FReaderContinuousDown: Boolean;
    FPunchOfflineDown: Boolean;
    FPunchContinuousDown: Boolean;
    FPrinterOfflineDown: Boolean;
    FReaderFrame: TU9200ReaderFrame;
    FPunchFrame: TU9200PunchFrame;
    FPrinterFrame: TU9200PrinterFrame;
    FTapePanels: array[0..7] of TPanel;
    FTapeFrames: array [0..7] of TU9200TapeFrame;
    FCycleDown: Boolean;
    FProcDown: Boolean;
    FDebugger: TU92Debugger;
    procedure DoError(Sender: TObject; E: Exception);
    procedure DoExecuteInstruction(Sender: TObject);
    procedure DoFetchInstruction(Sender: TObject);
    procedure DoHalt(Sender: TObject);
    procedure DoLampChange(Sender: TObject; lamp, state: Integer);
    function GetDataEntry: Byte;
    function GetMemoryAddress: Integer;
    procedure LampTest(onOff: Integer);
    procedure Load;
    procedure MomentaryOff(btn: TSpeedButton);
    procedure MomentaryOn(btn: TSpeedButton);
    procedure SetDisplay(value: Integer);
    function ToggleSwitch(btn: TSpeedButton): Boolean;
    procedure UpdateLamp(led: TImage; onOff: Integer); overload;
    procedure UpdateLamp(led: TImage; onOff: Boolean); overload;
    procedure UpdateLamps;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  ControlPanelForm: TControlPanelForm;

implementation

{$R *.dfm}

uses CardFile, U9200Config;

{$R ..\Common\Images.res}

var
  gMainThreadID: DWORD;

procedure TControlPanelForm.ABtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.ABtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        FDisplaySelect := 'A';
        UpdateLamp(ALed, LAMP_ON);
        UpdateLamp(BLed, LAMP_OFF);
        UpdateLamp(CLed, LAMP_OFF);
        UpdateLamp(DLed, LAMP_OFF);
        UpdateLamp(ELed, LAMP_OFF);
    end;
end;

procedure TControlPanelForm.AddrClick(Sender: TObject);
begin
    ToggleSwitch(TSpeedButton(Sender));
end;

procedure TControlPanelForm.AlterBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    if ((usPowerOn in FSystem.State) and
        ((FSystem.CPU.State * [ucsSingleStep, ucsHalted, ucsStalled]) <> [])) then
    begin
        if (FSystem.CPU.RestrictAlter) then
            FSystem.Memory.StoreByte(4, FDataRegister)
        else
            FSystem.Memory.StoreByte(GetMemoryAddress, FDataRegister);
    end;
end;

procedure TControlPanelForm.AlterBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
end;

procedure TControlPanelForm.BBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.BBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        FDisplaySelect := 'B';
        UpdateLamp(ALed, LAMP_OFF);
        UpdateLamp(BLed, LAMP_ON);
        UpdateLamp(CLed, LAMP_OFF);
        UpdateLamp(DLed, LAMP_OFF);
        UpdateLamp(ELed, LAMP_OFF);
    end;
end;

procedure TControlPanelForm.BtnClick(Sender: TObject);
begin
    ToggleSwitch(TSpeedButton(Sender));
end;

procedure TControlPanelForm.CBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.CBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        FDisplaySelect := 'C';
        UpdateLamp(ALed, LAMP_OFF);
        UpdateLamp(BLed, LAMP_OFF);
        UpdateLamp(CLed, LAMP_ON);
        UpdateLamp(DLed, LAMP_OFF);
        UpdateLamp(ELed, LAMP_OFF);
    end;
end;

procedure TControlPanelForm.ClearBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    FClearDown := True;
    MomentaryOn(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        LampTest(LAMP_ON);
end;

procedure TControlPanelForm.ClearBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        LampTest(LAMP_OFF);
        FDisplaySelect := ' ';
        FDisplayRegister := 0;
        FSystem.Clear;
        FSystem.SingleStep(FInstOn);
        CPUErrorLbl.Visible := False;
    end;
    FClearDown := False;
end;

constructor TControlPanelForm.Create(AOwner: TComponent);
var
    i, j: Integer;
begin
    inherited;
    gConfig.Load(Config);
    FDisplaySelect := ' ';
    FDisplayRegister := 0;
    FSystem := TU9200.Create;
    FSystem.OnLampChange := DoLampChange;
    FSystem.OnExecuteInstruction := DoExecuteInstruction;
    FSystem.OnFetchInstruction := DoFetchInstruction;
    FSystem.OnHalt := DoHalt;
    FSystem.OnError := DoError;
    FReaderFrame := TU9200ReaderFrame.Create(Self, FSystem.Reader);
    FReaderFrame.Parent := ReaderPanel;
    FReaderFrame.Align := alClient;
    FPunchFrame := TU9200PunchFrame.Create(Self, FSystem.Punch);
    FPunchFrame.Parent := PunchPanel;
    FPunchFrame.Align := alClient;
    FPrinterFrame := TU9200PrinterFrame.Create(Self, FSystem.Printer);
    FPrinterFrame.Parent := PrinterPanel;
    FPrinterFrame.Align := alClient;
    TapePeriphPage.Caption := Format('Tape (Channel %d)', [gConfig.TapeChannel]);
    FTapePanels[0] := TapePanel1;
    FTapePanels[1] := TapePanel2;
    FTapePanels[2] := TapePanel3;
    FTapePanels[3] := TapePanel4;
    FTapePanels[4] := TapePanel5;
    FTapePanels[5] := TapePanel6;
    FTapePanels[6] := TapePanel7;
    FTapePanels[7] := TapePanel8;
    i := Low(FTapeFrames);
    for j := 1 to gConfig.TapeCount do
    begin
        if (i <= High(FTapeFrames)) then
        begin
            FTapeFrames[i] := TU9200TapeFrame.Create(Self, FSystem.Tapes[i]);
            FTapeFrames[i].Name := Format('U9200TapeFrame%d', [i]);
            FTapeFrames[i].Parent := FTapePanels[i];
            FTapeFrames[i].Align := alClient;
            Inc(i);
        end;
    end;
    PeripheralPages.ActivePage := CardPeriphPage;
    FDebugger := TU92Debugger.Create(Self);
    FDebugger.Initialize(FSystem.CPU, FSystem.Memory);
end;

procedure TControlPanelForm.CycleBtnClick(Sender: TObject);
begin
    ToggleSwitch(TSpeedButton(Sender));
    FCycleDown := (not FCycleDown);
end;

procedure TControlPanelForm.DataClick(Sender: TObject);
begin
    ToggleSwitch(TSpeedButton(Sender));
    FDataRegister := GetDataEntry;
end;

procedure TControlPanelForm.DBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.DBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        FDisplaySelect := 'D';
        UpdateLamp(ALed, LAMP_OFF);
        UpdateLamp(BLed, LAMP_OFF);
        UpdateLamp(CLed, LAMP_OFF);
        UpdateLamp(DLed, LAMP_ON);
        UpdateLamp(ELed, LAMP_OFF);
    end;
end;

destructor TControlPanelForm.Destroy;
begin
    FreeAndNil(FSystem);
    inherited;
end;

procedure TControlPanelForm.DisplayBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    if ((usPowerOn in FSystem.State) and
        ((FSystem.CPU.State * [ucsSingleStep, ucsHalted, ucsStalled]) <> [])) then
    begin
        FDisplayRegister := (FDisplayRegister shl 8) or FSystem.Memory.FetchByte(GetMemoryAddress);
        SetDisplay(FDisplayRegister);
    end;
end;

procedure TControlPanelForm.DisplayBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
end;

procedure TControlPanelForm.DoError(Sender: TObject; E: Exception);
begin
    UpdateLamp(ProcAbnLed, LAMP_ON);
    CPUErrorLbl.Caption := E.Message;
    CPUErrorLbl.Visible := True;
end;

procedure TControlPanelForm.DoExecuteInstruction(Sender: TObject);
begin
    FDisplayRegister := FSystem.Memory.MIR1[[ucsProcessor]];
    Application.ProcessMessages;
end;

procedure TControlPanelForm.DoFetchInstruction(Sender: TObject);
begin
end;

procedure TControlPanelForm.DoHalt(Sender: TObject);
begin
    FDisplayRegister := FSystem.CPU.HPRCode;
    Application.ProcessMessages;
end;

procedure TControlPanelForm.DoLampChange(Sender: TObject; lamp, state: Integer);
begin
    // Turn a particular lamp on or off depending on state
    case lamp of
      LAMP_POWER:       UpdateLamp(PowerLed, state);
      LAMP_PRINTER_ABN: UpdateLamp(PrinterAbnLed, state);
      LAMP_READER_ABN:  UpdateLamp(ReaderAbnLed, state);
      LAMP_PUNCH_ABN:   UpdateLamp(PunchAbnLed, state);
      LAMP_RUN:         UpdateLamp(RunLed, state);
      LAMP_STOP:        UpdateLamp(StopLed, state);
      LAMP_PROC_ABN:    UpdateLamp(ProcAbnLed, state);
      LAMP_TEST:        UpdateLamp(TestLed, state);
    end;
end;

procedure TControlPanelForm.EBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.EBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        FDisplaySelect := 'D';
        UpdateLamp(ALed, LAMP_OFF);
        UpdateLamp(BLed, LAMP_OFF);
        UpdateLamp(CLed, LAMP_OFF);
        UpdateLamp(DLed, LAMP_OFF);
        UpdateLamp(ELed, LAMP_ON);
    end;
end;

procedure TControlPanelForm.FormShow(Sender: TObject);
begin
    gMainThreadID := getCurrentThreadId;
    // Load the images for the various switches and LEDs on the form
    FGreenLedOff := TBitmap.Create;
    FGreenLedOff.LoadFromResourceName(hInstance, 'GREEN_LED_OFF');
    FGreenLedOn := TBitmap.Create;
    FGreenLedOn.LoadFromResourceName(hInstance, 'GREEN_LED_ON');
    FRedLedOff := TBitmap.Create;
    FRedLedOff.LoadFromResourceName(hInstance, 'RED_LED_OFF');
    FRedLedOn := TBitmap.Create;
    FRedLedOn.LoadFromResourceName(hInstance, 'RED_LED_ON');
    FSwitchOff := TBitmap.Create;
    FSwitchOff.LoadFromResourceName(hInstance, 'SWITCH_OFF');
    FSwitchOn := TBitmap.Create;
    FSwitchOn.LoadFromResourceName(hInstance, 'SWITCH_ON');
    FWhiteLedOff := TBitmap.Create;
    FWhiteLedOff.LoadFromResourceName(hInstance, 'WHITE_LED_OFF');
    FWhiteLedOn := TBitmap.Create;
    FWhiteLedOn.LoadFromResourceName(hInstance, 'WHITE_LED_ON');
end;

function TControlPanelForm.GetDataEntry: Byte;
begin
    Result := Data7Btn.Tag shl 7 or
              Data6Btn.Tag shl 6 or
              Data5Btn.Tag shl 5 or
              Data4Btn.Tag shl 4 or
              Data3Btn.Tag shl 3 or
              Data2Btn.Tag shl 2 or
              Data1Btn.Tag shl 1 or
              Data0Btn.Tag;
end;

function TControlPanelForm.GetMemoryAddress: Integer;
begin
    Result := Addr14Btn.Tag shl 14 or
              Addr13Btn.Tag shl 13 or
              Addr12Btn.Tag shl 12 or
              Addr11Btn.Tag shl 11 or
              Addr10Btn.Tag shl 10 or
              Addr9Btn.Tag shl 9 or
              Addr8Btn.Tag shl 8 or
              Addr7Btn.Tag shl 7 or
              Addr6Btn.Tag shl 6 or
              Addr5Btn.Tag shl 5 or
              Addr4Btn.Tag shl 4 or
              Addr3Btn.Tag shl 3 or
              Addr2Btn.Tag shl 2 or
              Addr1Btn.Tag shl 1 or
              Addr0Btn.Tag;
end;

procedure TControlPanelForm.InstBtnClick(Sender: TObject);
var
    switch: TSpeedButton;
begin
    switch := TSpeedButton(Sender);
    ToggleSwitch(switch);
    FInstOn := (switch.Tag = 1);
    FSystem.SingleStep(FInstOn);
end;

procedure TControlPanelForm.LampTest(onOff: Integer);
var
    i: Integer;
    ctrl: TControl;
begin
    // Turn all LEDs on or off depending on onOff
    for i := 0 to ControlCount - 1 do
    begin
        ctrl := Controls[i];
        if (ctrl is TImage) then
            UpdateLamp(TImage(ctrl), onOff);
    end;
end;

procedure TControlPanelForm.Load;
var
    dev: Byte;
    pc: Smallint;
begin
    if (usPowerOn in FSystem.State) then
    begin
        dev := GetDataEntry;
        if (dev = $80) then
        begin
            if (LoadPgmDlg.Execute) then
                FSystem.Load(LoadPgmDlg.FileName);
        end else
        begin
            FSystem.Load(dev);
        end;
        pc := FSystem.Memory.FAP[[ucsIO]];
        FDisplayRegister := FSystem.Memory.FetchHalfword(pc);
    end;
end;

procedure TControlPanelForm.LoadBtnClick(Sender: TObject);
begin
    FLoadDown := ToggleSwitch(TSpeedButton(Sender));
end;

procedure TControlPanelForm.MomentaryOff(btn: TSpeedButton);
begin
    btn.Glyph := FSwitchOff;
end;

procedure TControlPanelForm.MomentaryOn(btn: TSpeedButton);
begin
    btn.Glyph := FSwitchOn;
end;

procedure TControlPanelForm.OffBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    FSystem.PowerOff;
end;

procedure TControlPanelForm.OffBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
end;

procedure TControlPanelForm.OnBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.OnBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOff in FSystem.State) then
    begin
        LampTest(LAMP_OFF);
        FSystem.PowerOn;
        FSystem.SingleStep(FInstOn);
        FSystem.Reader.OnLine := (not FReaderOfflineDown);
        FSystem.Punch.OnLine := (not FPunchOfflineDown);
        FSystem.Printer.OnLine := (not FPrinterOfflineDown);
    end;
end;

procedure TControlPanelForm.OpReqBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    if ((usPowerOn in FSystem.State) and (FSystem.OpReq.Enabled)) then
    begin
        FSystem.Memory.StoreByte(5, FDataRegister);
        FSystem.OpReq.OpReqPressed;
    end;
end;

procedure TControlPanelForm.OpReqBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PrinterOfflineBtnClick(Sender: TObject);
begin
    FPrinterOfflineDown := ToggleSwitch(TSpeedButton(Sender));
    FSystem.Printer.OnLine := (not FPrinterOfflineDown);
end;

procedure TControlPanelForm.PrinterSpaceBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PrinterSpaceBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Printer.Feed;
end;

procedure TControlPanelForm.ProcBtnClick(Sender: TObject);
begin
    FProcDown := ToggleSwitch(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PunchClearBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PunchClearBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Punch.Clear;
end;

procedure TControlPanelForm.PunchContinuousBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    FPunchContinuousDown := True;
end;

procedure TControlPanelForm.PunchContinuousBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    FPunchContinuousDown := False;
end;

procedure TControlPanelForm.PunchFeedBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PunchFeedBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    Application.ProcessMessages;
    if ((usPowerOn in FSystem.State) and  (FSystem.Punch.OnLine)) then
        FSystem.Punch.Feed;
end;

procedure TControlPanelForm.PunchOfflineBtnClick(Sender: TObject);
begin
    FPunchOfflineDown := ToggleSwitch(TSpeedButton(Sender));
    FSystem.Punch.OnLine := (not FPunchOfflineDown);
end;

procedure TControlPanelForm.ReaderClrBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.ReaderClrBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Reader.Clear;
end;

procedure TControlPanelForm.ReaderContinuousBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
    FReaderContinuousDown := True;
end;

procedure TControlPanelForm.ReaderContinuousBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    FReaderContinuousDown := False;
end;

procedure TControlPanelForm.ReaderFeedBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.ReaderFeedBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    Application.ProcessMessages;
    if ((usPowerOn in FSystem.State) and (FSystem.Reader.OnLine)) then
        FSystem.Reader.Feed;
end;

procedure TControlPanelForm.ReaderOfflineBtnClick(Sender: TObject);
begin
    FReaderOfflineDown := ToggleSwitch(TSpeedButton(Sender));
    FSystem.Reader.OnLine := (not FReaderOfflineDown);
end;

procedure TControlPanelForm.RefreshTimerTimer(Sender: TObject);
begin
    if (usPowerOn in FSystem.State) then
    begin
        UpdateLamps;
        if (FReaderContinuousDown and (not FSystem.Reader.OnLine) and FSystem.Reader.HasCardToFeed) then
            FSystem.Reader.Feed;
        if (FPunchContinuousDown and (not FSystem.Punch.OnLine) and FSystem.Punch.HasCardToFeed) then
            FSystem.Punch.Feed;
    end;
end;

procedure TControlPanelForm.SetDisplay(value: Integer);
begin
    UpdateLamp(Display0Led, (value and $1) <> 0);
    UpdateLamp(Display1Led, (value and $2) <> 0);
    UpdateLamp(Display2Led, (value and $4) <> 0);
    UpdateLamp(Display3Led, (value and $8) <> 0);
    UpdateLamp(Display4Led, (value and $10) <> 0);
    UpdateLamp(Display5Led, (value and $20) <> 0);
    UpdateLamp(Display6Led, (value and $40) <> 0);
    UpdateLamp(Display7Led, (value and $80) <> 0);
    UpdateLamp(Display8Led, (value and $100) <> 0);
    UpdateLamp(Display9Led, (value and $200) <> 0);
    UpdateLamp(Display10Led, (value and $400) <> 0);
    UpdateLamp(Display11Led, (value and $800) <> 0);
    UpdateLamp(Display12Led, (value and $1000) <> 0);
    UpdateLamp(Display13Led, (value and $2000) <> 0);
    UpdateLamp(Display14Led, (value and $4000) <> 0);
    UpdateLamp(Display15Led, (value and $8000) <> 0);
end;

procedure TControlPanelForm.ChannelClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.ChannelClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Mux.Clear;
end;

procedure TControlPanelForm.PrinterClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PrinterClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Printer.Clear;
end;

procedure TControlPanelForm.PrinterHomeBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.PrinterHomeBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
        FSystem.Printer.Home;
end;

procedure TControlPanelForm.StartBtnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
    MomentaryOn(TSpeedButton(Sender));
end;

procedure TControlPanelForm.StartBtnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
    sw: TAurStopWatch;
begin
    MomentaryOff(TSpeedButton(Sender));
    if (usPowerOn in FSystem.State) then
    begin
        if (FLoadDown) then
        begin
            Load;
        end else
        begin
            sw := TAurStopWatch.Create;
            try
                sw.Start;
                if (FCycleDown) then
                    FSystem.CPU.OnDebug := FDebugger.DoDebug
                else
                    FSystem.CPU.OnDebug := nil;
                FSystem.CPU.Start;
                sw.Stop;
//                ShowMessageFmt('Elapsed = %d', [sw.ElapsedMS]);
            finally
                sw.Free;
            end;
        end;
    end;
end;

function TControlPanelForm.ToggleSwitch(btn: TSpeedButton): Boolean;
// Returns True if button down
begin
    with btn do
    begin
        if (Tag = 0) then
        begin
            Glyph.Assign(FSwitchOn);
            Tag := 1;
        end else
        begin
            Glyph.Assign(FSwitchOff);
            Tag := 0;
        end;
        Result := (Tag = 1);
    end;
end;

procedure TControlPanelForm.UpdateLamp(led: TImage; onOff: Integer);
begin
    if (onOff = LAMP_ON) then
    begin
        with led do
        begin
            case Tag of
              LC_WHITE:     Picture.Bitmap := FWhiteLedOn;
              LC_GREEN:     Picture.Bitmap := FGreenLedOn;
              LC_RED:       Picture.Bitmap := FRedLedOn;
            end;
        end;
    end else
    begin
        with led do
        begin
            case Tag of
              LC_WHITE:     Picture.Bitmap := FWhiteLedOff;
              LC_GREEN:     Picture.Bitmap := FGreenLedOff;
              LC_RED:       Picture.Bitmap := FRedLedOff;
            end;
        end;
    end;
end;

procedure TControlPanelForm.UpdateLamp(led: TImage; onOff: Boolean);
begin
    if (onOff) then
        UpdateLamp(led, LAMP_ON)
    else
        UpdateLamp(led, LAMP_OFF);
end;

procedure TControlPanelForm.UpdateLamps;
begin
    if (not (usPowerOn in FSystem.State)) then
    begin
        LampTest(LAMP_OFF);
        Exit;
    end;
    if (FClearDown) then
        Exit;
    // Update the condition of all lamps based on the system state
    UpdateLamp(PowerLed, usPowerOn in FSystem.State);
    UpdateLamp(ReaderAbnLed, (not FSystem.Reader.OnLine) or (not FSystem.Reader.HasCardToFeed));
    UpdateLamp(PunchAbnLed, (not FSystem.Punch.OnLine) or (not FSystem.Punch.HasCardToFeed));
    UpdateLamp(PrinterAbnLed, (not FSystem.Printer.OnLine));
    UpdateLamp(RunLed, ((usPowerOn in FSystem.State) and
                       ((FSystem.CPU.State * [ucsHalted, ucsStalled]) = [])));
    UpdateLamp(StopLed, (FSystem.CPU.State * [ucsSingleStep, ucsHalted, ucsStalled, ucsError]) <> []);
    UpdateLamp(ProcAbnLed, ucsError in FSystem.CPU.State);
    UpdateLamp(OpReqLed, FSystem.OpReq.Enabled);
    SetDisplay(FDisplayRegister);
    // Set the display mode specific lamps
    UpdateLamp(Indicator0Led, False);
    UpdateLamp(Indicator1Led, False);
    UpdateLamp(Indicator2Led, False);
    UpdateLamp(Indicator3Led, False);
    if (FProcDown) then
    begin
        case FDisplaySelect of
          'A':
          begin
            UpdateLamp(Indicator1Led, FSystem.CPU.RestrictAlter);
          end;
          'B':
          begin
            UpdateLamp(Indicator2LeD, ucsIO in FSystem.CPU.State);
          end;
          'D':
          begin
            UpdateLamp(Indicator0Led, FSystem.Memory.ASCII[FSystem.CPU.State]);
          end;
        end;
    end;
end;

end.
