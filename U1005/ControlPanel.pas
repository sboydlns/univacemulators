unit ControlPanel;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, U1005System, Vcl.ComCtrls,
  U1005ReaderFram, U1005PrinterFram, U1005PunchFram;

type
  TControlPanelFrm = class(TForm)
    ButtonPanel: TPanel;
    Alt1Btn: TSpeedButton;
    Alt3Btn: TSpeedButton;
    Alt4Btn: TSpeedButton;
    LocalLbl: TLabel;
    RemoteLbl: TLabel;
    ForwardLbl: TLabel;
    UnloadLbl: TLabel;
    Alt2Btn: TSpeedButton;
    Label35: TLabel;
    CardPeriphPage: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    StartBtn: TSpeedButton;
    ClearBtn: TSpeedButton;
    FeedBtn: TSpeedButton;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    PowerBtn: TSpeedButton;
    RunBtn: TSpeedButton;
    StopBtn: TSpeedButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ModeSelector: TImage;
    DisplayPanel: TPanel;
    DisplayLed1: TImage;
    DisplayLed2: TImage;
    DisplayLed3: TImage;
    DisplayLed4: TImage;
    DisplayLed5: TImage;
    DisplayLed6: TImage;
    DisplayLed7: TImage;
    DisplayLed8: TImage;
    DisplayLed9: TImage;
    DisplayLed10: TImage;
    DisplayLed11: TImage;
    DisplayLed12: TImage;
    DisplayLed13: TImage;
    DisplayLed14: TImage;
    DisplayLed15: TImage;
    DisplayLed16: TImage;
    DisplayLed17: TImage;
    DisplayLed18: TImage;
    DisplayLed19: TImage;
    DisplayLed20: TImage;
    DisplayLed21: TImage;
    DisplayLed22: TImage;
    DisplayLed23: TImage;
    DisplayLed24: TImage;
    DisplayLed25: TImage;
    DisplayLed26: TImage;
    DisplayLed27: TImage;
    DisplayLed28: TImage;
    DisplayLed29: TImage;
    DisplayLed30: TImage;
    DisplayLed31: TImage;
    DisplayLed32: TImage;
    LedLabel1: TLabel;
    LedLabel2: TLabel;
    LedLabel3: TLabel;
    LedLabel4: TLabel;
    LedLabel5: TLabel;
    LedLabel6: TLabel;
    LedLabel7: TLabel;
    LedLabel8: TLabel;
    LedLabel9: TLabel;
    LedLabel10: TLabel;
    LedLabel11: TLabel;
    LedLabel12: TLabel;
    LedLabel13: TLabel;
    LedLabel14: TLabel;
    LedLabel15: TLabel;
    LedLabel16: TLabel;
    LedLabel17: TLabel;
    LedLabel18: TLabel;
    LedLabel19: TLabel;
    LedLabel20: TLabel;
    LedLabel21: TLabel;
    LedLabel22: TLabel;
    LedLabel23: TLabel;
    LedLabel24: TLabel;
    LedLabel25: TLabel;
    LedLabel26: TLabel;
    LedLabel27: TLabel;
    LedLabel28: TLabel;
    LedLabel29: TLabel;
    LedLabel30: TLabel;
    LedLabel31: TLabel;
    LedLabel32: TLabel;
    RadioGroup1: TRadioGroup;
    Label43: TLabel;
    Display4Btn: TRadioButton;
    Display6Btn: TRadioButton;
    Display8Btn: TRadioButton;
    Display9Btn: TRadioButton;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    PeriperalPages: TPageControl;
    ReaderPage: TTabSheet;
    PunchPage: TTabSheet;
    ReaderPanel: TPanel;
    RefreshTimer: TTimer;
    PrinterPage: TTabSheet;
    PunchPanel: TPanel;
    PrinterPanel: TPanel;
    SystemTypeBtn: TRadioGroup;
    ErrorLabel: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    procedure PowerBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PowerBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt1BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt1BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt2BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt2BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt3BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt3BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt4BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Alt4BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StartBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FeedBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FeedBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RefreshTimerTimer(Sender: TObject);
    procedure RunBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure RunBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure SystemTypeBtnClick(Sender: TObject);
    procedure StopBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure StopBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ModeSelectorClick(Sender: TObject);
    procedure Display4BtnClick(Sender: TObject);
    procedure Display6BtnClick(Sender: TObject);
    procedure Display8BtnClick(Sender: TObject);
    procedure Display9BtnClick(Sender: TObject);
  private
    FGreenOffUp: TBitmap;
    FGreenOnUp: TBitmap;
    FGreenDown: TBitmap;
    FRedOffUp: TBitmap;
    FRedOnUp: TBitmap;
    FRedDown: TBitmap;
    FWhiteOffUp: TBitmap;
    FWhiteOnUp: TBitmap;
    FWhiteDown: TBitmap;
    FWhiteLedOff: TBitmap;
    FWhiteLedOn: TBitmap;
    FModeSelect1: TBitmap;
    FModeSelect2: TBitmap;
    FModeSelect3: TBitmap;
    FModeSelect4: TBitmap;
    FPowerOn: Boolean;
    FModeSelectPosn: Integer;
    FSystem: T1005System;
    FReaderFrame: TU1005ReaderFrame;
    FPunchFrame: TU1005PunchFrame;
    FPrinterFrame: TU1005PrinterFrame;
    procedure CPUError(Sender: TObject; E: Exception);
    procedure Refresh;
    procedure SetLamp(led: TImage; onOff: Cardinal);
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  ControlPanelFrm: TControlPanelFrm;

implementation

{$R *.dfm}
{$R ..\Common\Images.res}

uses U1005Types, U1005Memory;

{ TControlPanelFrm }

procedure TControlPanelFrm.Alt1BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Alt1Btn.Glyph.Assign(FWhiteDown);
end;

procedure TControlPanelFrm.Alt1BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        FSystem.Alt1 := not FSystem.Alt1;
        if (FSystem.Alt1) then
            Alt1Btn.Glyph.Assign(FWhiteOnUp)
        else
            Alt1Btn.Glyph.Assign(FWhiteOffUp);
    end else
        Alt1Btn.Glyph.Assign(FWhiteOffUp);
end;

procedure TControlPanelFrm.Alt2BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Alt2Btn.Glyph.Assign(FWhiteDown);
end;

procedure TControlPanelFrm.Alt2BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        FSystem.Alt2 := not FSystem.Alt2;
        if (FSystem.Alt2) then
            Alt2Btn.Glyph.Assign(FWhiteOnUp)
        else
            Alt2Btn.Glyph.Assign(FWhiteOffUp);
    end else
        Alt2Btn.Glyph.Assign(FWhiteOffUp);
end;

procedure TControlPanelFrm.Alt3BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Alt3Btn.Glyph.Assign(FWhiteDown);
end;

procedure TControlPanelFrm.Alt3BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        FSystem.Alt3 := not FSystem.Alt3;
        if (FSystem.Alt3) then
            Alt3Btn.Glyph.Assign(FWhiteOnUp)
        else
            Alt3Btn.Glyph.Assign(FWhiteOffUp);
    end else
        Alt3Btn.Glyph.Assign(FWhiteOffUp);
end;

procedure TControlPanelFrm.Alt4BtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    Alt4Btn.Glyph.Assign(FWhiteDown);
end;

procedure TControlPanelFrm.Alt4BtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        FSystem.Alt4 := not FSystem.Alt4;
        if (FSystem.Alt4) then
            Alt4Btn.Glyph.Assign(FWhiteOnUp)
        else
            Alt4Btn.Glyph.Assign(FWhiteOffUp);
    end else
        Alt4Btn.Glyph.Assign(FWhiteOffUp);
end;

procedure TControlPanelFrm.ClearBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        ClearBtn.Glyph.Assign(FRedOnUp);
end;

procedure TControlPanelFrm.ClearBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        FSystem.CPU.Clear;
        ErrorLabel.Caption := '';
        ErrorLabel.Visible := False;
    end;
    ClearBtn.Glyph.Assign(FRedOffUp);
end;

procedure TControlPanelFrm.CPUError(Sender: TObject; E: Exception);
begin
    ErrorLabel.Caption := E.Message;
    ErrorLabel.Visible := True;
end;

constructor TControlPanelFrm.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    // Create button glyphs
    FGreenOffUp := TBitmap.Create;
    FGreenOffUp.LoadFromResourceName(hInstance, 'GREEN_TAPE_OFF_UP');
    FGreenOnUp := TBitmap.Create;
    FGreenOnUp.LoadFromResourceName(hInstance, 'GREEN_TAPE_ON_UP');
    FGreenDown := TBitmap.Create;
    FGreenDown.LoadFromResourceName(hInstance, 'GREEN_TAPE_DOWN');
    FRedOffUp := TBitmap.Create;
    FRedOffUp.LoadFromResourceName(hInstance, 'RED_TAPE_OFF_UP');
    FRedOnUp := TBitmap.Create;
    FRedOnUp.LoadFromResourceName(hInstance, 'RED_TAPE_ON_UP');
    FRedDown := TBitmap.Create;
    FRedDown.LoadFromResourceName(hInstance, 'RED_TAPE_DOWN');
    FWhiteOffUp := TBitmap.Create;
    FWhiteOffUp.LoadFromResourceName(hInstance, 'WHITE_TAPE_OFF_UP');
    FWhiteOnUp := TBitmap.Create;
    FWhiteOnUp.LoadFromResourceName(hInstance, 'WHITE_TAPE_ON_UP');
    FWhiteDown := TBitmap.Create;
    FWhiteDown.LoadFromResourceName(hInstance, 'WHITE_TAPE_DOWN');
    FWhiteLedOff := TBitmap.Create;
    FWhiteLedOff.LoadFromResourceName(hInstance, 'WHITE_LED_OFF_BLACK');
    FWhiteLedOn := TBitmap.Create;
    FWhiteLedOn.LoadFromResourceName(hInstance, 'WHITE_LED_ON_BLACK');
    FModeSelect1 := TBitmap.Create;
    FModeSelect1.LoadFromResourceName(hInstance, 'ROTATE_SWITCH1');
    FModeSelect2 := TBitmap.Create;
    FModeSelect2.LoadFromResourceName(hInstance, 'ROTATE_SWITCH2');
    FModeSelect3 := TBitmap.Create;
    FModeSelect3.LoadFromResourceName(hInstance, 'ROTATE_SWITCH3');
    FModeSelect4 := TBitmap.Create;
    FModeSelect4.LoadFromResourceName(hInstance, 'ROTATE_SWITCH4');
    //
    FSystem := T1005System.Create(stComm);
    FSystem.CPU.OnError := CPUError;
    //
    FReaderFrame := TU1005ReaderFrame.Create(Self, FSystem.Reader);
    FReaderFrame.Parent := ReaderPanel;
    FReaderFrame.Align := alClient;
    //
    FPunchFrame := TU1005PunchFrame.Create(Self, FSystem.Punch);
    FPunchFrame.Parent := PunchPanel;
    FPunchFrame.Align := alClient;
    //
    FPrinterFrame := TU1005PrinterFrame.Create(Self, FSystem.Printer);
    FPrinterFrame.Parent := PrinterPanel;
    FPrinterFrame.Align := alClient;
    //
    FModeSelectPosn := 1;
    Display4BtnClick(nil);
end;

procedure TControlPanelFrm.Display4BtnClick(Sender: TObject);
const
    ttl: array [1..32] of String = (
        'HPR', 'FD', 'RJM', 'TJM', 'STK', 'FRM', 'ADV', 'PCH',
        'HLT', 'I1', 'I2', 'I3', 'I4', 'RD', 'PR', 'PCH',
        'SP1', 'SP2', 'SK1', 'SK2', 'SK4', 'ER', 'EP', 'EX',
        '', '', '', '', '', '', '', ''
    );
var
    i: Integer;
    ctl: TLabel;
begin
    if (Display4Btn.Checked) then
    begin
        for i := 1 to 32 do
        begin
            ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
            if (Assigned(ctl)) then
                ctl.Caption := ttl[i];
        end;
    end;
end;

procedure TControlPanelFrm.Display6BtnClick(Sender: TObject);
const
    ttl: array [1..32] of String = (
        'LA', 'LD', 'LPR', 'SA', 'SD', 'SPR', 'SHR', 'SHL',
        'CLR', 'CA', 'CN', 'IC', 'J', 'JL', 'JG', 'JE',
        'JR', 'JX', 'AM', 'AR', 'SM', 'SR', 'MUL', 'DIV',
        'TRL', 'SZS', 'LWS', 'LN', 'SED', 'PTE', 'XF', ''
    );
var
    i: Integer;
    ctl: TLabel;
begin
    if (Display6Btn.Checked) then
    begin
        if (FSystem.SystemType = stFedSys) then
        begin
            for i := 1 to 32 do
            begin
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := ttl[i];
            end;
        end else
        begin
            for i := 1 to 32 do
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := '';
        end;
    end;
end;

procedure TControlPanelFrm.Display8BtnClick(Sender: TObject);
const
    ttl: array [1..32] of String = (
        'MR5', 'MR4', 'MR3', 'MR2', 'MR1', 'MC5', 'MC4', 'MC3',
        'MC2', 'MC1', 'LR5', 'LR4', 'LR3', 'LR2', 'LR1', 'LC5',
        'LC4', 'LC3', 'LC2', 'LC1', 'CCB', 'CCA', 'CBB', 'CBA',
        'CAB', 'CAA', 'ICA', 'ICB', 'ICC', 'ICD', 'OCA', 'OCB'
    );
var
    i: Integer;
    ctl: TLabel;
begin
    if (Display8Btn.Checked) then
    begin
        if (FSystem.SystemType = stFedSys) then
        begin
            for i := 1 to 32 do
            begin
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := ttl[i];
            end;
        end else
        begin
            for i := 1 to 32 do
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := '';
        end;
    end;
end;

procedure TControlPanelFrm.Display9BtnClick(Sender: TObject);
const
    ttl: array [1..32] of String = (
        '', '', '', '', '', '', '', '',
        '', '', '', '', '', '', '', '',
        'I1X', 'I2X', 'I3X', 'I4X', 'I5X', '', '', '',
        '', '', '', '', '', '', '', ''
    );
var
    i: Integer;
    ctl: TLabel;
begin
    if (Display9Btn.Checked) then
    begin
        if (FSystem.SystemType = stFedSys) then
        begin
            for i := 1 to 32 do
            begin
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := ttl[i];
            end;
        end else
        begin
            for i := 1 to 32 do
                ctl := TLabel(FindComponent(Format('LedLabel%d', [i])));
                if (Assigned(ctl)) then
                    ctl.Caption := '';
        end;
    end;
end;

procedure TControlPanelFrm.FeedBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        FeedBtn.Glyph.Assign(FWhiteOnUp);
end;

procedure TControlPanelFrm.FeedBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        FSystem.Reader.Feed;
    FeedBtn.Glyph.Assign(FWhiteOffUp);
end;

procedure TControlPanelFrm.FormShow(Sender: TObject);
begin
    PeriperalPages.ActivePage := ReaderPage;
end;

procedure TControlPanelFrm.ModeSelectorClick(Sender: TObject);
begin
    Inc(FModeSelectPosn);
    if (FModeSelectPosn > 4) then
        FModeSelectPosn := 1;
    FSystem.CPU.SingleStep := False;
    case FModeSelectPosn of
      1:
      begin
        ModeSelector.Picture.Bitmap.Assign(FModeSelect1);
      end;
      2:
      begin
        ModeSelector.Picture.Bitmap.Assign(FModeSelect2);
        FSystem.CPU.SingleStep := True;
      end;
      3:
      begin
        ModeSelector.Picture.Bitmap.Assign(FModeSelect3);
      end;
      4:
      begin
        ModeSelector.Picture.Bitmap.Assign(FModeSelect4);
      end;
    end;
end;

procedure TControlPanelFrm.PowerBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    PowerBtn.Glyph.Assign(FRedDown);
end;

procedure TControlPanelFrm.PowerBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    FPowerOn := not FPowerOn;
    if (FPowerOn) then
    begin
        FSystem.PowerOn;
        PowerBtn.Glyph.Assign(FRedOnUp);
    end else
    begin
        FSystem.PowerOff;
        PowerBtn.Glyph.Assign(FRedOffUp);
    end;
end;

procedure TControlPanelFrm.Refresh;
var
    i: Integer;
    img: TImage;
    bit: Cardinal;
    display: Cardinal;
begin
    if (FPowerOn) then
    begin
        if (Display4Btn.Checked) then
            display := FSystem.Display4
        else if (Display6Btn.Checked) then
            display := FSystem.Display6
        else if (Display8Btn.Checked) then
            display := FSystem.Display8
        else if (Display9Btn.Checked) then
            display := FSystem.Display9
        else
            display := 0;
        begin
            bit := $80000000;
            for i := 1 to 32 do
            begin
                img := TImage(FindComponent(Format('DisplayLed%d', [i])));
                SetLamp(img, display and bit);
                bit := bit shr 1;
            end;
        end;
    end else
    begin
        for i := 1 to 32 do
        begin
            img := TImage(FindComponent(Format('DisplayLed%d', [i])));
            SetLamp(img, 0);
        end;
    end;
end;

procedure TControlPanelFrm.RefreshTimerTimer(Sender: TObject);
begin
    if (FPowerOn) then
        Refresh;
end;

procedure TControlPanelFrm.RunBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        RunBtn.Glyph.Assign(FWhiteOnUp);
end;

procedure TControlPanelFrm.RunBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    try
        if (FPowerOn) then
        begin
            ErrorLabel.Visible := False;
            if (FSystem.SystemType = stFedSys) then
            begin
                if (FSystem.Alt1) then
                begin
                    if ((not FSystem.Alt2) and (not FSystem.Alt3) and (not FSystem.Alt4)) then
                        FSystem.Load
                    else
                        FSystem.Start;
                end else
                begin
                    FSystem.Start;
                end;
            end else
            begin
                FSystem.Start;
                if ((FSystem.CPU.ConditionCodes and CC_INDICATOR1) <> 0) then
                begin
                    if ((FSystem.CPU.ConditionCodes and CC_INDICATOR2) <> 0) then
                    begin
                        ErrorLabel.Caption := 'HALT 3';
                        ErrorLabel.Visible := True;
                    end else
                    begin
                        ErrorLabel.Caption := 'HALT 1';
                        ErrorLabel.Visible := True;
                    end
                end else if ((FSystem.CPU.ConditionCodes and CC_INDICATOR2) <> 0)  then
                begin
                    ErrorLabel.Caption := 'HALT 2';
                    ErrorLabel.Visible := True;
                end;
            end;
        end;
    finally
        RunBtn.Glyph.Assign(FWhiteOffUp);
    end;
end;

procedure TControlPanelFrm.SetLamp(led: TImage; onOff: Cardinal);
begin
    if (onOff <> 0) then
        led.Picture.Bitmap.Assign(FWhiteLedOn)
    else
        led.Picture.Bitmap.Assign(FWhiteLedOff);
end;

procedure TControlPanelFrm.StartBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        StartBtn.Glyph.Assign(FGreenOnUp);
end;

procedure TControlPanelFrm.StartBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
     StartBtn.Glyph.Assign(FGreenOffUp);
end;

procedure TControlPanelFrm.StopBtnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
        StopBtn.Glyph.Assign(FWhiteOnUp);
end;

procedure TControlPanelFrm.StopBtnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    if (FPowerOn) then
    begin
        StopBtn.Glyph.Assign(FWhiteOffUp);
    end;
end;

procedure TControlPanelFrm.SystemTypeBtnClick(Sender: TObject);
begin
    if (SystemTypeBtn.ItemIndex = 0) then
        FSystem.SystemType := stFedSys
    else
        FSystem.SystemType := stComm;
end;

end.
