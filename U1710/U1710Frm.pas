unit U1710Frm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.ImgList, Vcl.Samples.Gauges,
  CardFile, EmulatorTypes, Vcl.MPlayer, Vcl.Imaging.jpeg;

type
  TPunchVerifyState = ( pvsPunch = 0, pvsVerify);

  TLoadState = ( lsProgram = 0, lsOff, lsData );

  TAutoState = ( asAuto = 0, asManual );

  TProgramState = ( psProgram1 = 0, psProgram2 );

  TU1710ShiftState = ( ssAlpha, ssNumeric );

  TU1710Form = class(TForm)
    ControlPanel: TPanel;
    ProgramBtn: TTrackBar;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    AutoBtn: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    LoadBtn: TTrackBar;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    PunchVerifyBtn: TTrackBar;
    Label9: TLabel;
    AlphaNumLed: TImage;
    KeyboardPanel: TPanel;
    StatusBar2: TStatusBar;
    InterlockLed: TImage;
    Panel1: TPanel;
    ColumnIndicator: TLabel;
    Input: TGauge;
    AuxInput: TGauge;
    LoadInputBtn: TButton;
    LoadAuxBtn: TButton;
    Output: TGauge;
    SaveOutputBtn: TButton;
    SelectOutput: TGauge;
    SaveSelectBtn: TButton;
    Panel2: TPanel;
    VisibleStation: TLabel;
    InputLbl: TLabel;
    AuxInputLbl: TLabel;
    SelectOutputLbl: TLabel;
    OutputLbl: TLabel;
    EmptyInputBtn: TButton;
    EmptyAuxBtn: TButton;
    EmptyOutputBtn: TButton;
    EmptySelectBtn: TButton;
    SaveDlg: TSaveDialog;
    Player: TMediaPlayer;
    Image1: TImage;
    Label10: TLabel;
    Label11: TLabel;
    NonMatchLed: TImage;
    Label12: TLabel;
    ErrorLed: TImage;
    Label13: TLabel;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure SaveOutputBtnClick(Sender: TObject);
    procedure EmptyOutputBtnClick(Sender: TObject);
    procedure EmptySelectBtnClick(Sender: TObject);
    procedure EmptyAuxBtnClick(Sender: TObject);
    procedure EmptyInputBtnClick(Sender: TObject);
    procedure LoadAuxBtnClick(Sender: TObject);
    procedure LoadInputBtnClick(Sender: TObject);
    procedure PunchVerifyBtnChange(Sender: TObject);
    procedure LoadBtnChange(Sender: TObject);
    procedure AutoBtnChange(Sender: TObject);
    procedure ProgramBtnChange(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    FGreenLedOff: TBitmap;
    FGreenLedOn: TBitmap;
    FRedLedOff: TBitmap;
    FRedLedOn: TBitmap;
    FYellowLedOff: TBitmap;
    FYellowLedOn: TBitmap;

    FAuxCount: Integer;
    FAuxFile: TCardFileStream;
    FCardFileDir: String;
    FColumn: Integer;
    FCurrentFile: TCardFileStream;
    FData: TCardRec;
    FMultiPunch: Boolean;
    FMultiPunchBfr: WORD;
    FInputCount: Integer;
    FInputFiles: TCardFileList;
    FInterlocked: Boolean;
    FOutputCount: Integer;
    FOutputFile: TCardFileStream;
    FProgram: TCardRec;
    FSelectCount: Integer;
    FSelectFile: TCardFileStream;
    FShiftState: TU1710ShiftState;
    FCurrentCard: TCardRec;
    FCurrentCardLoaded: Boolean;
    FVerifyError: Boolean;
    FNonMatchCount: Integer;

    function AlphaPunch: Word;
    function AutoManualState: TAutoState;
    procedure Backspace;
    procedure Clear;
    procedure ColumnDuplicate;
    procedure Duplicate;
    function DuplicatePunch: Word;
    procedure Eject;
    procedure Feed;
    function FieldPunch: Word;
    function LoadCard(var bfr: TCardRec): Boolean;
    procedure LoadCurrentCard;
    procedure LoadData;
    procedure LoadProgram;
    function LoadState: TLoadState;
    procedure PlayErrorSound;
    procedure PlayFeedSound;
    function ProgramState: TProgramState;
    procedure Punch;
    function PunchVerifyState: TPunchVerifyState;
    function RJEnd: Integer;
    function RJEndPunch: Word;
    procedure RJMinus;
    procedure RJPlus;
    function RJStart: Integer;
    function RJStartPunch: Word;
    procedure SaveSelectCard(const bfr: TCardRec);
    procedure SetAlphaMode;
    procedure SetColumn(col: Integer);
    procedure SetError(Value: Boolean);
    procedure SetInterlocked(Value: Boolean);
    procedure SetNonMatch(Value: Boolean);
    procedure SetNumericMode;
    procedure SetShiftLED;
    procedure SetVisibleStation(cr: TCardRec);
    function ShiftState: TU1710ShiftState;
    procedure Skip;
    function SkipPunch: Word;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  U1710Form: TU1710Form;

implementation

{$R *.dfm}

uses LoadCardsFrm;
{$R ..\Common\Images.res}

const
  //
  P_12 = $8000;
  P_11 = $4000;
  P_0  = $2000;
  P_1  = $1000;
  P_2  = $0800;
  P_3  = $0400;
  P_4  = $0200;
  P_5  = $0100;
  P_6  = $0080;
  P_7  = $0040;
  P_8  = $0020;
  P_9  = $0010;
  //
  K_MULTIPUNCH = $80;
  K_CORR = $81;
  K_EJECT = $82;
  K_PLUSRJ = $83;
  K_FEED = $84;
  K_MINUSRJ = $85;
  K_CLEAR = $86;
  K_SKIP = $87;
  K_HOME = $88;
  K_COLDUP = $89;
  K_NUMERIC = $8A;
  K_ALPHA = $8B;
  K_DUP = $8C;
  K_BS = $8D;
  K_12_2_8 = P_12 + P_2 + P_8;
  K_0_8_2 = P_0 + P_8 + P_2;
  K_12_0 = P_12 + P_0;
  K_11_7_8 = P_11 + P_7 + P_8;
  K_11_0 = P_11 + P_0;
  //
  RK_CAPS = 'a';
  RK_LSHIFT = 'b';
  RK_RSHIFT = 'c';

type
  T1710KeyXlate = record
  public
    RawKey: AnsiChar;
    XlateKey: Word;
  end;

const
  alphaXlate: array [0..48] of T1710KeyXlate = (
    ( RawKey: ' '; XlateKey: Byte(' '); ),
    ( RawKey: '1'; XlateKey: K_MULTIPUNCH; ),
    ( RawKey: '2'; XlateKey: Byte('@'); ),
    ( RawKey: '3'; XlateKey: Byte('%'); ),
    ( RawKey: '4'; XlateKey: Byte('*'); ),
    ( RawKey: '5'; XlateKey: Byte('<'); ),
    ( RawKey: '6'; XlateKey: K_BS; ),
    ( RawKey: '7'; XlateKey: K_DUP; ),
    ( RawKey: '8'; XlateKey: Byte('-'); ),
    ( RawKey: '9'; XlateKey: Byte('/'); ),
    ( RawKey: '0'; XlateKey: K_CORR; ),
    ( RawKey: '-'; XlateKey: K_EJECT; ),
    ( RawKey: '='; XlateKey: K_PLUSRJ; ),
    ( RawKey: 'Q'; XlateKey: Byte('Q'); ),
    ( RawKey: 'W'; XlateKey: Byte('W'); ),
    ( RawKey: 'E'; XlateKey: Byte('E'); ),
    ( RawKey: 'R'; XlateKey: Byte('R'); ),
    ( RawKey: 'T'; XlateKey: Byte('T'); ),
    ( RawKey: 'Y'; XlateKey: Byte('Y'); ),
    ( RawKey: 'U'; XlateKey: Byte('U'); ),
    ( RawKey: 'I'; XlateKey: Byte('I'); ),
    ( RawKey: 'O'; XlateKey: Byte('O'); ),
    ( RawKey: 'P'; XlateKey: Byte('P'); ),
    ( RawKey: '['; XlateKey: K_FEED; ),
    ( RawKey: ']'; XlateKey: K_MINUSRJ; ),
    ( RawKey: RK_CAPS; XlateKey: K_CLEAR; ),
    ( RawKey: 'A'; XlateKey: Byte('A'); ),
    ( RawKey: 'S'; XlateKey: Byte('S'); ),
    ( RawKey: 'D'; XlateKey: Byte('D'); ),
    ( RawKey: 'F'; XlateKey: Byte('F'); ),
    ( RawKey: 'G'; XlateKey: Byte('G'); ),
    ( RawKey: 'H'; XlateKey: Byte('H'); ),
    ( RawKey: 'J'; XlateKey: Byte('J'); ),
    ( RawKey: 'K'; XlateKey: Byte('K'); ),
    ( RawKey: 'L'; XlateKey: Byte('L'); ),
    ( RawKey: ';'; XlateKey: K_SKIP; ),
    ( RawKey: ''''; XlateKey: K_HOME; ),
    ( RawKey: RK_LSHIFT; XlateKey: K_NUMERIC; ),
    ( RawKey: 'Z'; XlateKey: Byte('Z'); ),
    ( RawKey: 'X'; XlateKey: Byte('X'); ),
    ( RawKey: 'C'; XlateKey: Byte('C'); ),
    ( RawKey: 'V'; XlateKey: Byte('V'); ),
    ( RawKey: 'B'; XlateKey: Byte('B'); ),
    ( RawKey: 'N'; XlateKey: Byte('N'); ),
    ( RawKey: 'M'; XlateKey: Byte('M'); ),
    ( RawKey: ','; XlateKey: Byte(','); ),
    ( RawKey: '.'; XlateKey: Byte('.'); ),
    ( RawKey: '/'; XlateKey: K_COLDUP; ),
    ( RawKey: RK_RSHIFT; XlateKey: K_ALPHA; )
  );

  numericXlate: array [0..48] of T1710KeyXlate = (
    ( RawKey: ' '; XlateKey: Byte(' '); ),
    ( RawKey: '1'; XlateKey: K_MULTIPUNCH; ),
    ( RawKey: '2'; XlateKey: Byte('#'); ),
    ( RawKey: '3'; XlateKey: Byte(','); ),
    ( RawKey: '4'; XlateKey: Byte('$'); ),
    ( RawKey: '5'; XlateKey: Byte('.'); ),
    ( RawKey: '6'; XlateKey: K_BS; ),
    ( RawKey: '7'; XlateKey: K_DUP; ),
    ( RawKey: '8'; XlateKey: Byte('-'); ),
    ( RawKey: '9'; XlateKey: Byte('0'); ),
    ( RawKey: '0'; XlateKey: K_CORR; ),
    ( RawKey: '-'; XlateKey: K_EJECT; ),
    ( RawKey: '='; XlateKey: K_PLUSRJ; ),
    ( RawKey: 'Q'; XlateKey: Byte('+'); ),
    ( RawKey: 'W'; XlateKey: Byte('-'); ),
    ( RawKey: 'E'; XlateKey: Byte(')'); ),
    ( RawKey: 'R'; XlateKey: K_12_2_8; ),
    ( RawKey: 'T'; XlateKey: K_0_8_2; ),
    ( RawKey: 'Y'; XlateKey: Byte('|'); ),
    ( RawKey: 'U'; XlateKey: Byte('1'); ),
    ( RawKey: 'I'; XlateKey: Byte('2'); ),
    ( RawKey: 'O'; XlateKey: Byte('3'); ),
    ( RawKey: 'P'; XlateKey: Byte('&'); ),
    ( RawKey: '['; XlateKey: K_FEED; ),
    ( RawKey: ']'; XlateKey: K_MINUSRJ; ),
    ( RawKey: RK_CAPS; XlateKey: K_CLEAR; ),
    ( RawKey: 'A'; XlateKey: K_12_0; ),
    ( RawKey: 'S'; XlateKey: Byte('>'); ),
    ( RawKey: 'D'; XlateKey: Byte(':'); ),
    ( RawKey: 'F'; XlateKey: Byte(';'); ),
    ( RawKey: 'G'; XlateKey: K_11_7_8; ),
    ( RawKey: 'H'; XlateKey: Byte(''''); ),
    ( RawKey: 'J'; XlateKey: Byte('4'); ),
    ( RawKey: 'K'; XlateKey: Byte('5'); ),
    ( RawKey: 'L'; XlateKey: Byte('6'); ),
    ( RawKey: ';'; XlateKey: K_SKIP; ),
    ( RawKey: ''''; XlateKey: K_HOME; ),
    ( RawKey: RK_LSHIFT; XlateKey: K_NUMERIC; ),
    ( RawKey: 'Z'; XlateKey: K_11_0; ),
    ( RawKey: 'X'; XlateKey: Byte('?'); ),
    ( RawKey: 'C'; XlateKey: Byte('"'); ),
    ( RawKey: 'V'; XlateKey: Byte('='); ),
    ( RawKey: 'B'; XlateKey: Byte('!'); ),
    ( RawKey: 'N'; XlateKey: Byte('('); ),
    ( RawKey: 'M'; XlateKey: Byte('7'); ),
    ( RawKey: ','; XlateKey: Byte('8'); ),
    ( RawKey: '.'; XlateKey: Byte('9'); ),
    ( RawKey: '/'; XlateKey: K_COLDUP; ),
    ( RawKey: RK_RSHIFT; XlateKey: K_ALPHA; )
  );

{ TU1710Form }

function TU1710Form.AlphaPunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_1
    else
        Result := P_7;
end;

procedure TU1710Form.AutoBtnChange(Sender: TObject);
begin
    ActiveControl := nil;
end;

function TU1710Form.AutoManualState: TAutoState;
begin
    Result := TAutoState(AutoBtn.Position);
end;

procedure TU1710Form.Backspace;
begin
    if (FColumn > 1) then
        SetColumn(FColumn - 1);
end;

procedure TU1710Form.Clear;
begin
    SetInterlocked(False);
end;

procedure TU1710Form.ColumnDuplicate;
begin
    if (FColumn < 80) then
        SetColumn(FColumn + 1);
end;

constructor TU1710Form.Create(AOwner: TComponent);
begin
    inherited;
    FCardFileDir := UserDataDir;
    FShiftState := ssNumeric;
    FInputFiles := TCardFileList.Create;
    FOutputFile := TCardFileStream.Create(Format('%s\output.h16', [FCardFileDir]), fmCreate);
    FSelectFile := TCardFileStream.Create(Format('%s\select.h16', [FCardFileDir]), fmCreate)
end;

procedure TU1710Form.Duplicate;
var
    i: Integer;
begin
    if (AutoManualState = asManual) then
    begin
        if (FColumn < 80) then
            SetColumn(FColumn + 1)
        else
            SetColumn(1);
    end else
    begin
        i := FColumn;
        repeat
            Inc(i);
        until ((i > 80) or ((FProgram.ColumnAsWord[i] and FieldPunch) = 0));
        if (i > 80) then
            Feed
        else
            SetColumn(i);
    end;
end;

function TU1710Form.DuplicatePunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_0
    else
        Result := P_6;
end;

procedure TU1710Form.Eject;
begin
    PlayFeedSound;
    SaveSelectCard(FData);
    FCurrentCardLoaded := False;
    SetColumn(1);
end;

procedure TU1710Form.EmptyAuxBtnClick(Sender: TObject);
begin
    FreeAndNil(FAuxFile);
    FAuxCount := 0;
    AuxInput.Progress := 0;
    AuxInputLbl.Caption := '0';
    ActiveControl := nil;
end;

procedure TU1710Form.EmptyInputBtnClick(Sender: TObject);
begin
    FInputFiles.Clear;
    FreeAndNil(FCurrentFile);
    FInputCount := 0;
    Input.Progress := 0;
    InputLbl.Caption := '0';
    ActiveControl := nil;
end;

procedure TU1710Form.EmptyOutputBtnClick(Sender: TObject);
begin
    FOutputFile.Free;
    FOutputFile := TCardFileStream.Create(Format('%s\output.h16', [FCardFileDir]), fmCreate);
    FOutputCount := 0;
    Output.Progress := 0;
    OutputLbl.Caption := '0';
    ActiveControl := nil;
end;

procedure TU1710Form.EmptySelectBtnClick(Sender: TObject);
begin
    FSelectFile.Free;
    FSelectFile := TCardFileStream.Create(Format('%s\select.h16', [FCardFileDir]), fmCreate);
    FSelectCount := 0;
    SelectOutputLbl.Caption := '0';
    ActiveControl := nil;
end;

procedure TU1710Form.Feed;
begin
    if (FCurrentCardLoaded) then
    begin
        PlayFeedSound;
        Punch;
    end else
        SetInterlocked(True);
    case LoadState of
      lsOff:        LoadCurrentCard;
      lsProgram:    LoadProgram;
      lsData:       LoadData;
    end;
    SetColumn(1);
end;

function TU1710Form.FieldPunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_12
    else
        Result := P_4;
end;

procedure TU1710Form.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    c: AnsiChar;
    b: Byte;
    w: WORD;
    xk: WORD;
    kx: T1710KeyXlate;
begin
    if (FInterlocked and (Key <> VK_CAPITAL)) then
    begin
        PlayErrorSound;
        Exit;
    end;

    case Key of
      VK_CAPITAL:
      begin
        c := RK_CAPS;
      end;
      VK_SHIFT:
      begin
        if (GetAsyncKeyState(VK_LSHIFT) <> 0) then
            c := RK_LSHIFT
        else
            c := RK_RSHIFT;
      end;
      VK_RETURN:                                // use as alternate feed
      begin
        c := '[';
      end;
      VK_BACK:                                  // use as alternate backspace
      begin
        c := '6';
      end;
      VK_HOME:                                  // use as alternate home
      begin
        c := '''';
      end;
      VK_END:                                   // use as alternate skip
      begin
        c := ';'
      end;
      VK_NEXT:                                  // use as alternate eject
      begin
        c := '-';
      end;
      VK_F1:                                    // use as alternate +RJ
      begin
        c := '=';
      end;
      VK_F2:                                    // use as alternate -RJ
      begin
        c := '''';
      end;
      VK_F3:                                    // use as alternate COL DUP
      begin
        c := '/';
      end;
      else
      begin
        c := AnsiChar(MapVirtualKeyA(Key, MAPVK_VK_TO_CHAR));
      end;
    end;

    xk := 0;
    if (ShiftState = ssNumeric) then
    begin
        for kx in numericXlate do
        begin
            if (kx.RawKey = c) then
            begin
                xk := kx.XlateKey;
                Break;
            end;
        end;
    end else
    begin
        for kx in alphaXlate do
        begin
            if (kx.RawKey = c) then
            begin
                xk := kx.XlateKey;
                Break;
            end;
        end;
    end;

    case xk of
      0:
      begin
        PlayErrorSound;
      end;
      K_MULTIPUNCH:
      begin
        FMultiPunch := True;
        FMultiPunchBfr := 0;
      end;
      K_CORR:
      begin
        ;{ TODO : Implement CORR key. Not possible until I find something that tellsm me what it does. }
      end;
      K_EJECT:
      begin
        Eject;
      end;
      K_PLUSRJ:
      begin
        RJPlus;
      end;
      K_FEED:
      begin
        Feed;
      end;
      K_MINUSRJ:
      begin
        RJMinus;
      end;
      K_CLEAR:
      begin
        Clear;
      end;
      K_SKIP:
      begin
        Skip;
      end;
      K_HOME:
      begin
        SetColumn(1);
      end;
      K_COLDUP:
      begin
        ColumnDuplicate;
      end;
      K_NUMERIC:
      begin
       SetNumericMode;
      end;
      K_ALPHA:
      begin
        SetAlphaMode;
      end;
      K_DUP:
      begin
        Duplicate;
      end;
      K_BS:
      begin
        Backspace;
      end;
      else
      begin
        if ((xk and $FF00) = 0) then
        begin
            // Plain ascii character
            b := TCodeTranslator.AsciiToHollerith8(AnsiChar(xk));
            w := TCodeTranslator.Hollerith8ToHollerith12(b);
        end else
            // Special multi-punch characters
            w := xk;
        if (FMultiPunch) then
            FMultiPunchBfr := FMultiPunchBfr or w
        else
        begin
            if (PunchVerifyState = pvsPunch) then
            begin
                FData.ColumnAsWord[FColumn] := w;
                if (FColumn < 80) then
                    SetColumn(FColumn + 1)
                else
                    Feed;
                SetVisibleStation(FData);
            end else
            begin
                if (FNonMatchCount >= 3) then
                begin
                    FData.ColumnAsWord[FColumn] := w;
                    SetNonMatch(False);
                end else
                begin
                    if (FData.ColumnAsWord[FColumn] = w) then
                        SetNonMatch(False)
                    else
                        SetNonMatch(True);
                end;
                if (FColumn < 80) then
                    SetColumn(FColumn + 1)
                else if (not FVerifyError) then
                    Feed;
                SetVisibleStation(FCurrentCard);
            end;
        end;
      end;
    end;
end;

procedure TU1710Form.FormKeyPress(Sender: TObject; var Key: Char);
begin
    ;
end;

procedure TU1710Form.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    c: AnsiChar;
begin
    c := AnsiChar(MapVirtualKeyA(Key, MAPVK_VK_TO_CHAR));
    case c of
      '1':
      begin
        FMultiPunch := False;
        if (PunchVerifyState = pvsPunch) then
        begin
            FData.ColumnAsWord[FColumn] := FMultiPunchBfr;
            if (FColumn < 80) then
                SetColumn(FColumn + 1)
            else
                Feed;
            SetNonMatch(False);
            SetVisibleStation(FData);
        end else
        begin
            if (FNonMatchCount >= 3) then
            begin
                FData.ColumnAsWord[FColumn] := FMultiPunchBfr;
                SetNonMatch(False);
            end else
            begin
                if (FData.ColumnAsWord[FColumn] = FMultiPunchBfr) then
                    SetNonMatch(False)
                else
                    SetNonMatch(True);
            end;
            if (FColumn < 80) then
                SetColumn(FColumn + 1)
            else if (not FVerifyError) then
                Feed;
            SetVisibleStation(FCurrentCard);
        end;
      end;
    end;
end;

procedure TU1710Form.FormShow(Sender: TObject);
var
    exeDir: String;
    fname: String;
begin
    FGreenLedOff := TBitmap.Create;
    FGreenLedOff.LoadFromResourceName(hInstance, 'GREEN_LED_OFF');
    FGreenLedOn := TBitmap.Create;
    FGreenLedOn.LoadFromResourceName(hInstance, 'GREEN_LED_ON');
    FRedLedOff := TBitmap.Create;
    FRedLedOff.LoadFromResourceName(hInstance, 'RED_LED_OFF');
    FRedLedOn := TBitmap.Create;
    FRedLedOn.LoadFromResourceName(hInstance, 'RED_LED_ON');
    FYellowLedOff := TBitmap.Create;
    FYellowLedOff.LoadFromResourceName(hInstance, 'YELLOW_LED_OFF');
    FYellowLedOn := TBitmap.Create;
    FYellowLedOn.LoadFromResourceName(hInstance, 'YELLOW_LED_ON');
    SetNumericMode;
    SetInterlocked(False);
    SetNonMatch(False);
    FShiftState := ssNumeric;
    SetColumn(1);
    VisibleStation.Caption := '';
    exeDir := ExtractFilePath(Application.ExeName);
    fname := Format('%slayered_low_beep.mp3', [exeDir]);
    if (not FileExists(fname)) then
    begin
        fname := Format('%s..\..\layered_low_beep.mp3', [exeDir]);
        if (not FileExists(fname)) then
            fname := '';
    end;
    try
        Player.FileName := fname;
        Player.Open;
    except
        ;
    end;
end;

procedure TU1710Form.LoadAuxBtnClick(Sender: TObject);
begin
    if (LoadCardsForm.ShowModal <> mrOk) then
        Exit;
    if (LoadCardsForm.FileName <> '') then
        FAuxFile := TCardFileStream.Create(LoadCardsForm.FileName, fmOpenRead)
    else
        FAuxFile := TBlankCardStream.Create(LoadCardsForm.NumCards);
    AuxInput.Progress := 1;
    AuxInputLbl.Caption := '1';
    FAuxCount := 1;
    ActiveControl := nil;
end;

procedure TU1710Form.LoadBtnChange(Sender: TObject);
begin
    ActiveControl := nil;
end;

function TU1710Form.LoadCard(var bfr: TCardRec): Boolean;
begin
    Result := False;
    if (Assigned(FAuxFile) and (FAuxCount > 0)) then
    begin
        FAuxFile.ReadRaw(bfr);
        Dec(FAuxCount);
        AuxInputLbl.Caption := IntToStr(FAuxCount);
        AuxInput.Progress := FAuxCount;
        if (FAuxCount <= 0) then
            FreeAndNil(FAuxFile);
        Result := True;
        Exit;
    end;
    if ((not Assigned(FCurrentFile)) or FCurrentFile.Eof) then
    begin
        if (Assigned(FCurrentFile)) then
        begin
            FInputFiles.Delete(0);
            FreeAndNil(FCurrentFile);
        end;
        if (FInputFiles.Count > 0) then
        begin
            if (FInputFiles[0].FileName <> '') then
                FCurrentFile := TCardFileStream.Create(FInputFiles[0].FileName, fmOpenRead)
            else
                FCurrentFile := TBlankCardStream.Create(FInputFiles[0].BlankCards);
        end;
    end;
    if (Assigned(FCurrentFile)) then
    begin
        FCurrentFile.ReadRaw(bfr);
        Dec(FInputCount);
        InputLbl.Caption := IntToStr(FInputCount);
        Input.Progress := FInputCount;
        Result := True;
    end;
end;

procedure TU1710Form.LoadCurrentCard;
begin
    if (LoadCard(FCurrentCard)) then
    begin
        FCurrentCardLoaded := True;
        if (PunchVerifyState = pvsVerify) then
            FData.Assign(FCurrentCard);
    end;
end;

procedure TU1710Form.LoadData;
begin
    if (LoadCard(FData)) then
    begin
        SaveSelectCard(FData);
        FCurrentCard.Clear;
        SetVisibleStation(FCurrentCard);
    end;
end;

procedure TU1710Form.LoadInputBtnClick(Sender: TObject);
var
    cfr:TCardFileRec;
    fin: TCardFileStream;
begin
    if (LoadCardsForm.ShowModal <> mrOk) then
        Exit;
    if (LoadCardsForm.FileName <> '') then
    begin
        cfr.FileName := LoadCardsForm.FileName;
        FInputFiles.Add(cfr);
        fin := TCardFileStream.Create(cfr.FileName, fmOpenRead);
        try
            FInputCount := FInputCount + fin.RecordCount;
        finally
            fin.Free;
        end;
    end else
    begin
        cfr.BlankCards := LoadCardsForm.NumCards;
        FInputFiles.Add(cfr);
        FInputCount := FInputCount + cfr.BlankCards;
    end;
    InputLbl.Caption := IntToStr(FInputCount);
    Input.Progress := FInputCount;
    ActiveControl := nil;
end;

procedure TU1710Form.LoadProgram;
begin
    if (LoadCard(FProgram)) then
    begin
        SaveSelectCard(FProgram);
        FCurrentCard.Clear;
        SetVisibleStation(FCurrentCard);
    end;
end;

function TU1710Form.LoadState: TLoadState;
begin
    Result := TLoadState(LoadBtn.Position);
end;

procedure TU1710Form.PlayErrorSound;
var
    exeDir: String;
    fname: String;
begin
    try
        exeDir := ExtractFilePath(Application.ExeName);
        Player.Close;
        fname := Format('%slayered_low_beep.mp3', [exeDir]);
        if (not FileExists(fname)) then
        begin
            fname := Format('%s..\..\layered_low_beep.mp3', [exeDir]);
            if (not FileExists(fname)) then
                fname := '';
        end;
        Player.FileName := fname;
        Player.Open;
        Player.Play;
    except
        ;
    end;
end;

procedure TU1710Form.PlayFeedSound;
var
    exeDir: String;
    fname: String;
begin
    try
        exeDir := ExtractFilePath(Application.ExeName);
        Player.Close;
        fname := Format('%sempty_bell_beep.wav', [exeDir]);
        if (not FileExists(fname)) then
        begin
            fname := Format('%s..\..\empty_bell_beep.wav', [exeDir]);
            if (not FileExists(fname)) then
                fname := '';
        end;
        Player.FileName := fname;
        Player.Open;
        Player.Play;
    except
        ;
    end;
end;

procedure TU1710Form.ProgramBtnChange(Sender: TObject);
begin
    ActiveControl := nil;
end;

function TU1710Form.ProgramState: TProgramState;
begin
    Result := TProgramState(ProgramBtn.Position);
end;

procedure TU1710Form.Punch;
begin
    if (PunchVerifyState = pvsPunch) then
        FOutputFile.Merge(FCurrentCard, FData)
    else
    begin
        if (FVerifyError) then
        begin
            if (FAuxCount > 0) then
            begin
                SaveSelectCard(FCurrentCard);                   // save error card to select stacker
                LoadCard(FCurrentCard);                         // get 1st card in aux feed.
                FOutputFile.Merge(FCurrentCard, FData);         // save corrected card to output stacker
            end else
                SetInterlocked(True);
        end else
            FOutputFile.WriteRaw(FCurrentCard);                 // save original card to output stacker
    end;
    Inc(FOutputCount);
    OutputLbl.Caption := IntToStr(FOutputCount);
    Output.Progress := FOutputCount;
    FCurrentCardLoaded := False;
end;

procedure TU1710Form.PunchVerifyBtnChange(Sender: TObject);
begin
    ActiveControl := nil;
end;

function TU1710Form.PunchVerifyState: TPunchVerifyState;
begin
    Result := TPunchVerifyState(PunchVerifyBtn.Position);
end;

function TU1710Form.RJEnd: Integer;
begin
    try
        if (AutoManualState <> asAuto) then
            raise Exception.Create('Not auto mode');
        Result := FColumn;
        // Forwards until we either find an RJ end punch or the end of the current field
        while ((Result <= 80) and ((FProgram.ColumnAsWord[Result] and RJEndPunch) <> RJEndPunch)) do
        begin
            if ((FProgram.ColumnAsWord[Result] and FieldPunch) = 0) then
                raise Exception.Create('Not an RJ field');
            Inc(Result);
        end;
        if (Result > 80) then
            raise Exception.Create('Not an RJ field');
    except
        PlayErrorSound;
        Result := 0;
    end;
end;

function TU1710Form.RJEndPunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_12 or P_11 or P_1
    else
        Result := P_4 or P_5 or P_7;
end;

procedure TU1710Form.RJMinus;
var
    start, stop: Integer;
begin
    start := RJStart;
    stop := RJEnd;
    if ((start = 0) or (stop = 0)) then
        Exit;
    // Set the minus sign on the last digit entered
    if ((FColumn - 1) >= start) then
        FData.ColumnAsWord[FColumn - 1] := FData.ColumnAsWord[FColumn - 1] or P_11;
    RJPlus;
end;

procedure TU1710Form.RJPlus;
var
    start, stop, i: Integer;
begin
    start := RJStart;
    stop := RJEnd;
    if ((start = 0) or (stop = 0)) then
        Exit;
    i := FColumn - 1;
    // Right justify data entered so far.
    while (i <= start) do
    begin
        FData.ColumnAsWord[stop] := FData.ColumnAsWord[i];
        Dec(i);
        Dec(stop);
    end;
    // Zero fill on the left
    while (stop <= start) do
    begin
        FData.ColumnAsWord[stop] := P_0;
        Dec(stop);
    end;
    // Skip to the beginning of the next field. Using Duplicate rather than Skip
    // because Duplicate doesn't space fill.
    Duplicate;
end;

function TU1710Form.RJStart: Integer;
begin
    try
        if (AutoManualState <> asAuto) then
            raise Exception.Create('Not auto mode');
        Result := FColumn;
        // Backwards until we find the start of the current field
        while ((Result > 0) and ((FProgram.ColumnAsWord[Result] and FieldPunch) <> 0)) do
            Dec(Result);
        if ((Result <= 0) or ((FProgram.ColumnAsWord[Result] and RJStartPunch) <> RJStartPunch)) then
            raise Exception.Create('Not an RJ field');
    except
        PlayErrorSound;
        Result := 0;
    end;
end;

function TU1710Form.RJStartPunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_11 or P_1
    else
        Result := P_5 or P_7;
end;

procedure TU1710Form.SaveOutputBtnClick(Sender: TObject);
begin
    if (not SaveDlg.Execute) then
        Exit;
    FOutputFile.SaveToFile(SaveDlg.FileName);
    EmptyOutputBtnClick(nil);
    ActiveControl := nil;
end;

procedure TU1710Form.SaveSelectCard(const bfr: TCardRec);
begin
    FSelectFile.WriteRaw(bfr);
    Inc(FSelectCount);
    SelectOutputLbl.Caption := IntToStr(FSelectCount);
    SelectOutput.Progress := FSelectCount;
end;

procedure TU1710Form.SetAlphaMode;
begin
    FShiftState := ssAlpha;
    SetShiftLED;
end;

procedure TU1710Form.SetColumn(col: Integer);
begin
    FColumn := col;
    ColumnIndicator.Caption := Format('%2.2d', [FColumn]);
    SetShiftLED;
    if (AutoManualState = asAuto) then
    begin
        if ((FProgram.ColumnAsWord[FColumn] and DuplicatePunch) <> 0) then
            Duplicate
        else if (((FProgram.ColumnAsWord[FColumn] and RJStartPunch) <> RJStartPunch) and
                 ((FProgram.ColumnAsWord[FColumn] and SkipPunch) <> 0)) then
            Skip;
    end;
end;

procedure TU1710Form.SetError(Value: Boolean);
begin
    if (Value) then
    begin
        FVerifyError := True;
        ErrorLed.Picture.Bitmap := FRedLedOn;
    end else
        ErrorLed.Picture.Bitmap := FRedLedOff;
end;

procedure TU1710Form.SetInterlocked(Value: Boolean);
begin
    FInterlocked := Value;
    if (FInterlocked) then
    begin
        InterlockLed.Picture.Bitmap := FRedLedOn;
        PlayErrorSound;
    end else
        InterlockLed.Picture.Bitmap := FRedLedOff;
end;

procedure TU1710Form.SetNonMatch(Value: Boolean);
begin
    if (Value) then
    begin
        NonMatchLed.Picture.Bitmap := FYellowLedOn;
        Inc(FNonMatchCount);
        if (FNonMatchCount >= 3) then
            SetError(True)
        else
            SetInterlocked(True);
    end else
    begin
        NonMatchLed.Picture.Bitmap := FYellowLedOff;
        SetError(False);
        FNonMatchCount := 0;
    end;
end;

procedure TU1710Form.SetNumericMode;
begin
    FShiftState := ssNumeric;
    SetShiftLED;
end;

procedure TU1710Form.SetShiftLED;
begin
    if (ShiftState = ssNumeric) then
        AlphaNumLed.Picture.Bitmap := FGreenLedOff
    else
        AlphaNumLed.Picture.Bitmap := FGreenLedOn;
end;

procedure TU1710Form.SetVisibleStation(cr: TCardRec);
var
    i: Integer;
    c: AnsiChar;
    s: String;
begin
    s := '';
    for i := 1 to 80 do
    begin
        c := TCodeTranslator.Hollerith8ToAscii(TCodeTranslator.Hollerith12ToHollerith8(cr.ColumnAsWord[i]));
        s := s + Char(c);
    end;
    VisibleStation.Caption := TrimRight(s);
end;

function TU1710Form.ShiftState: TU1710ShiftState;
begin
    if (AutoManualState = asManual) then
    begin
        Result := FShiftState;
    end else
    begin
        if ((FProgram.ColumnAsWord[FColumn] and AlphaPunch) <> 0) then
            Result := ssAlpha
        else
            Result := FShiftState;
    end;
end;

procedure TU1710Form.Skip;
var
    i: Integer;
begin
    if (AutoManualState = asManual) then
    begin
        for i := FColumn to 80 do
            FData.ColumnAsWord[i] := 0;
        SetColumn(1);
    end else
    begin
        i := FColumn;
        repeat
            FData.ColumnAsWord[i] := 0;
            Inc(i);
        until ((i > 80) or ((FProgram.ColumnAsWord[i] and FieldPunch) = 0));
        if (i > 80) then
            Feed
        else
            SetColumn(i);
    end;
end;

function TU1710Form.SkipPunch: Word;
begin
    if (ProgramState = psProgram1) then
        Result := P_11
    else
        Result := P_5;
end;

end.
