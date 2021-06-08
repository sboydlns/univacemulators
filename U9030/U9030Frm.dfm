object U9030Form: TU9030Form
  Left = 0
  Top = 0
  Caption = 'Sperry*Univac 90/30 Emulator'
  ClientHeight = 527
  ClientWidth = 556
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Courier New'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    556
    527)
  PixelsPerInch = 96
  TextHeight = 14
  object StepBtn: TButton
    Left = 273
    Top = 501
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Step'
    TabOrder = 6
    OnClick = StepBtnClick
  end
  object StopBtn: TButton
    Left = 199
    Top = 501
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Stop'
    TabOrder = 4
    OnClick = StopBtnClick
  end
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 556
    Height = 456
    Align = alTop
    TabOrder = 1
    DesignSize = (
      556
      456)
    object Label1: TLabel
      Left = 12
      Top = 25
      Width = 21
      Height = 14
      Caption = 'PSW'
    end
    object Label2: TLabel
      Left = 51
      Top = 6
      Width = 245
      Height = 14
      Caption = 'T I K A R S E M IC L C B D E S ADDR'
    end
    object Label3: TLabel
      Left = 12
      Top = 51
      Width = 28
      Height = 14
      Caption = 'INST'
    end
    object StateLbl: TLabel
      Left = 336
      Top = 439
      Width = 217
      Height = 14
      Anchors = [akLeft, akRight, akBottom]
      AutoSize = False
      Caption = 'StateLbl'
    end
    object Label4: TLabel
      Left = 362
      Top = 6
      Width = 70
      Height = 14
      Caption = 'Supervisor'
    end
    object Label5: TLabel
      Left = 468
      Top = 6
      Width = 49
      Height = 14
      Caption = 'Program'
    end
    object Label6: TLabel
      Left = 336
      Top = 26
      Width = 7
      Height = 14
      Caption = '0'
    end
    object Label7: TLabel
      Left = 336
      Top = 51
      Width = 7
      Height = 14
      Caption = '1'
    end
    object Label8: TLabel
      Left = 336
      Top = 77
      Width = 7
      Height = 14
      Caption = '2'
    end
    object Label9: TLabel
      Left = 336
      Top = 102
      Width = 7
      Height = 14
      Caption = '3'
    end
    object Label10: TLabel
      Left = 336
      Top = 129
      Width = 7
      Height = 14
      Caption = '4'
    end
    object Label11: TLabel
      Left = 336
      Top = 154
      Width = 7
      Height = 14
      Caption = '5'
    end
    object Label12: TLabel
      Left = 336
      Top = 180
      Width = 7
      Height = 14
      Caption = '6'
    end
    object Label13: TLabel
      Left = 336
      Top = 205
      Width = 7
      Height = 14
      Caption = '7'
    end
    object Label14: TLabel
      Left = 336
      Top = 236
      Width = 7
      Height = 14
      Caption = '8'
    end
    object Label15: TLabel
      Left = 336
      Top = 261
      Width = 7
      Height = 14
      Caption = '9'
    end
    object Label16: TLabel
      Left = 336
      Top = 287
      Width = 14
      Height = 14
      Caption = '10'
    end
    object Label17: TLabel
      Left = 336
      Top = 312
      Width = 14
      Height = 14
      Caption = '11'
    end
    object Label18: TLabel
      Left = 336
      Top = 339
      Width = 14
      Height = 14
      Caption = '12'
    end
    object Label19: TLabel
      Left = 336
      Top = 364
      Width = 14
      Height = 14
      Caption = '13'
    end
    object Label20: TLabel
      Left = 336
      Top = 390
      Width = 14
      Height = 14
      Caption = '14'
    end
    object Label21: TLabel
      Left = 336
      Top = 415
      Width = 14
      Height = 14
      Caption = '15'
    end
    object Label22: TLabel
      Left = 447
      Top = 415
      Width = 14
      Height = 14
      Caption = '15'
    end
    object Label23: TLabel
      Left = 447
      Top = 390
      Width = 14
      Height = 14
      Caption = '14'
    end
    object Label24: TLabel
      Left = 447
      Top = 364
      Width = 14
      Height = 14
      Caption = '13'
    end
    object Label25: TLabel
      Left = 447
      Top = 339
      Width = 14
      Height = 14
      Caption = '12'
    end
    object Label26: TLabel
      Left = 447
      Top = 312
      Width = 14
      Height = 14
      Caption = '11'
    end
    object Label27: TLabel
      Left = 447
      Top = 287
      Width = 14
      Height = 14
      Caption = '10'
    end
    object Label28: TLabel
      Left = 447
      Top = 261
      Width = 7
      Height = 14
      Caption = '9'
    end
    object Label29: TLabel
      Left = 447
      Top = 236
      Width = 7
      Height = 14
      Caption = '8'
    end
    object Label30: TLabel
      Left = 447
      Top = 205
      Width = 7
      Height = 14
      Caption = '7'
    end
    object Label31: TLabel
      Left = 447
      Top = 180
      Width = 7
      Height = 14
      Caption = '6'
    end
    object Label32: TLabel
      Left = 447
      Top = 154
      Width = 7
      Height = 14
      Caption = '5'
    end
    object Label33: TLabel
      Left = 447
      Top = 129
      Width = 7
      Height = 14
      Caption = '4'
    end
    object Label34: TLabel
      Left = 447
      Top = 102
      Width = 7
      Height = 14
      Caption = '3'
    end
    object Label35: TLabel
      Left = 447
      Top = 77
      Width = 7
      Height = 14
      Caption = '2'
    end
    object Label36: TLabel
      Left = 447
      Top = 51
      Width = 7
      Height = 14
      Caption = '1'
    end
    object Label37: TLabel
      Left = 447
      Top = 26
      Width = 7
      Height = 14
      Caption = '0'
    end
    object Label38: TLabel
      Left = 12
      Top = 77
      Width = 21
      Height = 14
      Caption = 'REL'
    end
    object Bevel1: TBevel
      Left = 12
      Top = 114
      Width = 307
      Height = 49
      Shape = bsFrame
    end
    object Label39: TLabel
      Left = 18
      Top = 106
      Width = 49
      Height = 14
      Caption = 'PRINTER'
      Transparent = False
    end
    object PSWEdt: TEdit
      Left = 46
      Top = 22
      Width = 273
      Height = 22
      TabOrder = 0
    end
    object InstEdt: TEdit
      Left = 46
      Top = 48
      Width = 101
      Height = 22
      TabOrder = 1
    end
    object SR0: TEdit
      Left = 358
      Top = 22
      Width = 74
      Height = 22
      TabOrder = 2
    end
    object SR1: TEdit
      Left = 358
      Top = 48
      Width = 74
      Height = 22
      TabOrder = 3
    end
    object SR2: TEdit
      Left = 358
      Top = 74
      Width = 74
      Height = 22
      TabOrder = 4
    end
    object SR3: TEdit
      Left = 358
      Top = 100
      Width = 74
      Height = 22
      TabOrder = 5
    end
    object SR4: TEdit
      Left = 358
      Top = 126
      Width = 74
      Height = 22
      TabOrder = 6
    end
    object SR5: TEdit
      Left = 358
      Top = 152
      Width = 74
      Height = 22
      TabOrder = 7
    end
    object SR6: TEdit
      Left = 358
      Top = 178
      Width = 74
      Height = 22
      TabOrder = 8
    end
    object SR7: TEdit
      Left = 358
      Top = 204
      Width = 74
      Height = 22
      TabOrder = 9
    end
    object SR8: TEdit
      Left = 358
      Top = 230
      Width = 74
      Height = 22
      TabOrder = 10
    end
    object SR9: TEdit
      Left = 358
      Top = 256
      Width = 74
      Height = 22
      TabOrder = 11
    end
    object SR10: TEdit
      Left = 358
      Top = 282
      Width = 74
      Height = 22
      TabOrder = 12
    end
    object SR11: TEdit
      Left = 358
      Top = 308
      Width = 74
      Height = 22
      TabOrder = 13
    end
    object SR12: TEdit
      Left = 358
      Top = 334
      Width = 74
      Height = 22
      TabOrder = 14
    end
    object SR13: TEdit
      Left = 358
      Top = 360
      Width = 74
      Height = 22
      TabOrder = 15
    end
    object SR14: TEdit
      Left = 358
      Top = 386
      Width = 74
      Height = 22
      TabOrder = 16
    end
    object SR15: TEdit
      Left = 358
      Top = 412
      Width = 74
      Height = 22
      TabOrder = 17
    end
    object PR0: TEdit
      Left = 465
      Top = 22
      Width = 74
      Height = 22
      TabOrder = 18
    end
    object PR1: TEdit
      Left = 465
      Top = 48
      Width = 74
      Height = 22
      TabOrder = 19
    end
    object PR2: TEdit
      Left = 465
      Top = 74
      Width = 74
      Height = 22
      TabOrder = 20
    end
    object Pr3: TEdit
      Left = 465
      Top = 100
      Width = 74
      Height = 22
      TabOrder = 21
    end
    object PR4: TEdit
      Left = 465
      Top = 126
      Width = 74
      Height = 22
      TabOrder = 22
    end
    object PR5: TEdit
      Left = 465
      Top = 152
      Width = 74
      Height = 22
      TabOrder = 23
    end
    object PR6: TEdit
      Left = 465
      Top = 178
      Width = 74
      Height = 22
      TabOrder = 24
    end
    object PR7: TEdit
      Left = 465
      Top = 204
      Width = 74
      Height = 22
      TabOrder = 25
    end
    object PR8: TEdit
      Left = 465
      Top = 230
      Width = 74
      Height = 22
      TabOrder = 26
    end
    object PR9: TEdit
      Left = 465
      Top = 256
      Width = 74
      Height = 22
      TabOrder = 27
    end
    object PR10: TEdit
      Left = 465
      Top = 282
      Width = 74
      Height = 22
      TabOrder = 28
    end
    object PR11: TEdit
      Left = 465
      Top = 308
      Width = 74
      Height = 22
      TabOrder = 29
    end
    object PR12: TEdit
      Left = 465
      Top = 334
      Width = 74
      Height = 22
      TabOrder = 30
    end
    object PR13: TEdit
      Left = 465
      Top = 360
      Width = 74
      Height = 22
      TabOrder = 31
    end
    object PR14: TEdit
      Left = 465
      Top = 386
      Width = 74
      Height = 22
      TabOrder = 32
    end
    object PR15: TEdit
      Left = 465
      Top = 412
      Width = 74
      Height = 22
      TabOrder = 33
    end
    object RelRegEdt: TEdit
      Left = 46
      Top = 74
      Width = 101
      Height = 22
      TabOrder = 34
    end
    object PrtBrkptBtn: TButton
      Left = 18
      Top = 130
      Width = 75
      Height = 25
      Caption = 'Brkpt'
      TabOrder = 35
    end
    object PrtFileNameEdt: TEdit
      Left = 95
      Top = 131
      Width = 187
      Height = 22
      TabOrder = 36
    end
    object PrtBrowseBtn: TButton
      Left = 284
      Top = 130
      Width = 26
      Height = 25
      Caption = '...'
      TabOrder = 37
    end
  end
  object CpuTestBtn: TButton
    Left = 421
    Top = 501
    Width = 75
    Height = 25
    Caption = 'CPU Test'
    TabOrder = 0
    OnClick = CpuTestBtnClick
  end
  object BootBtn: TButton
    Left = 0
    Top = 501
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'IPL'
    TabOrder = 2
    OnClick = BootBtnClick
  end
  object BootDvcEdt: TEdit
    Left = 81
    Top = 502
    Width = 40
    Height = 22
    Anchors = [akLeft, akBottom]
    CharCase = ecUpperCase
    TabOrder = 3
    Text = '300'
  end
  object RunBtn: TButton
    Left = 125
    Top = 501
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Run'
    TabOrder = 5
    OnClick = RunBtnClick
  end
  object DebugBtn: TButton
    Left = 347
    Top = 501
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Debug'
    TabOrder = 7
    OnClick = DebugBtnClick
  end
  object DisableTimerBox: TCheckBox
    Left = 6
    Top = 464
    Width = 115
    Height = 17
    Caption = 'Inhibit Timer'
    TabOrder = 8
    OnClick = DisableTimerBoxClick
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 228
    Top = 48
  end
end
