object U494PanelFrm: TU494PanelFrm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'Univac 494 Virtual Maintenance Panel'
  ClientHeight = 649
  ClientWidth = 924
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MaintenancePanel: TPanel
    Left = 0
    Top = 0
    Width = 493
    Height = 649
    Align = alLeft
    TabOrder = 0
    object Label26: TLabel
      Left = 49
      Top = 185
      Width = 20
      Height = 13
      Caption = 'CPU'
    end
    object Label27: TLabel
      Left = 162
      Top = 185
      Width = 48
      Height = 13
      Caption = 'Arithmetic'
    end
    object Label28: TLabel
      Left = 286
      Top = 185
      Width = 54
      Height = 13
      Caption = 'Exec Index'
    end
    object Label29: TLabel
      Left = 407
      Top = 185
      Width = 53
      Height = 13
      Caption = 'User Index'
    end
    object CpuStatusLbl: TLabel
      Left = 414
      Top = 238
      Width = 63
      Height = 13
      Alignment = taRightJustify
      Caption = 'CpuStatusLbl'
    end
    object Bevel1: TBevel
      Left = 196
      Top = 208
      Width = 281
      Height = 27
      Shape = bsFrame
    end
    object Label15: TLabel
      Left = 208
      Top = 200
      Width = 42
      Height = 13
      Caption = 'Switches'
      Transparent = False
    end
    object AuditMemo: TMemo
      Left = 1
      Top = 259
      Width = 491
      Height = 368
      Align = alBottom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      Lines.Strings = (
        'Type ? for help')
      ParentFont = False
      TabOrder = 0
      WordWrap = False
    end
    object CpuRegPanel: TPanel
      Left = 6
      Top = 0
      Width = 121
      Height = 179
      TabOrder = 1
      object Label1: TLabel
        Left = 8
        Top = 9
        Width = 17
        Height = 13
        Caption = 'IFR'
      end
      object Label2: TLabel
        Left = 8
        Top = 33
        Width = 18
        Height = 13
        Caption = 'PLR'
      end
      object Label3: TLabel
        Left = 8
        Top = 58
        Width = 18
        Height = 13
        Caption = 'RIR'
      end
      object Label5: TLabel
        Left = 8
        Top = 83
        Width = 6
        Height = 13
        Caption = 'P'
      end
      object Label9: TLabel
        Left = 8
        Top = 158
        Width = 35
        Height = 13
        Caption = 'OPRND'
      end
      object Label4: TLabel
        Left = 8
        Top = 108
        Width = 20
        Height = 13
        Caption = 'CSR'
      end
      object Label16: TLabel
        Left = 8
        Top = 133
        Width = 24
        Height = 13
        Caption = 'IASR'
      end
      object IfrEdt: TEdit
        Left = 48
        Top = 6
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 0
      end
      object PlrEdt: TEdit
        Left = 48
        Top = 30
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 1
      end
      object RirEdt: TEdit
        Left = 48
        Top = 55
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 2
      end
      object PEdt: TEdit
        Left = 48
        Top = 80
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 3
      end
      object OperandEdt: TEdit
        Left = 48
        Top = 155
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 4
      end
      object CsrEdt: TEdit
        Left = 48
        Top = 105
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 5
      end
      object IasrEdt: TEdit
        Left = 48
        Top = 130
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 6
      end
    end
    object Panel1: TPanel
      Left = 127
      Top = 0
      Width = 116
      Height = 179
      TabOrder = 2
      object Label6: TLabel
        Left = 16
        Top = 9
        Width = 7
        Height = 13
        Caption = 'A'
      end
      object Label7: TLabel
        Left = 16
        Top = 33
        Width = 8
        Height = 13
        Caption = 'Q'
      end
      object Label8: TLabel
        Left = 16
        Top = 58
        Width = 6
        Height = 13
        Caption = 'X'
      end
      object Label30: TLabel
        Left = 16
        Top = 83
        Width = 6
        Height = 13
        Caption = 'Y'
      end
      object AEdt: TEdit
        Left = 35
        Top = 6
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 0
      end
      object QEdt: TEdit
        Left = 35
        Top = 30
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 1
      end
      object XEdt: TEdit
        Left = 35
        Top = 55
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 2
      end
      object YEdt: TEdit
        Left = 35
        Top = 80
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 3
      end
    end
    object Panel2: TPanel
      Left = 243
      Top = 0
      Width = 121
      Height = 179
      TabOrder = 3
      object Label10: TLabel
        Left = 18
        Top = 9
        Width = 18
        Height = 13
        Caption = 'EB1'
      end
      object Label11: TLabel
        Left = 18
        Top = 33
        Width = 18
        Height = 13
        Caption = 'EB2'
      end
      object Label12: TLabel
        Left = 18
        Top = 58
        Width = 18
        Height = 13
        Caption = 'EB3'
      end
      object Label13: TLabel
        Left = 18
        Top = 82
        Width = 18
        Height = 13
        Caption = 'EB4'
      end
      object Label14: TLabel
        Left = 18
        Top = 107
        Width = 18
        Height = 13
        Caption = 'EB5'
      end
      object Label17: TLabel
        Left = 18
        Top = 132
        Width = 18
        Height = 13
        Caption = 'EB6'
      end
      object Label18: TLabel
        Left = 18
        Top = 158
        Width = 18
        Height = 13
        Caption = 'EB7'
      end
      object ExecB1Edt: TEdit
        Left = 42
        Top = 6
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 0
      end
      object ExecB2Edt: TEdit
        Left = 42
        Top = 30
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 1
      end
      object ExecB3Edt: TEdit
        Left = 42
        Top = 55
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 2
      end
      object ExecB4Edt: TEdit
        Left = 42
        Top = 79
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 3
      end
      object ExecB5Edt: TEdit
        Left = 42
        Top = 104
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 4
      end
      object ExecB6Edt: TEdit
        Left = 42
        Top = 129
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 5
      end
      object ExecB7Edt: TEdit
        Left = 42
        Top = 155
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 6
      end
    end
    object Panel3: TPanel
      Left = 364
      Top = 0
      Width = 121
      Height = 179
      TabOrder = 4
      object Label19: TLabel
        Left = 18
        Top = 9
        Width = 19
        Height = 13
        Caption = 'UB1'
      end
      object Label20: TLabel
        Left = 18
        Top = 33
        Width = 19
        Height = 13
        Caption = 'UB2'
      end
      object Label21: TLabel
        Left = 18
        Top = 58
        Width = 19
        Height = 13
        Caption = 'UB3'
      end
      object Label22: TLabel
        Left = 18
        Top = 82
        Width = 19
        Height = 13
        Caption = 'UB4'
      end
      object Label23: TLabel
        Left = 18
        Top = 107
        Width = 19
        Height = 13
        Caption = 'UB5'
      end
      object Label24: TLabel
        Left = 18
        Top = 132
        Width = 19
        Height = 13
        Caption = 'UB6'
      end
      object Label25: TLabel
        Left = 18
        Top = 156
        Width = 19
        Height = 13
        Caption = 'UB7'
      end
      object UserB1Edt: TEdit
        Left = 42
        Top = 6
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 0
      end
      object UserB2Edt: TEdit
        Left = 42
        Top = 30
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 1
      end
      object UserB3Edt: TEdit
        Left = 42
        Top = 55
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 2
      end
      object UserB4Edt: TEdit
        Left = 42
        Top = 79
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 3
      end
      object UserB5Edt: TEdit
        Left = 42
        Top = 104
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 4
      end
      object UserB6Edt: TEdit
        Left = 42
        Top = 129
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 5
      end
      object UserB7Edt: TEdit
        Left = 42
        Top = 155
        Width = 69
        Height = 21
        Alignment = taRightJustify
        Enabled = False
        MaxLength = 10
        TabOrder = 6
      end
    end
    object InputEdt: TEdit
      Left = 1
      Top = 627
      Width = 491
      Height = 21
      Align = alBottom
      TabOrder = 5
      OnKeyPress = InputEdtKeyPress
    end
    object StartBtn: TButton
      Left = 22
      Top = 206
      Width = 41
      Height = 45
      Caption = 'Start'
      TabOrder = 6
      OnClick = StartBtnClick
    end
    object StopBtn: TButton
      Left = 64
      Top = 206
      Width = 41
      Height = 45
      Caption = 'Stop'
      TabOrder = 7
      OnClick = StopBtnClick
    end
    object Switch1Btn: TCheckBox
      Left = 202
      Top = 213
      Width = 28
      Height = 17
      Caption = '1'
      TabOrder = 8
      OnClick = Switch1BtnClick
    end
    object Switch2Btn: TCheckBox
      Left = 243
      Top = 213
      Width = 28
      Height = 17
      Caption = '2'
      TabOrder = 9
      OnClick = Switch2BtnClick
    end
    object Switch3Btn: TCheckBox
      Left = 284
      Top = 213
      Width = 28
      Height = 17
      Caption = '3'
      TabOrder = 10
      OnClick = Switch3BtnClick
    end
    object Switch4Btn: TCheckBox
      Left = 325
      Top = 213
      Width = 28
      Height = 17
      Caption = '4'
      TabOrder = 11
      OnClick = Switch4BtnClick
    end
    object Switch5Btn: TCheckBox
      Left = 366
      Top = 213
      Width = 28
      Height = 17
      Caption = '5'
      TabOrder = 12
      OnClick = Switch5BtnClick
    end
    object Switch6Btn: TCheckBox
      Left = 407
      Top = 213
      Width = 28
      Height = 17
      Caption = '6'
      TabOrder = 13
      OnClick = Switch6BtnClick
    end
    object Switch7Btn: TCheckBox
      Left = 449
      Top = 213
      Width = 28
      Height = 17
      Caption = '7'
      TabOrder = 14
      OnClick = Switch7BtnClick
    end
    object ClearBtn: TButton
      Left = 106
      Top = 206
      Width = 41
      Height = 45
      Caption = 'Clear'
      TabOrder = 15
      OnClick = ClearBtnClick
    end
    object InterruptsCheck: TCheckBox
      Left = 202
      Top = 238
      Width = 114
      Height = 17
      Caption = 'Interrupt Lockout'
      TabOrder = 16
    end
  end
  object PeripheralsPanel: TPanel
    Left = 493
    Top = 0
    Width = 431
    Height = 649
    Align = alClient
    TabOrder = 1
    object PeripheralPages: TPageControl
      Left = 1
      Top = 1
      Width = 429
      Height = 647
      ActivePage = PaperTapePage
      Align = alClient
      TabOrder = 0
      object CardPage: TTabSheet
        Caption = 'Reader/Punch'
        object Panel4: TPanel
          Left = 0
          Top = 0
          Width = 421
          Height = 619
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          object ReaderPanel: TPanel
            Left = 1
            Top = 1
            Width = 419
            Height = 281
            Align = alTop
            TabOrder = 0
          end
          object PunchPanel: TPanel
            Left = 1
            Top = 282
            Width = 419
            Height = 281
            Align = alTop
            TabOrder = 1
          end
        end
      end
      object PrinterPage: TTabSheet
        Caption = 'Printer'
        ImageIndex = 1
        object PrinterPanel: TPanel
          Left = 0
          Top = 0
          Width = 421
          Height = 619
          Align = alClient
          ParentBackground = False
          TabOrder = 0
        end
      end
      object PaperTapePage: TTabSheet
        Caption = 'Paper Tape'
        ImageIndex = 2
        object PaperTapePanel: TPanel
          Left = 0
          Top = 0
          Width = 421
          Height = 619
          Align = alClient
          ParentBackground = False
          TabOrder = 0
          object Label31: TLabel
            Left = 22
            Top = 24
            Width = 46
            Height = 13
            Caption = 'File Name'
          end
          object PTLoadedLbl: TLabel
            Left = 21
            Top = 94
            Width = 391
            Height = 17
            Alignment = taCenter
            AutoSize = False
            Caption = 'PTLoadedLbl'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clRed
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            Visible = False
          end
          object PTFileNameEdt: TEdit
            Left = 80
            Top = 21
            Width = 303
            Height = 21
            TabOrder = 0
          end
          object PTBrowseBtn: TButton
            Left = 386
            Top = 19
            Width = 26
            Height = 25
            Caption = '...'
            TabOrder = 1
            OnClick = PTBrowseBtnClick
          end
          object PTMountBtn: TButton
            Left = 80
            Top = 48
            Width = 75
            Height = 25
            Caption = 'Load'
            TabOrder = 2
            OnClick = PTMountBtnClick
          end
          object PTUnmountBtn: TButton
            Left = 154
            Top = 48
            Width = 75
            Height = 25
            Caption = 'Unload'
            TabOrder = 3
            OnClick = PTUnmountBtnClick
          end
        end
      end
    end
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 142
    Top = 532
  end
  object PTOpenDlg: TOpenDialog
    Filter = 'Paper Tape Files|*.pt|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 880
    Top = 86
  end
end
