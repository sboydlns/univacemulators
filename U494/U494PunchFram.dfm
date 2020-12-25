object U494PunchFrame: TU494PunchFrame
  Left = 0
  Top = 0
  Width = 419
  Height = 283
  Color = clBtnFace
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  OnResize = FrameResize
  DesignSize = (
    419
    283)
  object Label63: TLabel
    Left = 0
    Top = 0
    Width = 419
    Height = 18
    Align = alTop
    Alignment = taCenter
    Caption = 'P U N C H'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    ExplicitWidth = 90
  end
  object PunchInput: TGauge
    Left = 260
    Top = 40
    Width = 155
    Height = 236
    Anchors = [akTop, akRight]
    BackColor = clSilver
    Color = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    ParentColor = False
    ParentShowHint = False
    Progress = 0
    ShowHint = False
    ShowText = False
    ExplicitLeft = 267
  end
  object Stacker0: TGauge
    Left = 2
    Top = 40
    Width = 155
    Height = 105
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object Label66: TLabel
    Left = 316
    Top = 22
    Width = 35
    Height = 14
    Alignment = taCenter
    Anchors = [akTop, akRight]
    Caption = 'INPUT'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object StkrHdr0Lbl: TLabel
    Left = 43
    Top = 24
    Width = 63
    Height = 14
    Alignment = taCenter
    Caption = 'STACKER 0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object ReadStation: TGauge
    Left = 157
    Top = 269
    Width = 103
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    BackColor = clBtnFace
    BorderStyle = bsNone
    Color = clBtnFace
    ForeColor = clWhite
    MaxValue = 1
    ParentColor = False
    Progress = 0
    ShowText = False
    ExplicitTop = 267
  end
  object Stacker1: TGauge
    Left = 2
    Top = 171
    Width = 155
    Height = 105
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object PunchStation: TGauge
    Left = 157
    Top = 254
    Width = 103
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    BackColor = clBtnFace
    BorderStyle = bsNone
    Color = clBtnFace
    ForeColor = clWhite
    MaxValue = 1
    ParentColor = False
    Progress = 0
    ShowText = False
    ExplicitTop = 252
  end
  object Stacker0Lbl: TLabel
    Left = 67
    Top = 87
    Width = 10
    Height = 18
    Caption = '0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Stacker1Lbl: TLabel
    Left = 67
    Top = 220
    Width = 10
    Height = 18
    Caption = '0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object PunchInputLbl: TLabel
    Left = 303
    Top = 152
    Width = 40
    Height = 18
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = '0'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object Stkr1HdrLbl: TLabel
    Left = 43
    Top = 154
    Width = 63
    Height = 14
    Alignment = taCenter
    Caption = 'STACKER 1'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    WordWrap = True
  end
  object PunchLoadBtn: TButton
    Left = 163
    Top = 61
    Width = 88
    Height = 25
    Caption = 'Load Cards'
    TabOrder = 0
    OnClick = PunchLoadBtnClick
  end
  object PunchEmptyBtn: TButton
    Left = 163
    Top = 85
    Width = 88
    Height = 25
    Caption = 'Empty Hopper'
    TabOrder = 1
    OnClick = PunchEmptyBtnClick
  end
  object SaveHopper1Btn: TButton
    Left = 163
    Top = 109
    Width = 88
    Height = 25
    Caption = 'Save Stacker 0'
    TabOrder = 2
    OnClick = SaveHopper1BtnClick
  end
  object SaveHopper2Btn: TButton
    Left = 163
    Top = 133
    Width = 88
    Height = 25
    Caption = 'Save Stacker 1'
    TabOrder = 3
    OnClick = SaveHopper2BtnClick
  end
  object SaveDlg: TSaveDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.fd;*.asm;*.asm3;*.rpg;*' +
      '.jcl;*.ccl|Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*' +
      '.asc;*.xs3;*.fd|Assembler Source|*.asm;*.asm3|RPG Source|*.rpg|J' +
      'ob Control|*.jcl|Card Control Language|*.ccl'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 186
    Top = 166
  end
end
