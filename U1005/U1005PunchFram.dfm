object U1005PunchFrame: TU1005PunchFrame
  Left = 0
  Top = 0
  Width = 419
  Height = 281
  Color = clBlack
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  OnResize = FrameResize
  DesignSize = (
    419
    281)
  object Label63: TLabel
    Left = 0
    Top = 0
    Width = 419
    Height = 18
    Align = alTop
    Alignment = taCenter
    Caption = 'R E A D E R  /  P U N C H'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 250
  end
  object PunchInput: TGauge
    Left = 259
    Top = 40
    Width = 155
    Height = 236
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
  end
  object PunchOutput1: TGauge
    Left = 2
    Top = 41
    Width = 155
    Height = 113
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object Label66: TLabel
    Left = 312
    Top = 22
    Width = 50
    Height = 18
    Alignment = taCenter
    Caption = 'INPUT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label67: TLabel
    Left = 43
    Top = 22
    Width = 60
    Height = 18
    Alignment = taCenter
    Caption = 'OUTPUT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ReadStation: TGauge
    Left = 157
    Top = 266
    Width = 103
    Height = 9
    Anchors = [akLeft, akBottom]
    BackColor = clGray
    BorderStyle = bsNone
    Color = clGray
    ForeColor = clWhite
    MaxValue = 1
    ParentColor = False
    Progress = 0
    ShowText = False
  end
  object PunchOutput2: TGauge
    Left = 2
    Top = 163
    Width = 155
    Height = 113
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object PunchStation: TGauge
    Left = 157
    Top = 251
    Width = 103
    Height = 9
    Anchors = [akLeft, akBottom]
    BackColor = clGray
    BorderStyle = bsNone
    Color = clGray
    ForeColor = clWhite
    MaxValue = 1
    ParentColor = False
    Progress = 0
    ShowText = False
  end
  object Punchoutput1Lbl: TLabel
    Left = 161
    Top = 41
    Width = 10
    Height = 18
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object PunchOutput2Lbl: TLabel
    Left = 161
    Top = 258
    Width = 10
    Height = 18
    Anchors = [akLeft]
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object PunchInputLbl: TLabel
    Left = 216
    Top = 41
    Width = 40
    Height = 18
    Alignment = taRightJustify
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
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
    Caption = 'Empty Hoppers'
    TabOrder = 1
    OnClick = PunchEmptyBtnClick
  end
  object SaveHopper1Btn: TButton
    Left = 163
    Top = 109
    Width = 88
    Height = 25
    Caption = 'Save Upper'
    TabOrder = 2
    OnClick = SaveHopper1BtnClick
  end
  object SaveHopper2Btn: TButton
    Left = 163
    Top = 133
    Width = 88
    Height = 25
    Caption = 'Save Lower'
    TabOrder = 3
    OnClick = SaveHopper2BtnClick
  end
  object SaveDlg: TSaveDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.asm;*.asm3;*.rpg;*.jcl|' +
      'Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*.asc;*.xs3|' +
      'Assembler Source|*.asm;*.asm3|RPG Source|*.rpg|Job Control|*.jcl'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 48
    Top = 70
  end
end
