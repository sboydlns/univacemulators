object U1005ReaderFrame: TU1005ReaderFrame
  Left = 0
  Top = 0
  Width = 417
  Height = 281
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentBackground = False
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  DesignSize = (
    417
    281)
  object Label63: TLabel
    Left = 0
    Top = 0
    Width = 417
    Height = 18
    Align = alTop
    Alignment = taCenter
    Caption = 'R  E  A  D  E  R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 160
  end
  object ReaderInput: TGauge
    Left = 257
    Top = 40
    Width = 155
    Height = 236
    Anchors = [akLeft, akTop, akBottom]
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
  object ReaderOutput: TGauge
    Left = 2
    Top = 41
    Width = 155
    Height = 235
    Anchors = [akLeft, akTop, akBottom]
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object Label66: TLabel
    Left = 310
    Top = 22
    Width = 50
    Height = 18
    Alignment = taCenter
    Anchors = [akLeft, akTop, akBottom]
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
    Left = 156
    Top = 266
    Width = 102
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
  object ReaderOutputLbl: TLabel
    Left = 159
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
  object ReaderInputLbl: TLabel
    Left = 214
    Top = 41
    Width = 40
    Height = 18
    Alignment = taRightJustify
    Anchors = [akLeft, akTop, akBottom]
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
  object ReaderLoadBtn: TButton
    Left = 163
    Top = 65
    Width = 88
    Height = 25
    Caption = 'Load Cards'
    TabOrder = 0
    OnClick = ReaderLoadBtnClick
  end
  object ReaderEmptyBtn: TButton
    Left = 163
    Top = 88
    Width = 88
    Height = 25
    Caption = 'Empty Hoppers'
    TabOrder = 1
    OnClick = ReaderEmptyBtnClick
  end
  object LoadCardsDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.asm;*.asm3;*.rpg;*.jcl;' +
      '*.ccl|Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*.asc;' +
      '*.xs3|Assembler Source|*.asm;*.asm3|RPG Source|*.rpg|Job Control' +
      '|*.jcl|Card Control Language|*.ccl'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 29
    Top = 167
  end
end
