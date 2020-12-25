object U494ReaderFrame: TU494ReaderFrame
  Left = 0
  Top = 0
  Width = 417
  Height = 281
  Color = clBtnFace
  ParentBackground = False
  ParentColor = False
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
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 160
  end
  object ReaderInput: TGauge
    Left = 258
    Top = 43
    Width = 155
    Height = 236
    Anchors = [akTop, akRight, akBottom]
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
  object Stacker0: TGauge
    Left = 2
    Top = 43
    Width = 155
    Height = 233
    Anchors = [akLeft, akTop, akBottom]
    BackColor = clSilver
    ForeColor = clWhite
    Kind = gkVerticalBar
    MaxValue = 2000
    Progress = 0
    ShowText = False
  end
  object Label66: TLabel
    Left = 318
    Top = 25
    Width = 35
    Height = 14
    Alignment = taCenter
    Anchors = [akTop, akRight]
    Caption = 'INPUT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object Label67: TLabel
    Left = 52
    Top = 25
    Width = 42
    Height = 14
    Alignment = taCenter
    Caption = 'OUTPUT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ReadStation: TGauge
    Left = 157
    Top = 267
    Width = 101
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
    ExplicitWidth = 110
  end
  object Stacker0Lbl: TLabel
    Left = 68
    Top = 139
    Width = 10
    Height = 18
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ReaderInputLbl: TLabel
    Left = 301
    Top = 139
    Width = 40
    Height = 18
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object ReaderLoadBtn: TButton
    Left = 163
    Top = 69
    Width = 88
    Height = 25
    Caption = 'Load Cards'
    TabOrder = 0
    OnClick = ReaderLoadBtnClick
  end
  object ReaderEmptyBtn: TButton
    Left = 163
    Top = 93
    Width = 88
    Height = 25
    Caption = 'Empty Hoppers'
    TabOrder = 1
    OnClick = ReaderEmptyBtnClick
  end
  object LoadCardsDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.fd;*.asm;*.asm3;*.rpg;*' +
      '.jcl;*.ccl|Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*' +
      '.asc;*.xs3;*.fd|Assembler Source|*.asm;*.asm3|RPG Source|*.rpg|J' +
      'ob Control|*.jcl|Card Control Language|*.ccl'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 195
    Top = 221
  end
end
