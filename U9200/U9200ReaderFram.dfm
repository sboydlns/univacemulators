object U9200ReaderFrame: TU9200ReaderFrame
  Left = 0
  Top = 0
  Width = 417
  Height = 281
  Color = clGray
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
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 160
  end
  object ReaderInput: TGauge
    Left = 258
    Top = 40
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
    ExplicitLeft = 267
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
    Left = 311
    Top = 22
    Width = 50
    Height = 18
    Alignment = taCenter
    Anchors = [akTop, akRight]
    Caption = 'INPUT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitLeft = 320
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
    Top = 267
    Width = 101
    Height = 9
    Anchors = [akLeft, akRight, akBottom]
    BackColor = clGray
    BorderStyle = bsNone
    Color = clGray
    ForeColor = clWhite
    MaxValue = 1
    ParentColor = False
    Progress = 0
    ShowText = False
    ExplicitWidth = 110
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
    Left = 215
    Top = 41
    Width = 40
    Height = 18
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = '0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    WordWrap = True
    ExplicitLeft = 224
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
    Caption = 'Empty Hopper'
    TabOrder = 1
    OnClick = ReaderEmptyBtnClick
  end
  object LoadCardsDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.asm;*.rpg;*.jcl;*.ccl|Holleri' +
      'th 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*.asc|Assembler Sou' +
      'rce|*.asm|RPG Source|*.rpg|Job Control|*.jcl|Card Control Langua' +
      'ge|*.ccl'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 29
    Top = 167
  end
end
