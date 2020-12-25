object U1005PrinterFrame: TU1005PrinterFrame
  Left = 0
  Top = 0
  Width = 772
  Height = 556
  Color = clGray
  ParentBackground = False
  ParentColor = False
  TabOrder = 0
  DesignSize = (
    772
    556)
  object Label65: TLabel
    Left = 0
    Top = 0
    Width = 772
    Height = 18
    Align = alTop
    Alignment = taCenter
    Caption = 'P  R  I  N  T  E  R'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -16
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ExplicitWidth = 190
  end
  object Label3: TLabel
    Left = 6
    Top = 3
    Width = 15
    Height = 13
    Caption = 'LPI'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCream
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Transparent = False
  end
  object LPIBtn: TRadioGroup
    Left = 1
    Top = 18
    Width = 93
    Height = 35
    Columns = 2
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = 0
    Items.Strings = (
      '6'
      '8')
    ParentFont = False
    TabOrder = 0
    OnClick = LPIBtnClick
  end
  object ScrollBox1: TScrollBox
    Left = 0
    Top = 58
    Width = 771
    Height = 497
    Anchors = [akLeft, akTop, akRight, akBottom]
    BorderStyle = bsNone
    TabOrder = 1
    object Page: TImage
      Left = 6
      Top = -2
      Width = 425
      Height = 228
      AutoSize = True
    end
  end
  object PrintBtn: TCheckBox
    Left = 104
    Top = 33
    Width = 46
    Height = 17
    Caption = 'Print'
    TabOrder = 2
    OnClick = PrintBtnClick
  end
  object SingleSpcBtn: TCheckBox
    Left = 155
    Top = 33
    Width = 72
    Height = 17
    Caption = 'Single Spc.'
    TabOrder = 3
    OnClick = SingleSpcBtnClick
  end
  object PrintDlg: TPrintDialog
    Left = 20
    Top = 163
  end
end
