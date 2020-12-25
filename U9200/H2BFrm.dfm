object H2BForm: TH2BForm
  Left = 0
  Top = 0
  Caption = 'Hollerith to Binary Translation'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ResultLbl: TLabel
    Left = 102
    Top = 154
    Width = 75
    Height = 18
    AutoSize = False
    Caption = 'Result'
    Transparent = False
  end
  object B12: TCheckBox
    Left = 36
    Top = 14
    Width = 40
    Height = 17
    Caption = '12'
    TabOrder = 0
  end
  object B11: TCheckBox
    Left = 36
    Top = 37
    Width = 40
    Height = 17
    Caption = '11'
    TabOrder = 1
  end
  object b0: TCheckBox
    Left = 36
    Top = 60
    Width = 40
    Height = 17
    Caption = '0'
    TabOrder = 2
  end
  object b1: TCheckBox
    Left = 36
    Top = 83
    Width = 40
    Height = 17
    Caption = '1'
    TabOrder = 3
  end
  object b2: TCheckBox
    Left = 36
    Top = 106
    Width = 40
    Height = 17
    Caption = '2'
    TabOrder = 4
  end
  object b3: TCheckBox
    Left = 36
    Top = 129
    Width = 40
    Height = 17
    Caption = '3'
    TabOrder = 5
  end
  object b4: TCheckBox
    Left = 36
    Top = 153
    Width = 40
    Height = 17
    Caption = '4'
    TabOrder = 6
  end
  object b5: TCheckBox
    Left = 36
    Top = 176
    Width = 40
    Height = 17
    Caption = '5'
    TabOrder = 7
  end
  object b6: TCheckBox
    Left = 36
    Top = 199
    Width = 40
    Height = 17
    Caption = '6'
    TabOrder = 8
  end
  object b7: TCheckBox
    Left = 36
    Top = 222
    Width = 40
    Height = 17
    Caption = '7'
    TabOrder = 9
  end
  object b8: TCheckBox
    Left = 36
    Top = 245
    Width = 40
    Height = 17
    Caption = '8'
    TabOrder = 10
  end
  object b9: TCheckBox
    Left = 36
    Top = 269
    Width = 40
    Height = 17
    Caption = '9'
    TabOrder = 11
  end
  object XlateBtn: TButton
    Left = 102
    Top = 94
    Width = 75
    Height = 25
    Caption = 'Xlate'
    TabOrder = 12
    OnClick = XlateBtnClick
  end
  object CloseBtn: TButton
    Left = 102
    Top = 118
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 13
  end
end
