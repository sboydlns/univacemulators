object CaveUtilForm: TCaveUtilForm
  Left = 0
  Top = 0
  Caption = 'Cave Utilities'
  ClientHeight = 188
  ClientWidth = 495
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 26
    Width = 364
    Height = 13
    Caption = 
      'Reformat Cave data to make it easier to parse for a 494 assemble' +
      'r program'
  end
  object Label2: TLabel
    Left = 18
    Top = 74
    Width = 120
    Height = 13
    Caption = 'Line Counts by Line Type'
  end
  object Label3: TLabel
    Left = 16
    Top = 52
    Width = 91
    Height = 13
    Caption = 'Generate text map'
  end
  object FormatBtn: TButton
    Left = 394
    Top = 21
    Width = 75
    Height = 25
    Caption = 'Format'
    TabOrder = 0
    OnClick = FormatBtnClick
  end
  object CountMemo: TMemo
    Left = 18
    Top = 89
    Width = 185
    Height = 89
    TabOrder = 1
  end
  object MapBtn: TButton
    Left = 394
    Top = 47
    Width = 75
    Height = 25
    Caption = 'Map'
    TabOrder = 2
    OnClick = MapBtnClick
  end
end
