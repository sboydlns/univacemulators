object Form3: TForm3
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Convert Duane Craps 642B UPAC file '
  ClientHeight = 117
  ClientWidth = 642
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameEdt: TEdit
    Left = 40
    Top = 30
    Width = 547
    Height = 21
    TabOrder = 0
    Text = '..\..\DuaneCraps\642b_upac.txt'
  end
  object OkBtn: TButton
    Left = 40
    Top = 71
    Width = 75
    Height = 25
    Caption = 'GO!'
    TabOrder = 1
    OnClick = OkBtnClick
  end
end
