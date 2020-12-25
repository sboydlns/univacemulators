object FH880DumpForm: TFH880DumpForm
  Left = 0
  Top = 0
  Caption = 'Drum Dump'
  ClientHeight = 422
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 381
    Width = 754
    Height = 41
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 14
      Width = 39
      Height = 13
      Caption = 'Address'
    end
    object AddressEdt: TEdit
      Left = 67
      Top = 11
      Width = 121
      Height = 21
      BiDiMode = bdLeftToRight
      NumbersOnly = True
      ParentBiDiMode = False
      TabOrder = 0
      Text = '0'
    end
    object GoBtn: TButton
      Left = 204
      Top = 9
      Width = 75
      Height = 25
      Caption = 'GO'
      Default = True
      TabOrder = 1
      OnClick = GoBtnClick
    end
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 754
    Height = 381
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
end
