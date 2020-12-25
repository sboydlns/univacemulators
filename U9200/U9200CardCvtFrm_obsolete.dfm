object Form3: TForm3
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Univac 9200 Card File Converter'
  ClientHeight = 91
  ClientWidth = 400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 18
    Top = 18
    Width = 42
    Height = 13
    Caption = 'Card File'
  end
  object FileNameEdt: TEdit
    Left = 71
    Top = 15
    Width = 288
    Height = 21
    TabOrder = 0
  end
  object BrowseBtn: TButton
    Left = 363
    Top = 15
    Width = 27
    Height = 21
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseBtnClick
  end
  object ConvertBtn: TButton
    Left = 71
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Convert'
    Default = True
    TabOrder = 2
    OnClick = ConvertBtnClick
  end
  object CancelBtn: TButton
    Left = 145
    Top = 50
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelBtnClick
  end
  object CodeBtn: TRadioGroup
    Left = 230
    Top = 44
    Width = 129
    Height = 39
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Raw'
      'EBCDIC')
    TabOrder = 4
  end
  object OpenDlg: TOpenDialog
    Filter = 'Hollerith 16-bit|*.h16'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 19
    Top = 36
  end
end
