object U1005AsmForm: TU1005AsmForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Univac 1005 Assembler'
  ClientHeight = 118
  ClientWidth = 412
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
    Width = 52
    Height = 13
    Caption = 'Source File'
  end
  object FileNameEdt: TEdit
    Left = 77
    Top = 15
    Width = 288
    Height = 21
    TabOrder = 0
  end
  object BrowseBtn: TButton
    Left = 371
    Top = 15
    Width = 27
    Height = 21
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseBtnClick
  end
  object ConvertBtn: TButton
    Left = 131
    Top = 82
    Width = 75
    Height = 25
    Caption = 'Compile'
    Default = True
    TabOrder = 3
    OnClick = ConvertBtnClick
  end
  object CancelBtn: TButton
    Left = 207
    Top = 82
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 4
    OnClick = CancelBtnClick
  end
  object SystemTypeBtn: TRadioGroup
    Left = 77
    Top = 42
    Width = 321
    Height = 34
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'Federal Systems (military)'
      'Commercial')
    TabOrder = 2
  end
  object OpenDlg: TOpenDialog
    Filter = 'Source Files|*.asm|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 19
    Top = 36
  end
end
