object U9030PrintForm: TU9030PrintForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Sperry*Univac 90/30 File Print'
  ClientHeight = 117
  ClientWidth = 497
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusLbl: TLabel
    Left = 153
    Top = 45
    Width = 55
    Height = 13
    Caption = '** Done **'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
  end
  object FileNameEdt: TLabeledEdit
    Left = 8
    Top = 18
    Width = 353
    Height = 21
    EditLabel.Width = 41
    EditLabel.Height = 13
    EditLabel.Caption = 'Print File'
    TabOrder = 0
  end
  object PrintBtn: TButton
    Left = 389
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Print'
    Default = True
    TabOrder = 2
    OnClick = PrintBtnClick
  end
  object BrowseBtn: TButton
    Left = 362
    Top = 16
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = BrowseBtnClick
  end
  object LpiBtn: TRadioGroup
    Left = 8
    Top = 54
    Width = 93
    Height = 45
    Caption = 'LPI'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      '6'
      '8')
    TabOrder = 3
  end
  object OpenDlg: TOpenDialog
    Filter = 'Print Files|*.prn|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 224
    Top = 52
  end
  object PrinterDlg: TPrintDialog
    Left = 268
    Top = 54
  end
end
