object FH880UtilForm: TFH880UtilForm
  Left = 0
  Top = 0
  Caption = 'FH880 Utilities'
  ClientHeight = 302
  ClientWidth = 435
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
  object Label1: TLabel
    Left = 12
    Top = 26
    Width = 59
    Height = 13
    Caption = 'File Location'
  end
  object ProgressLbl: TLabel
    Left = 0
    Top = 283
    Width = 435
    Height = 19
    Align = alBottom
    Alignment = taCenter
    Caption = 'ProgressLbl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Visible = False
    ExplicitWidth = 82
  end
  object Label2: TLabel
    Left = 12
    Top = 72
    Width = 44
    Height = 13
    Caption = 'Drum File'
  end
  object Label3: TLabel
    Left = 12
    Top = 49
    Width = 51
    Height = 13
    Caption = 'Import File'
  end
  object LocationEdt: TEdit
    Left = 84
    Top = 23
    Width = 309
    Height = 21
    TabOrder = 0
  end
  object BrowseBtn: TButton
    Left = 400
    Top = 21
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseBtnClick
  end
  object CreateBtn: TButton
    Left = 84
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Create'
    TabOrder = 3
    OnClick = CreateBtnClick
  end
  object DumpBtn: TButton
    Left = 158
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Dump'
    TabOrder = 4
    OnClick = DumpBtnClick
  end
  object FileNameEdt: TEdit
    Left = 84
    Top = 69
    Width = 121
    Height = 21
    CharCase = ecUpperCase
    MaxLength = 10
    TabOrder = 2
  end
  object LoadTextBtn: TButton
    Left = 232
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Load Text File'
    TabOrder = 5
    OnClick = LoadTextBtnClick
  end
  object ImportEdt: TEdit
    Left = 84
    Top = 46
    Width = 309
    Height = 21
    TabOrder = 6
  end
  object ImportBrowseBtn: TButton
    Left = 400
    Top = 44
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 7
    TabStop = False
    OnClick = ImportBrowseBtnClick
  end
  object LoadPgmBtn: TButton
    Left = 306
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Load Program'
    TabOrder = 8
    OnClick = LoadPgmBtnClick
  end
  object MfdMemo: TMemo
    Left = 0
    Top = 161
    Width = 435
    Height = 122
    Align = alBottom
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 9
  end
  object MfdBtn: TButton
    Left = 84
    Top = 120
    Width = 75
    Height = 25
    Caption = 'List MFD'
    TabOrder = 10
    OnClick = MfdBtnClick
  end
  object OpenDrumDlg: TOpenDialog
    Filter = 'Drum File|*.drum'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 22
    Top = 154
  end
  object OpenDataDlg: TOpenDialog
    Filter = 'All Files|*.txt;*.mem|Text Files|*.txt|Program Files|*.mem'
    Left = 100
    Top = 158
  end
end
