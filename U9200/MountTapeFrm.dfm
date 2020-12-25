object MountTapeForm: TMountTapeForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Mount a Tape File'
  ClientHeight = 100
  ClientWidth = 507
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 16
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object FileNameEdt: TEdit
    Left = 65
    Top = 13
    Width = 396
    Height = 21
    TabOrder = 0
    Text = 'FileNameEdt'
  end
  object BrowseBtn: TButton
    Left = 463
    Top = 13
    Width = 26
    Height = 21
    Caption = '...'
    TabOrder = 1
    OnClick = BrowseBtnClick
  end
  object WriteEnableCheck: TCheckBox
    Left = 65
    Top = 40
    Width = 97
    Height = 17
    Caption = 'Write Enable'
    TabOrder = 2
  end
  object MountBtn: TButton
    Left = 65
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Mount'
    Default = True
    TabOrder = 3
    OnClick = MountBtnClick
  end
  object CancelBtn: TButton
    Left = 139
    Top = 63
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object OpenDlg: TOpenDialog
    Filter = 'Simulate Tape Files|*.AWS'
    Options = [ofEnableSizing]
    Left = 450
    Top = 54
  end
end
