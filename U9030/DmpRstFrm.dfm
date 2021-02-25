object DmpRstForm: TDmpRstForm
  Left = 0
  Top = 0
  Caption = 'Dump/Restore'
  ClientHeight = 557
  ClientWidth = 819
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    819
    557)
  PixelsPerInch = 96
  TextHeight = 13
  object FileNameEdt: TLabeledEdit
    Left = 16
    Top = 26
    Width = 357
    Height = 21
    EditLabel.Width = 43
    EditLabel.Height = 13
    EditLabel.Caption = 'Tape File'
    TabOrder = 0
    Text = '..\..\OS3\90_30_OS3_4.2\os3_4.2.tap'
  end
  object DumpBtn: TButton
    Left = 379
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Dump'
    Default = True
    TabOrder = 1
    OnClick = DumpBtnClick
  end
  object DataMemo: TMemo
    Left = 316
    Top = 58
    Width = 495
    Height = 489
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object HdrList: TListBox
    Left = 16
    Top = 58
    Width = 294
    Height = 489
    Anchors = [akLeft, akTop, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ItemHeight = 14
    ParentFont = False
    TabOrder = 3
    OnClick = HdrListClick
  end
  object RestoreBtn: TButton
    Left = 454
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Restore'
    TabOrder = 4
    OnClick = RestoreBtnClick
  end
  object OpenDlg: TOpenDialog
    Filter = '8418 Disk Files|*.8418|8416 Disk Files|*.8416'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 782
    Top = 22
  end
end
