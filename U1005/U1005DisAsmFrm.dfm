object U1005DisAsmForm: TU1005DisAsmForm
  Left = 0
  Top = 0
  Caption = 'Univac 1005 Disassembler'
  ClientHeight = 561
  ClientWidth = 784
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    784
    561)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 496
    Width = 51
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Object File'
  end
  object OkBtn: TButton
    Left = 383
    Top = 491
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'GO!'
    TabOrder = 0
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 531
    Top = 491
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
  end
  object FileNameEdt: TEdit
    Left = 65
    Top = 493
    Width = 287
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object BrowseBtn: TButton
    Left = 355
    Top = 491
    Width = 26
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 3
    TabStop = False
    OnClick = BrowseBtnClick
  end
  object SourceMemo: TMemo
    Left = 2
    Top = 4
    Width = 780
    Height = 477
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
  end
  object SaveBtn: TButton
    Left = 457
    Top = 491
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 5
  end
  object SystemTypeBtn: TRadioGroup
    Left = 65
    Top = 516
    Width = 316
    Height = 34
    Columns = 2
    ItemIndex = 1
    Items.Strings = (
      'Federal Systems (military)'
      'Commercial')
    TabOrder = 6
  end
  object OpenDlg: TOpenDialog
    Filter = 'Object Files|*.h16;*.h80;*.xs3|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 576
    Top = 513
  end
  object SaveDlg: TSaveDialog
    Filter = 'All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 510
    Top = 513
  end
end
