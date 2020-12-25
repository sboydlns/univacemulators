object U9200DisAsmForm: TU9200DisAsmForm
  Left = 0
  Top = 0
  Caption = 'Univac 9200 DisAssembler'
  ClientHeight = 576
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    635
    576)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 552
    Width = 51
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Object File'
    ExplicitTop = 551
  end
  object OkBtn: TButton
    Left = 335
    Top = 547
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'GO!'
    TabOrder = 2
    OnClick = OkBtnClick
    ExplicitTop = 546
  end
  object CancelBtn: TButton
    Left = 483
    Top = 547
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelBtnClick
    ExplicitTop = 546
  end
  object FileNameEdt: TEdit
    Left = 65
    Top = 549
    Width = 233
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 0
    ExplicitTop = 548
  end
  object BrowseBtn: TButton
    Left = 300
    Top = 547
    Width = 26
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseBtnClick
    ExplicitTop = 546
  end
  object SourceMemo: TMemo
    Left = 2
    Top = 4
    Width = 631
    Height = 539
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 4
    ExplicitHeight = 538
  end
  object SaveBtn: TButton
    Left = 409
    Top = 547
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Save'
    TabOrder = 5
    OnClick = SaveBtnClick
    ExplicitTop = 546
  end
  object OpenDlg: TOpenDialog
    Filter = 'Object Files|*.h16|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 576
    Top = 493
  end
  object SaveDlg: TSaveDialog
    Filter = 'All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 504
    Top = 497
  end
end
