object U9200ObjCardViewForm: TU9200ObjCardViewForm
  Left = 0
  Top = 0
  Caption = 'Univac 9200 Object Deck Viewer'
  ClientHeight = 505
  ClientWidth = 644
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  DesignSize = (
    644
    505)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 442
    Width = 42
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Card File'
  end
  object FileNameEdt: TEdit
    Left = 69
    Top = 439
    Width = 288
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 0
  end
  object BrowseBtn: TButton
    Left = 361
    Top = 439
    Width = 27
    Height = 21
    Anchors = [akLeft, akBottom]
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = BrowseBtnClick
  end
  object ShowBtn: TButton
    Left = 123
    Top = 474
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Show'
    Default = True
    TabOrder = 2
    OnClick = ShowBtnClick
  end
  object CancelBtn: TButton
    Left = 197
    Top = 474
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = CancelBtnClick
  end
  object CardGrid: TStringGrid
    Left = 0
    Top = 0
    Width = 644
    Height = 433
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    ColCount = 6
    DefaultColWidth = 10
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine]
    TabOrder = 4
    ColWidths = (
      10
      32
      30
      33
      41
      467)
  end
  object OpenDlg: TOpenDialog
    Filter = 'Card Files|*.crd|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 17
    Top = 460
  end
end
