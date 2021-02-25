object U8418UtilForm: TU8418UtilForm
  Left = 0
  Top = 0
  Caption = '8418 Utilities'
  ClientHeight = 493
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    711
    493)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 469
    Width = 38
    Height = 13
    Caption = 'Disk File'
  end
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 711
    Height = 460
    ActivePage = VTOCPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object VTOCPage: TTabSheet
      Caption = 'VTOC'
      ExplicitHeight = 473
      DesignSize = (
        703
        432)
      object ListVTOCBtn: TButton
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Caption = 'List'
        TabOrder = 0
        OnClick = ListVTOCBtnClick
      end
      object VTOCMemo: TMemo
        Left = 3
        Top = 34
        Width = 700
        Height = 398
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 1
      end
    end
  end
  object FileNameEdt: TEdit
    Left = 49
    Top = 466
    Width = 430
    Height = 21
    TabOrder = 1
    Text = '..\..\Disks\REL042.8418'
  end
  object BrowseBtn: TButton
    Left = 484
    Top = 464
    Width = 31
    Height = 25
    Caption = '...'
    TabOrder = 2
    OnClick = BrowseBtnClick
  end
  object OpenBtn: TButton
    Left = 519
    Top = 464
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 3
    OnClick = OpenBtnClick
  end
  object OpenDlg: TOpenDialog
    Filter = 'Disk Files|*.8418;*.8416'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 676
    Top = 462
  end
end
