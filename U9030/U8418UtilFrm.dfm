object U8418UtilForm: TU8418UtilForm
  Left = 0
  Top = 0
  Caption = '8418 Utilities'
  ClientHeight = 493
  ClientWidth = 930
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    930
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
    Width = 930
    Height = 460
    ActivePage = LibsPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PagesChange
    object VTOCPage: TTabSheet
      Caption = 'VTOC'
      DesignSize = (
        922
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
        Width = 919
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
    object LibsPage: TTabSheet
      Caption = 'Libs'
      ImageIndex = 1
      DesignSize = (
        922
        432)
      object LibsList: TListBox
        Left = 0
        Top = 44
        Width = 135
        Height = 385
        Anchors = [akLeft, akTop, akBottom]
        ItemHeight = 13
        TabOrder = 8
      end
      object LibsDumpBtn: TButton
        Left = 330
        Top = 13
        Width = 75
        Height = 25
        Caption = 'Dump'
        TabOrder = 3
        OnClick = LibsDumpBtnClick
      end
      object LibsTrackEdt: TLabeledEdit
        Left = 275
        Top = 15
        Width = 52
        Height = 21
        EditLabel.Width = 35
        EditLabel.Height = 13
        EditLabel.Caption = 'Block #'
        NumbersOnly = True
        TabOrder = 2
      end
      object LibsOpenBtn: TButton
        Left = 142
        Top = 13
        Width = 75
        Height = 25
        Caption = 'Open'
        TabOrder = 0
        OnClick = LibsOpenBtnClick
      end
      object LibsDirBtn: TButton
        Left = 404
        Top = 13
        Width = 75
        Height = 25
        Caption = 'Directory'
        TabOrder = 4
        OnClick = LibsDirBtnClick
      end
      object LibsPartEdt: TLabeledEdit
        Left = 223
        Top = 15
        Width = 48
        Height = 21
        EditLabel.Width = 35
        EditLabel.Height = 13
        EditLabel.Caption = 'Part. #'
        TabOrder = 1
      end
      object LibsMemo: TMemo
        Left = 139
        Top = 44
        Width = 780
        Height = 385
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 7
      end
      object LibsSourceBtn: TButton
        Left = 603
        Top = 13
        Width = 75
        Height = 25
        Caption = 'Show Source'
        TabOrder = 6
        OnClick = LibsSourceBtnClick
      end
      object LibsSourceEdt: TLabeledEdit
        Left = 481
        Top = 15
        Width = 121
        Height = 21
        CharCase = ecUpperCase
        EditLabel.Width = 68
        EditLabel.Height = 13
        EditLabel.Caption = 'Element Name'
        TabOrder = 5
      end
      object LibsExportBtn: TButton
        Left = 677
        Top = 13
        Width = 98
        Height = 25
        Caption = 'Export All Source'
        TabOrder = 9
        OnClick = LibsExportBtnClick
      end
    end
    object DiskPage: TTabSheet
      Caption = 'Disk'
      ImageIndex = 2
      DesignSize = (
        922
        432)
      object DiskCylEdt: TLabeledEdit
        Left = 7
        Top = 16
        Width = 44
        Height = 21
        EditLabel.Width = 19
        EditLabel.Height = 13
        EditLabel.Caption = 'Cyl.'
        NumbersOnly = True
        TabOrder = 0
        Text = '0'
      end
      object DiskHeadEdt: TLabeledEdit
        Left = 57
        Top = 16
        Width = 44
        Height = 21
        EditLabel.Width = 25
        EditLabel.Height = 13
        EditLabel.Caption = 'Head'
        NumbersOnly = True
        TabOrder = 1
        Text = '0'
      end
      object DiskRecEdt: TLabeledEdit
        Left = 107
        Top = 16
        Width = 44
        Height = 21
        EditLabel.Width = 22
        EditLabel.Height = 13
        EditLabel.Caption = 'Rec.'
        NumbersOnly = True
        TabOrder = 2
        Text = '1'
      end
      object DiskNumRecsEdt: TLabeledEdit
        Left = 157
        Top = 16
        Width = 44
        Height = 21
        EditLabel.Width = 38
        EditLabel.Height = 13
        EditLabel.Caption = '# Recs.'
        NumbersOnly = True
        TabOrder = 3
        Text = '1'
      end
      object DiskDumpBtn: TButton
        Left = 207
        Top = 12
        Width = 75
        Height = 25
        Caption = 'Dump'
        TabOrder = 4
        OnClick = DiskDumpBtnClick
      end
      object DiskMemo: TMemo
        Left = 0
        Top = 43
        Width = 921
        Height = 388
        Anchors = [akLeft, akTop, akRight, akBottom]
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssVertical
        TabOrder = 5
      end
      object DiskExportBtn: TButton
        Left = 287
        Top = 12
        Width = 75
        Height = 25
        Caption = 'Export'
        TabOrder = 6
        OnClick = DiskExportBtnClick
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
    Filter = 'Disk Volumes|*.8418;*.8416|Disk Data Files|*.dta'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 676
    Top = 462
  end
end
