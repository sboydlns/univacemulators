object U9200CardViewForm: TU9200CardViewForm
  Left = 0
  Top = 0
  Caption = 'Univac 9200 Card Deck Viewer'
  ClientHeight = 719
  ClientWidth = 1008
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
  object FileNamePanel: TPanel
    Left = 0
    Top = 685
    Width = 1008
    Height = 34
    Align = alBottom
    TabOrder = 0
    object Label1: TLabel
      Left = 4
      Top = 14
      Width = 42
      Height = 13
      Caption = 'Card File'
    end
    object FileNameEdt: TEdit
      Left = 52
      Top = 6
      Width = 288
      Height = 21
      TabOrder = 0
    end
    object BrowseBtn: TButton
      Left = 344
      Top = 6
      Width = 27
      Height = 21
      Caption = '...'
      TabOrder = 1
      TabStop = False
      OnClick = BrowseBtnClick
    end
    object ShowBtn: TButton
      Left = 372
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Show'
      Default = True
      TabOrder = 2
      OnClick = ShowBtnClick
    end
    object CancelBtn: TButton
      Left = 521
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Cancel'
      TabOrder = 4
      OnClick = CancelBtnClick
    end
    object FileTypeBtn: TRadioGroup
      Left = 599
      Top = 1
      Width = 409
      Height = 32
      Columns = 7
      ItemIndex = 0
      Items.Strings = (
        'Auto'
        'Obj'
        'Exe'
        'Macro'
        '1005'
        '494'
        'Other')
      TabOrder = 5
    end
    object SaveBtn: TButton
      Left = 447
      Top = 4
      Width = 75
      Height = 25
      Caption = 'Save Sel.'
      TabOrder = 3
      OnClick = SaveBtnClick
    end
  end
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 1008
    Height = 685
    ActivePage = UnknownFilePage
    Align = alClient
    TabOrder = 1
    object ObjFilePage: TTabSheet
      Caption = 'Object File'
      object Label2: TLabel
        Left = 0
        Top = 0
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Element Definition'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 103
      end
      object Label3: TLabel
        Left = 0
        Top = 90
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Entry Points'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 67
      end
      object Label4: TLabel
        Left = 0
        Top = 201
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Program Reference'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 111
      end
      object Label5: TLabel
        Left = 0
        Top = 312
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'External Reference'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 108
      end
      object Label6: TLabel
        Left = 0
        Top = 423
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Text'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 25
      end
      object Label7: TLabel
        Left = 0
        Top = 546
        Width = 1000
        Height = 16
        Align = alBottom
        Caption = 'Transfer'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 49
      end
      object ObjAGrid: TStringGrid
        Left = 0
        Top = 16
        Width = 1000
        Height = 74
        Align = alTop
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          10
          28
          36
          29
          27
          41
          38
          42
          84
          35)
      end
      object ObjHGrid: TStringGrid
        Left = 0
        Top = 106
        Width = 1000
        Height = 95
        Align = alTop
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 1
        ColWidths = (
          10
          28
          36
          29
          38
          63
          70
          42
          84
          60)
      end
      object ObjJGrid: TStringGrid
        Left = 0
        Top = 217
        Width = 1000
        Height = 95
        Align = alTop
        ColCount = 8
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 2
        ColWidths = (
          10
          28
          36
          29
          38
          41
          38
          107)
      end
      object ObjKGrid: TStringGrid
        Left = 0
        Top = 328
        Width = 1000
        Height = 95
        Align = alTop
        ColCount = 7
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 3
        ColWidths = (
          10
          28
          36
          29
          42
          33
          115)
      end
      object ObjQGrid: TStringGrid
        Left = 0
        Top = 439
        Width = 1000
        Height = 107
        Align = alClient
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 4
        ColWidths = (
          10
          28
          36
          29
          37
          41
          43
          92
          615
          154)
      end
      object ObjYGrid: TStringGrid
        Left = 0
        Top = 562
        Width = 1000
        Height = 95
        Align = alBottom
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 5
        ColWidths = (
          10
          28
          36
          29
          38
          58
          57
          60
          48
          48)
      end
    end
    object ExeFilePage: TTabSheet
      Caption = 'Executable File'
      ImageIndex = 1
      object Label8: TLabel
        Left = 0
        Top = 0
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Text'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 25
      end
      object Label9: TLabel
        Left = 0
        Top = 557
        Width = 1000
        Height = 16
        Align = alBottom
        Caption = 'Transfer'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 49
      end
      object ExeQGrid: TStringGrid
        Left = 0
        Top = 16
        Width = 1000
        Height = 541
        Align = alClient
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          10
          28
          36
          29
          37
          41
          43
          48
          752
          154)
      end
      object ExeYGrid: TStringGrid
        Left = 0
        Top = 573
        Width = 1000
        Height = 84
        Align = alBottom
        ColCount = 10
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 1
        ColWidths = (
          10
          28
          36
          29
          38
          58
          57
          60
          48
          48)
      end
    end
    object MacroFilePage: TTabSheet
      Caption = 'Macro File'
      ImageIndex = 2
      DesignSize = (
        1000
        657)
      object Label10: TLabel
        Left = 0
        Top = 0
        Width = 1000
        Height = 16
        Align = alTop
        Caption = 'Macro Data'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 65
      end
      object Label11: TLabel
        Left = 0
        Top = 529
        Width = 1000
        Height = 16
        Align = alBottom
        Caption = 'Fix Ups'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitWidth = 41
      end
      object Label12: TLabel
        Left = 0
        Top = 658
        Width = 41
        Height = 16
        Caption = 'Header'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object MacroBGrid: TStringGrid
        Left = 0
        Top = 16
        Width = 1000
        Height = 513
        Align = alClient
        ColCount = 6
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          10
          28
          36
          40
          65
          777)
      end
      object MacroCGrid: TStringGrid
        Left = 0
        Top = 545
        Width = 1000
        Height = 112
        Align = alBottom
        ColCount = 6
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 1
        ColWidths = (
          10
          28
          36
          38
          68
          776)
      end
      object MacroZGrid: TStringGrid
        Left = 0
        Top = 673
        Width = 997
        Height = 83
        Anchors = [akLeft, akTop, akRight]
        ColCount = 8
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        ParentFont = False
        TabOrder = 2
        ColWidths = (
          10
          28
          36
          29
          38
          41
          38
          107)
      end
    end
    object UnknownFilePage: TTabSheet
      Caption = 'Unknown'
      ImageIndex = 3
      object UnknownGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 1000
        Height = 657
        Align = alClient
        ColCount = 2
        DefaultColWidth = 10
        DefaultRowHeight = 21
        RowCount = 2
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goRowSelect]
        ParentFont = False
        TabOrder = 0
        ColWidths = (
          10
          819)
      end
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.fd;*.asm;*.asm3;*.rpg;*' +
      '.jcl|Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*.asc;*' +
      '.xs3;*.fd|Assembler Source|*.asm;*.asm3|RPG Source|*.rpg|Job Con' +
      'trol;*.jcl'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 601
    Top = 276
  end
  object SaveDlg: TSaveDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.xs3;*.fd;*.asm;*.asm3;*.rpg;*' +
      '.jcl|Hollerith 16-bit|*.h16|Hollerith 12-bit|*.h80|ASCII|*.asc;*' +
      '.xs3;*.fd|Assembler Source|*.asm;.asm3|RPG Source|*.rpg|Job Cont' +
      'rol|*.jcl'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 602
    Top = 336
  end
end
