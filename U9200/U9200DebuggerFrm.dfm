object U92Debugger: TU92Debugger
  Left = 0
  Top = 0
  Caption = 'Debugger'
  ClientHeight = 489
  ClientWidth = 909
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  DesignSize = (
    909
    489)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 7
    Top = 10
    Width = 401
    Height = 64
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 17
    Top = 3
    Width = 26
    Height = 13
    Caption = 'State'
    Transparent = False
  end
  object Label2: TLabel
    Left = 7
    Top = 475
    Width = 231
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'N - next/C - continue/D x - Dump/B x - Set Brkpt'
  end
  object ExceptLbl: TLabel
    Left = 424
    Top = 10
    Width = 477
    Height = 16
    AutoSize = False
    Caption = 'ExceptLbl'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object ProcBox: TCheckBox
    Left = 15
    Top = 23
    Width = 97
    Height = 17
    Caption = 'Procssor'
    TabOrder = 1
  end
  object IOBox: TCheckBox
    Left = 112
    Top = 23
    Width = 97
    Height = 17
    Caption = 'I/O'
    TabOrder = 2
  end
  object StepBox: TCheckBox
    Left = 213
    Top = 23
    Width = 97
    Height = 17
    Caption = 'Single Step'
    TabOrder = 3
  end
  object HaltBox: TCheckBox
    Left = 314
    Top = 23
    Width = 97
    Height = 17
    Caption = 'Halted'
    TabOrder = 4
  end
  object StallBox: TCheckBox
    Left = 15
    Top = 47
    Width = 97
    Height = 17
    Caption = 'Stalled'
    TabOrder = 5
  end
  object FetchedBox: TCheckBox
    Left = 110
    Top = 46
    Width = 97
    Height = 17
    Caption = 'Inst. Fetched'
    TabOrder = 7
  end
  object DumpMemo: TMemo
    Left = 424
    Top = 27
    Width = 485
    Height = 462
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Terminal'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object RegGrid: TStringGrid
    Left = 7
    Top = 80
    Width = 401
    Height = 71
    ColCount = 11
    DefaultColWidth = 35
    DefaultRowHeight = 21
    RowCount = 3
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 8
  end
  object ErrorBox: TCheckBox
    Left = 213
    Top = 46
    Width = 97
    Height = 17
    Caption = 'Error'
    TabOrder = 6
  end
  object CommandEdt: TEdit
    Left = 7
    Top = 450
    Width = 401
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 9
    OnChange = CommandEdtChange
  end
  object Pages: TPageControl
    Left = 7
    Top = 154
    Width = 401
    Height = 292
    ActivePage = BrkptPage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 10
    object TracePage: TTabSheet
      Caption = 'Trace'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object TraceGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 393
        Height = 264
        Align = alClient
        ColCount = 2
        DefaultColWidth = 35
        RowCount = 51
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          35
          359)
      end
    end
    object BrkptPage: TTabSheet
      Caption = 'Brkpts'
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object BrkptGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 393
        Height = 264
        Align = alClient
        ColCount = 2
        DefaultColWidth = 10
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          10
          52)
      end
    end
  end
end
