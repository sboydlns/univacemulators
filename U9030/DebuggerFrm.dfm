object DebuggerForm: TDebuggerForm
  Left = 0
  Top = 0
  Caption = 'Debugger'
  ClientHeight = 634
  ClientWidth = 910
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  DesignSize = (
    910
    634)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 7
    Top = 10
    Width = 424
    Height = 123
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
    Top = 620
    Width = 371
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'N - next/C - continue/D x - Dump/B x - Set Brkpt/SD start,end - ' +
      'System Dump'
    ExplicitTop = 633
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
  object Label3: TLabel
    Left = 66
    Top = 18
    Width = 47
    Height = 13
    Caption = 'Processor'
  end
  object Label4: TLabel
    Left = 253
    Top = 18
    Width = 45
    Height = 13
    Caption = 'Registers'
  end
  object Label5: TLabel
    Left = 65
    Top = 53
    Width = 49
    Height = 13
    Caption = 'Interrupts'
  end
  object CondCodeLbl: TLabel
    Left = 11
    Top = 109
    Width = 57
    Height = 13
    Caption = 'Cond. Code'
  end
  object RelRegLbl: TLabel
    Left = 99
    Top = 109
    Width = 41
    Height = 13
    Caption = 'Rel. Reg'
  end
  object SuperBox: TCheckBox
    Left = 11
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Supervisor'
    TabOrder = 1
  end
  object ProgBox: TCheckBox
    Left = 108
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Program'
    TabOrder = 2
  end
  object StepBox: TCheckBox
    Left = 294
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Single Step'
    TabOrder = 3
  end
  object HaltBox: TCheckBox
    Left = 294
    Top = 108
    Width = 97
    Height = 17
    Caption = 'Halted'
    TabOrder = 4
  end
  object DumpMemo: TMemo
    Left = 437
    Top = 26
    Width = 473
    Height = 607
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object ErrorBox: TCheckBox
    Left = 197
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Error'
    TabOrder = 5
  end
  object CommandEdt: TEdit
    Left = 7
    Top = 596
    Width = 401
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 6
    OnKeyPress = CommandEdtKeyPress
  end
  object Pages: TPageControl
    Left = 7
    Top = 284
    Width = 424
    Height = 306
    ActivePage = TcbPage
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 7
    OnChange = PagesChange
    object TracePage: TTabSheet
      Caption = 'Trace'
      object TraceGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        ColCount = 2
        DefaultColWidth = 45
        RowCount = 101
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          45
          327)
      end
    end
    object BrkptPage: TTabSheet
      Caption = 'Brkpts'
      ImageIndex = 1
      object BrkptGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        ColCount = 2
        DefaultColWidth = 10
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          10
          58)
      end
    end
    object WatchesPage: TTabSheet
      Caption = 'Watches'
      ImageIndex = 2
      object WatchesGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        ColCount = 2
        DefaultColWidth = 10
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          10
          85)
      end
    end
    object SvcPage: TTabSheet
      Caption = 'Svc Trace'
      ImageIndex = 3
      object SvcGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        ColCount = 2
        DefaultColWidth = 45
        RowCount = 101
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          45
          327)
      end
    end
    object SibPage: TTabSheet
      Caption = 'SIB'
      ImageIndex = 4
      object SibMemo: TMemo
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
    object TcbPage: TTabSheet
      Caption = 'Tasks'
      ImageIndex = 5
      object TcbMemo: TMemo
        Left = 0
        Top = 0
        Width = 416
        Height = 278
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object TimerBox: TCheckBox
    Left = 11
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Timer Enabled'
    TabOrder = 8
  end
  object IOBox: TCheckBox
    Left = 108
    Top = 72
    Width = 75
    Height = 17
    Caption = 'I/O Enabled'
    TabOrder = 9
  end
  object RegSuperCheck: TCheckBox
    Left = 197
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Supervisor'
    TabOrder = 10
  end
  object RegPgmCheck: TCheckBox
    Left = 294
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Program'
    TabOrder = 11
  end
  object RegPages: TPageControl
    Left = 8
    Top = 139
    Width = 427
    Height = 144
    ActivePage = RegPage
    TabOrder = 12
    object RegPage: TTabSheet
      Caption = 'Regs'
      object RegGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 419
        Height = 116
        Align = alClient
        ColCount = 4
        DefaultColWidth = 98
        DefaultRowHeight = 21
        FixedCols = 0
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
      end
    end
    object FPRegPage: TTabSheet
      Caption = 'FP Double'
      ImageIndex = 1
      object FPRegGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 419
        Height = 116
        Align = alClient
        ColCount = 4
        DefaultColWidth = 98
        DefaultRowHeight = 21
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
      end
    end
    object FPRegSinglePage: TTabSheet
      Caption = 'FP Single'
      ImageIndex = 2
      object FPRegSingleGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 419
        Height = 116
        Align = alClient
        ColCount = 4
        DefaultColWidth = 98
        DefaultRowHeight = 21
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
      end
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 'Dump Files|*.dmp'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 840
    Top = 466
  end
end
