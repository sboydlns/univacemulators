object DebuggerForm: TDebuggerForm
  Left = 0
  Top = 0
  Caption = 'Debugger'
  ClientHeight = 622
  ClientWidth = 913
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
    913
    622)
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 7
    Top = 10
    Width = 401
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
    Top = 608
    Width = 371
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      'N - next/C - continue/D x - Dump/B x - Set Brkpt/SD start,end - ' +
      'System Dump'
    ExplicitTop = 524
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
    Left = 197
    Top = 108
    Width = 97
    Height = 17
    Caption = 'Halted'
    TabOrder = 4
  end
  object DumpMemo: TMemo
    Left = 414
    Top = 27
    Width = 499
    Height = 595
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
    ExplicitHeight = 511
  end
  object RegGrid: TStringGrid
    Left = 7
    Top = 135
    Width = 401
    Height = 117
    ColCount = 4
    DefaultColWidth = 98
    DefaultRowHeight = 21
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 6
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
    Top = 583
    Width = 401
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 7
    OnKeyPress = CommandEdtKeyPress
    ExplicitTop = 499
  end
  object Pages: TPageControl
    Left = 7
    Top = 254
    Width = 405
    Height = 325
    ActivePage = TracePage
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 8
    object TracePage: TTabSheet
      Caption = 'Trace'
      ExplicitHeight = 267
      object TraceGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 397
        Height = 297
        Align = alClient
        ColCount = 2
        DefaultColWidth = 45
        RowCount = 101
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ExplicitHeight = 267
        ColWidths = (
          45
          327)
      end
    end
    object BrkptPage: TTabSheet
      Caption = 'Brkpts'
      ImageIndex = 1
      ExplicitHeight = 263
      object BrkptGrid: TStringGrid
        Left = 0
        Top = 0
        Width = 397
        Height = 297
        Align = alClient
        ColCount = 2
        DefaultColWidth = 10
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ExplicitHeight = 263
        ColWidths = (
          10
          58)
      end
    end
  end
  object TimerBox: TCheckBox
    Left = 11
    Top = 72
    Width = 97
    Height = 17
    Caption = 'Timer Enabled'
    TabOrder = 9
  end
  object IOBox: TCheckBox
    Left = 108
    Top = 72
    Width = 75
    Height = 17
    Caption = 'I/O Enabled'
    TabOrder = 10
  end
  object RegSuperCheck: TCheckBox
    Left = 197
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Supervisor'
    TabOrder = 11
  end
  object RegPgmCheck: TCheckBox
    Left = 294
    Top = 34
    Width = 97
    Height = 17
    Caption = 'Program'
    TabOrder = 12
  end
  object OpenDlg: TOpenDialog
    Filter = 'Dump Files|*.dmp'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 840
    Top = 466
  end
end
