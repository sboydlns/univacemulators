object CpuTestForm: TCpuTestForm
  Left = 0
  Top = 0
  Caption = '90/30 CPU Tester'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 27
    Height = 13
    Caption = 'Script'
  end
  object ScriptEdt: TEdit
    Left = 41
    Top = 5
    Width = 308
    Height = 21
    TabOrder = 0
  end
  object BrowseBtn: TButton
    Left = 352
    Top = 3
    Width = 25
    Height = 25
    Caption = '...'
    TabOrder = 1
    OnClick = BrowseBtnClick
  end
  object ResultsMemo: TMemo
    Left = 0
    Top = 32
    Width = 635
    Height = 267
    Align = alBottom
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
  object ExecBtn: TButton
    Left = 379
    Top = 3
    Width = 75
    Height = 25
    Caption = 'Execute'
    TabOrder = 3
    OnClick = ExecBtnClick
  end
  object OpenDlg: TOpenDialog
    Filter = 'Test Files|*.tst'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 10
    Top = 42
  end
end
