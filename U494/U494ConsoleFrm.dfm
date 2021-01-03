object U494ConsoleForm: TU494ConsoleForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'Univac 494 Console'
  ClientHeight = 571
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  DesignSize = (
    670
    571)
  PixelsPerInch = 96
  TextHeight = 13
  object Printer: TMemo
    Left = 0
    Top = 0
    Width = 670
    Height = 571
    Anchors = [akLeft, akTop, akRight, akBottom]
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Timer: TTimer
    Enabled = False
    Interval = 100
    OnTimer = TimerTimer
    Left = 558
    Top = 236
  end
  object MainMenu: TMainMenu
    Left = 92
    Top = 12
    object FileMenu: TMenuItem
      Caption = 'File'
      object RecordMenu: TMenuItem
        Caption = 'Record'
        OnClick = RecordMenuClick
      end
    end
  end
  object OpenDlg: TOpenDialog
    Filter = 'Text Files|*.txt|All Files|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 92
    Top = 90
  end
end
