object U200TNForm: TU200TNForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'U200 Emulator / Telnet'
  ClientHeight = 309
  ClientWidth = 645
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Telnet: TIdTelnet
    OnStatus = TelnetStatus
    Host = 'localhost'
    Port = 9034
    OnTelnetCommand = TelnetTelnetCommand
    Terminal = 'U100'
    Left = 580
    Top = 244
  end
  object Timer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TimerTimer
    Left = 546
    Top = 248
  end
  object KeyTimer: TTimer
    Interval = 50
    OnTimer = KeyTimerTimer
    Left = 494
    Top = 250
  end
end
