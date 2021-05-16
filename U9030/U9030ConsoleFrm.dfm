object U9030ConsoleForm: TU9030ConsoleForm
  Left = 0
  Top = 0
  Caption = '90/30 Console'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Courier New'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnKeyUp = FormKeyUp
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object Telnet: TIdTelnet
    OnStatus = TelnetStatus
    Host = 'localhost'
    Port = 9030
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
end
