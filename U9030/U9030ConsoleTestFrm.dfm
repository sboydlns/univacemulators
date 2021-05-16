object U9030ConsoleTestForm: TU9030ConsoleTestForm
  Left = 0
  Top = 0
  Caption = 'Console Test'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  DesignSize = (
    635
    299)
  PixelsPerInch = 96
  TextHeight = 13
  object GoBtn: TButton
    Left = 4
    Top = 2
    Width = 75
    Height = 25
    Caption = 'Go'
    TabOrder = 0
    OnClick = GoBtnClick
  end
  object Memo: TMemo
    Left = 4
    Top = 32
    Width = 627
    Height = 265
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object Telnet: TIdTelnetServer
    Bindings = <>
    DefaultPort = 9030
    MaxConnections = 1
    OnConnect = TelnetConnect
    OnListenException = TelnetListenException
    LoginAttempts = 0
    LoginMessage = 'Sperry*Univac 90/30 Console'
    OnAuthentication = TelnetAuthentication
    OnExecute = TelnetExecute
    Left = 560
    Top = 248
  end
end
