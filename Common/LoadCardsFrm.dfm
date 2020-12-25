object LoadCardsForm: TLoadCardsForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Load Cards'
  ClientHeight = 177
  ClientWidth = 393
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 15
    Width = 47
    Height = 19
    Caption = 'Select:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 34
    Top = 42
    Width = 42
    Height = 13
    Caption = 'Card File'
  end
  object Label3: TLabel
    Left = 16
    Top = 68
    Width = 61
    Height = 19
    Caption = '** or **'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 34
    Top = 95
    Width = 56
    Height = 13
    Caption = 'Blank Cards'
  end
  object FileEdt: TEdit
    Left = 98
    Top = 39
    Width = 258
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 359
    Top = 39
    Width = 29
    Height = 21
    Caption = '...'
    TabOrder = 1
    TabStop = False
    OnClick = Button1Click
  end
  object BlankCardsEdt: TSpinEdit
    Left = 98
    Top = 92
    Width = 61
    Height = 22
    MaxValue = 2000
    MinValue = 0
    TabOrder = 2
    Value = 0
  end
  object OkBtn: TButton
    Left = 122
    Top = 135
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 3
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 196
    Top = 135
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object LoadCardsDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.asm;*.rpg;*.jcl|Hollerith 16-bit|*.h' +
      '16|Hollerith 12-bit|*.h80|ASCII|*.asc|Assembler Source|*.asm|RPG' +
      ' Source|*.rpg|Job Control|*.jcl'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 29
    Top = 128
  end
end
