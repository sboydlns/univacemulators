object ReverseCardFileForm: TReverseCardFileForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Reverse Card File'
  ClientHeight = 176
  ClientWidth = 476
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 26
    Top = 26
    Width = 45
    Height = 13
    Caption = 'Input File'
  end
  object Label2: TLabel
    Left = 26
    Top = 59
    Width = 53
    Height = 13
    Caption = 'Output File'
  end
  object Label3: TLabel
    Left = 93
    Top = 135
    Width = 214
    Height = 33
    Caption = 
      'A quick and dirty program to fix card files that were scanned in' +
      ' reverse order'
    WordWrap = True
  end
  object InputFileEdt: TEdit
    Left = 93
    Top = 23
    Width = 328
    Height = 21
    TabOrder = 0
  end
  object OutputFileEdt: TEdit
    Left = 93
    Top = 56
    Width = 328
    Height = 21
    TabOrder = 1
  end
  object InputFileBtn: TButton
    Left = 424
    Top = 21
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 2
    TabStop = False
    OnClick = InputFileBtnClick
  end
  object OutputFileBtn: TButton
    Left = 424
    Top = 54
    Width = 27
    Height = 25
    Caption = '...'
    TabOrder = 3
    TabStop = False
    OnClick = OutputFileBtnClick
  end
  object OkBtn: TButton
    Left = 93
    Top = 100
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 4
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 168
    Top = 100
    Width = 75
    Height = 25
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = CancelBtnClick
  end
  object OpenDlg: TOpenDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.asm;*.rpg;*.jcl|Hollerith 16-bit|*.' +
      'h16|Hollerith 12-bit|*.h80|ASCII|*.asc|Assembler Source|*.asm|RP' +
      'G Source|*.rpg|Job Control;*.jcl'
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 383
    Top = 2
  end
  object SaveDlg: TSaveDialog
    Filter = 
      'All Card Files|*.h16;*.h80;*.asc;*.asm;*.rpg;*.jcl|Hollerith 16-bit|*.' +
      'h16|Hollerith 12-bit|*.h80|ASCII|*.asc|Assembler Source|*.asm|RP' +
      'G Source|*.rpg|Job Control;*.jcl'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 384
    Top = 56
  end
end
