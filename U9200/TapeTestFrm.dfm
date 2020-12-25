object TapeTestForm: TTapeTestForm
  Left = 0
  Top = 0
  Caption = 'AWS File Format Tester'
  ClientHeight = 337
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 14
    Top = 30
    Width = 46
    Height = 13
    Caption = 'File Name'
  end
  object ProgressLbl: TLabel
    Left = 4
    Top = 198
    Width = 55
    Height = 13
    Caption = 'ProgressLbl'
  end
  object Label2: TLabel
    Left = 14
    Top = 56
    Width = 56
    Height = 13
    Caption = 'Num to Skip'
  end
  object FileNameEdt: TEdit
    Left = 77
    Top = 27
    Width = 355
    Height = 21
    TabOrder = 0
    Text = 'c:\temp\AWS\cbt490.583'
  end
  object ReadFwdBtn: TButton
    Left = 77
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Read Fwd'
    TabOrder = 1
    OnClick = ReadFwdBtnClick
  end
  object SkipEdt: TEdit
    Left = 77
    Top = 53
    Width = 42
    Height = 21
    NumbersOnly = True
    TabOrder = 2
  end
  object FileFwdBtn: TButton
    Left = 151
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Fwd Spc File'
    TabOrder = 3
    OnClick = FileFwdBtnClick
  end
  object OpenBtn: TButton
    Left = 3
    Top = 139
    Width = 75
    Height = 25
    Caption = 'Open'
    TabOrder = 4
    OnClick = OpenBtnClick
  end
  object ReadBkwdBtn: TButton
    Left = 77
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Read Bkwd'
    TabOrder = 5
    OnClick = ReadBkwdBtnClick
  end
  object FileBkwdBtn: TButton
    Left = 151
    Top = 163
    Width = 75
    Height = 25
    Caption = 'Bkwd Spc File'
    TabOrder = 6
    OnClick = FileBkwdBtnClick
  end
end
