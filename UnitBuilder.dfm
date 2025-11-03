object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Game Builder'
  ClientHeight = 617
  ClientWidth = 935
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    935
    617)
  TextHeight = 15
  object Label5: TLabel
    Left = 16
    Top = 56
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Game version'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 384
    Top = 16
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Build revision'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 384
    Top = 56
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Build folder'
    WordWrap = True
  end
  object lblBuildRevision: TLabel
    Left = 392
    Top = 32
    Width = 97
    Height = 25
    AutoSize = False
    Caption = 'Build revision'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object lblBuildFolder: TLabel
    Left = 392
    Top = 72
    Width = 353
    Height = 25
    AutoSize = False
    Caption = 'Build folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Game name'
    WordWrap = True
  end
  object Label4: TLabel
    Left = 384
    Top = 104
    Width = 91
    Height = 13
    AutoSize = False
    Caption = '7z archive'
    WordWrap = True
  end
  object lblResult7zip: TLabel
    Left = 392
    Top = 120
    Width = 521
    Height = 25
    AutoSize = False
    Caption = 'Build folder'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 312
    Top = 152
    Width = 607
    Height = 449
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btnStop: TButton
    Left = 16
    Top = 576
    Width = 289
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
  end
  object edBuildVersion: TEdit
    Left = 16
    Top = 72
    Width = 177
    Height = 23
    TabOrder = 2
    Text = 'Alpha 13 wip'
  end
  object edGameName: TEdit
    Left = 16
    Top = 32
    Width = 177
    Height = 23
    TabOrder = 3
    Text = 'Knights Province'
  end
  object pnlBuildStep: TPanel
    Left = 16
    Top = 168
    Width = 289
    Height = 401
    BevelOuter = bvNone
    Caption = '<Builder steps. Filled in dynamically>'
    TabOrder = 4
  end
  object pnlBuildConfig: TPanel
    Left = 16
    Top = 104
    Width = 289
    Height = 57
    BevelOuter = bvNone
    Caption = '<Builder configurations. Filled in dynamically>'
    TabOrder = 5
  end
end
