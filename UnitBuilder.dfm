object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Game Builder'
  ClientHeight = 617
  ClientWidth = 929
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    929
    617)
  TextHeight = 15
  object Label1: TLabel
    Left = 312
    Top = 16
    Width = 96
    Height = 15
    Caption = 'Build information:'
  end
  object Label2: TLabel
    Left = 312
    Top = 160
    Width = 50
    Height = 15
    Caption = 'Build log:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 77
    Height = 15
    Caption = 'Configuration:'
  end
  object Label4: TLabel
    Left = 16
    Top = 96
    Width = 31
    Height = 15
    Caption = 'Steps:'
  end
  object meLog: TMemo
    Left = 312
    Top = 176
    Width = 601
    Height = 425
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
  object pnlBuildStep: TPanel
    Left = 16
    Top = 112
    Width = 289
    Height = 401
    BevelOuter = bvNone
    Caption = '<Builder steps. Filled in dynamically>'
    Color = clMaroon
    TabOrder = 2
  end
  object pnlBuildConfig: TPanel
    Left = 16
    Top = 32
    Width = 289
    Height = 57
    BevelOuter = bvNone
    Caption = '<Builder configurations. Filled in dynamically>'
    TabOrder = 3
  end
  object meInfo: TMemo
    Left = 312
    Top = 32
    Width = 601
    Height = 121
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
    TabOrder = 4
  end
end
