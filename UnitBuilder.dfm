object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Game Builder'
  ClientHeight = 801
  ClientWidth = 1074
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    1074
    801)
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
    Top = 328
    Width = 50
    Height = 15
    Caption = 'Build log:'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 53
    Height = 15
    Caption = 'Scenarios:'
  end
  object Label4: TLabel
    Left = 16
    Top = 168
    Width = 31
    Height = 15
    Caption = 'Steps:'
  end
  object Label5: TLabel
    Left = 16
    Top = 712
    Width = 34
    Height = 15
    Anchors = [akLeft, akBottom]
    Caption = 'Utility:'
    ExplicitTop = 592
  end
  object meLog: TMemo
    Left = 312
    Top = 344
    Width = 746
    Height = 441
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitWidth = 742
    ExplicitHeight = 320
  end
  object btnStop: TButton
    Left = 16
    Top = 760
    Width = 289
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akBottom]
    Caption = 'Stop'
    TabOrder = 1
    OnClick = btnStopClick
    ExplicitTop = 640
  end
  object pnlBuildSteps: TPanel
    Left = 16
    Top = 184
    Width = 289
    Height = 513
    Anchors = [akLeft, akTop, akBottom]
    BevelOuter = bvNone
    Caption = '<Builder steps. Filled in dynamically>'
    Color = clMaroon
    TabOrder = 2
  end
  object pnlBuildScenarios: TPanel
    Left = 16
    Top = 32
    Width = 289
    Height = 81
    BevelOuter = bvNone
    Caption = '<Builder scenarios. Filled in dynamically>'
    TabOrder = 3
  end
  object meInfo: TMemo
    Left = 312
    Top = 32
    Width = 746
    Height = 289
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 4
    ExplicitWidth = 742
  end
  object btnBuildAllProjects: TButton
    Left = 16
    Top = 728
    Width = 289
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Anchors = [akLeft, akBottom]
    Caption = 'Build all projects'
    TabOrder = 5
    OnClick = btnBuildAllProjectsClick
    ExplicitTop = 608
  end
  object rgBuildConfiguration: TRadioGroup
    Left = 8
    Top = 120
    Width = 193
    Height = 41
    Caption = ' Configuration '
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Debug'
      'Release')
    TabOrder = 6
    OnClick = rgBuildConfigurationClick
  end
end
