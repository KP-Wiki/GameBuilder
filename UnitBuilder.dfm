object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Builder'
  ClientHeight = 513
  ClientWidth = 945
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  DesignSize = (
    945
    513)
  TextHeight = 15
  object Label5: TLabel
    Left = 16
    Top = 16
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Game version'
    WordWrap = True
  end
  object Memo1: TMemo
    Left = 328
    Top = 16
    Width = 601
    Height = 481
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
    Top = 472
    Width = 178
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
    Top = 32
    Width = 177
    Height = 23
    TabOrder = 2
    Text = 'Alpha 13 wip'
  end
  object btnStep1: TButton
    Left = 16
    Top = 120
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step1_Commit'
    TabOrder = 3
    OnClick = btnStep1Click
  end
  object btnStep2: TButton
    Left = 16
    Top = 152
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step3_CleanSources'
    TabOrder = 4
    OnClick = btnStep2Click
  end
  object btnStep3: TButton
    Left = 16
    Top = 184
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step4_BuildGameExe'
    TabOrder = 5
    OnClick = btnStep3Click
  end
  object btnStep4: TButton
    Left = 16
    Top = 216
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step5_PatchGameExe'
    TabOrder = 6
    OnClick = btnStep4Click
  end
  object btnStep5: TButton
    Left = 16
    Top = 248
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step6 PackData'
    TabOrder = 7
    OnClick = btnStep5Click
  end
  object btnStep6: TButton
    Left = 16
    Top = 280
    Width = 177
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step7 CopyIntoBuildFolder'
    TabOrder = 8
    OnClick = btnStep6Click
  end
  object pnlStep1: TPanel
    Left = 200
    Top = 120
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 9
  end
  object pnlStep2: TPanel
    Left = 200
    Top = 152
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 10
  end
  object pnlStep3: TPanel
    Left = 200
    Top = 184
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 11
  end
  object pnlStep4: TPanel
    Left = 200
    Top = 216
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 12
  end
  object pnlStep5: TPanel
    Left = 200
    Top = 248
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 13
  end
  object pnlStep6: TPanel
    Left = 200
    Top = 280
    Width = 65
    Height = 25
    Caption = '-'
    TabOrder = 14
  end
  object btnBuildPack7z: TButton
    Left = 16
    Top = 88
    Width = 249
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Build pack7z'
    TabOrder = 15
    OnClick = btnBuildPack7zClick
  end
end
