object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Game Builder'
  ClientHeight = 585
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
    585)
  TextHeight = 15
  object Label5: TLabel
    Left = 200
    Top = 16
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Game version'
    WordWrap = True
  end
  object Label1: TLabel
    Left = 16
    Top = 64
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Build revision'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 16
    Top = 104
    Width = 91
    Height = 13
    AutoSize = False
    Caption = 'Build folder'
    WordWrap = True
  end
  object lblBuildRevision: TLabel
    Left = 24
    Top = 80
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
    Left = 24
    Top = 120
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
  object Memo1: TMemo
    Left = 384
    Top = 16
    Width = 545
    Height = 553
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
    Top = 544
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
    Left = 200
    Top = 32
    Width = 177
    Height = 23
    TabOrder = 2
    Text = 'Alpha 13 wip'
  end
  object btnStep1: TButton
    Left = 40
    Top = 256
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step1'
    TabOrder = 3
    OnClick = btnStepClick
  end
  object btnStep2: TButton
    Left = 40
    Top = 288
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step2'
    TabOrder = 4
    OnClick = btnStepClick
  end
  object btnStep3: TButton
    Left = 40
    Top = 320
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step3'
    TabOrder = 5
    OnClick = btnStepClick
  end
  object btnStep4: TButton
    Left = 40
    Top = 352
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step4'
    TabOrder = 6
    OnClick = btnStepClick
  end
  object btnStep5: TButton
    Left = 40
    Top = 384
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step5'
    TabOrder = 7
    OnClick = btnStepClick
  end
  object btnStep6: TButton
    Left = 40
    Top = 416
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step6'
    TabOrder = 8
    OnClick = btnStepClick
  end
  object pnlStep1: TPanel
    Left = 232
    Top = 256
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 9
  end
  object pnlStep2: TPanel
    Left = 232
    Top = 288
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 10
  end
  object pnlStep3: TPanel
    Left = 232
    Top = 320
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 11
  end
  object pnlStep4: TPanel
    Left = 232
    Top = 352
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 12
  end
  object pnlStep5: TPanel
    Left = 232
    Top = 384
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 13
  end
  object pnlStep6: TPanel
    Left = 232
    Top = 416
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 14
  end
  object btnBuildNightly: TButton
    Left = 16
    Top = 160
    Width = 289
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Build nightly (7z)'
    TabOrder = 15
    OnClick = btnBuildNightlyClick
  end
  object btnStep7: TButton
    Left = 40
    Top = 448
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step7'
    TabOrder = 16
    OnClick = btnStepClick
  end
  object pnlStep7: TPanel
    Left = 232
    Top = 448
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 17
  end
  object btnStep8: TButton
    Left = 40
    Top = 480
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step8'
    TabOrder = 18
    OnClick = btnStepClick
  end
  object pnlStep8: TPanel
    Left = 232
    Top = 480
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 19
  end
  object cbStep1: TCheckBox
    Left = 16
    Top = 256
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 20
  end
  object cbStep2: TCheckBox
    Left = 16
    Top = 288
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 21
  end
  object cbStep3: TCheckBox
    Left = 16
    Top = 320
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 22
  end
  object cbStep4: TCheckBox
    Left = 16
    Top = 352
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 23
  end
  object cbStep5: TCheckBox
    Left = 16
    Top = 384
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 24
  end
  object cbStep6: TCheckBox
    Left = 16
    Top = 416
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 25
  end
  object cbStep7: TCheckBox
    Left = 16
    Top = 448
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 26
  end
  object cbStep8: TCheckBox
    Left = 16
    Top = 480
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 27
  end
  object btnStep9: TButton
    Left = 40
    Top = 512
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step9'
    TabOrder = 28
    OnClick = btnStepClick
  end
  object pnlStep9: TPanel
    Left = 232
    Top = 512
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 29
  end
  object cbStep9: TCheckBox
    Left = 16
    Top = 512
    Width = 17
    Height = 25
    Caption = 'Step8'
    Checked = True
    State = cbChecked
    TabOrder = 30
  end
  object edGameName: TEdit
    Left = 16
    Top = 32
    Width = 177
    Height = 23
    TabOrder = 31
    Text = 'Knights Province'
  end
  object btnBuildRelease: TButton
    Left = 16
    Top = 192
    Width = 289
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Build release (7z + Installer)'
    TabOrder = 32
    OnClick = btnBuildReleaseClick
  end
  object btnStep0: TButton
    Left = 40
    Top = 224
    Width = 185
    Height = 25
    Margins.Left = 2
    Margins.Top = 2
    Margins.Right = 2
    Margins.Bottom = 2
    Caption = 'Step1'
    TabOrder = 33
    OnClick = btnStepClick
  end
  object pnlStep0: TPanel
    Left = 232
    Top = 224
    Width = 73
    Height = 25
    BevelKind = bkFlat
    BevelOuter = bvNone
    Caption = '-'
    Color = clGray
    ParentBackground = False
    TabOrder = 34
  end
  object cbStep0: TCheckBox
    Left = 16
    Top = 224
    Width = 17
    Height = 25
    Checked = True
    State = cbChecked
    TabOrder = 35
  end
end
