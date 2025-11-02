unit UnitBuilder;
{$I ..\..\KM_CompilerDirectives.inc}
interface
uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes,
  KM_Builder, Vcl.ExtCtrls;


type
  TForm1 = class(TForm)
    btnStep3: TButton;
    Memo1: TMemo;
    btnStop: TButton;
    edBuildVersion: TEdit;
    Label5: TLabel;
    btnStep2: TButton;
    btnStep4: TButton;
    btnStep5: TButton;
    btnStep6: TButton;
    btnStep1: TButton;
    pnlStep1: TPanel;
    pnlStep2: TPanel;
    pnlStep3: TPanel;
    pnlStep4: TPanel;
    pnlStep5: TPanel;
    pnlStep6: TPanel;
    btnBuildPack7z: TButton;
    btnStep7: TButton;
    pnlStep7: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    lblBuildRevision: TLabel;
    lblBuildFolder: TLabel;
    btnStep8: TButton;
    pnlStep8: TPanel;
    cbStep1: TCheckBox;
    cbStep2: TCheckBox;
    cbStep3: TCheckBox;
    cbStep4: TCheckBox;
    cbStep5: TCheckBox;
    cbStep6: TCheckBox;
    cbStep7: TCheckBox;
    cbStep8: TCheckBox;
    btnStep9: TButton;
    pnlStep9: TPanel;
    cbStep9: TCheckBox;
    Label3: TLabel;
    edGameName: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnBuild(Sender: TObject);
  private
    fBuilder: TKMBuilder;
    fStepCheckBox: array [TKMBuilderStep] of TCheckBox;
    fStepButton: array [TKMBuilderStep] of TButton;
    fStepPanel: array [TKMBuilderStep] of TPanel;
    procedure ControlsEnable(aFlag: Boolean);
    procedure HandleBuilderLog(aText: string);
    procedure HandleBuilderStepBegin(aStep: TKMBuilderStep);
    procedure HandleBuilderStepDone(aStep: TKMBuilderStep; aTimeMsec: Integer);
    procedure HandleBuilderDone;
  end;


implementation
uses
  System.SysUtils,
  Vcl.Graphics,
  KromUtils;


{$R *.dfm}


{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  fStepCheckBox[bsStartBuild]     := cbStep1;
  fStepCheckBox[bsCleanSource]    := cbStep2;
  fStepCheckBox[bsBuildExe]       := cbStep3;
  fStepCheckBox[bsPatchExe]       := cbStep4;
  fStepCheckBox[bsPackData]       := cbStep5;
  fStepCheckBox[bsCopy]           := cbStep6;
  fStepCheckBox[bsPack7zip]       := cbStep7;
  fStepCheckBox[bsPackInstaller]  := cbStep8;
  fStepCheckBox[bsCommitAndTag]   := cbStep9;

  fStepButton[bsStartBuild]     := btnStep1;
  fStepButton[bsCleanSource]    := btnStep2;
  fStepButton[bsBuildExe]       := btnStep3;
  fStepButton[bsPatchExe]       := btnStep4;
  fStepButton[bsPackData]       := btnStep5;
  fStepButton[bsCopy]           := btnStep6;
  fStepButton[bsPack7zip]       := btnStep7;
  fStepButton[bsPackInstaller]  := btnStep8;
  fStepButton[bsCommitAndTag]   := btnStep9;

  fStepPanel[bsStartBuild]    := pnlStep1;
  fStepPanel[bsCleanSource]   := pnlStep2;
  fStepPanel[bsBuildExe]      := pnlStep3;
  fStepPanel[bsPatchExe]      := pnlStep4;
  fStepPanel[bsPackData]      := pnlStep5;
  fStepPanel[bsCopy]          := pnlStep6;
  fStepPanel[bsPack7zip]      := pnlStep7;
  fStepPanel[bsPackInstaller] := pnlStep8;
  fStepPanel[bsCommitAndTag]  := pnlStep9;

  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
  begin
    fStepButton[I].Caption := BuilderStepName[I];
    fStepButton[I].Tag := Ord(I);
    fStepPanel[I].Color := $808080;
  end;
end;


procedure TForm1.HandleBuilderStepBegin(aStep: TKMBuilderStep);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Append(Format('>>>--- Step "%s"', [BuilderStepName[aStep]]));
      fStepPanel[aStep].Color := $80FFFF;
    end);
end;


procedure TForm1.HandleBuilderStepDone(aStep: TKMBuilderStep; aTimeMsec: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Append(Format('>>>--- Done "%s"', [BuilderStepName[aStep]]));
      fStepPanel[aStep].Caption := Format('%dms', [aTimeMsec]);
      fStepPanel[aStep].Color := $80FF80;

      lblBuildRevision.Caption := IntToStr(fBuilder.BuildRevision);
      lblBuildFolder.Caption := fBuilder.BuildFolder;
    end);
end;


procedure TForm1.HandleBuilderDone;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      ControlsEnable(True);
      Memo1.Lines.Append('Task done');
    end);
end;


procedure TForm1.HandleBuilderLog(aText: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      Memo1.Lines.Append(aText);
    end);
end;


procedure TForm1.ControlsEnable(aFlag: Boolean);
begin
  for var I := 0 to ControlCount - 1 do
    if (Controls[I] is TButton) and (Controls[I] <> btnStop) then
      Controls[I].Enabled := aFlag;
end;


procedure TForm1.btnBuild(Sender: TObject);
begin
  fBuilder := TKMBuilder.Create(edGameName.Text, edBuildVersion.Text, HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  ControlsEnable(False);

  var steps: TKMBuilderStepSet;
  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
  begin
    if fStepCheckBox[I].Checked then
      steps := steps + [I];

    fStepPanel[I].Color := $808080;
  end;

  fBuilder.Perform(steps);
end;

procedure TForm1.btnStepClick(Sender: TObject);
begin
  fBuilder := TKMBuilder.Create(edGameName.Text, edBuildVersion.Text, HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  ControlsEnable(False);
  var step := TKMBuilderStep(TButton(Sender).Tag);
  fBuilder.Perform([step]);
end;


procedure TForm1.btnStopClick(Sender: TObject);
begin
  fBuilder.Stop;
end;


end.
