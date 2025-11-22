unit UnitBuilder;
{$I ..\..\KM_CompilerDirectives.inc}
interface
uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes,
  KM_BuilderKP, Vcl.ExtCtrls;


type
  TForm1 = class(TForm)
    Memo1: TMemo;
    btnStop: TButton;
    edBuildVersion: TEdit;
    Label5: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    lblBuildRevision: TLabel;
    lblBuildFolder: TLabel;
    Label3: TLabel;
    edGameName: TEdit;
    pnlBuildStep: TPanel;
    pnlBuildConfig: TPanel;
    Label4: TLabel;
    lblResult7zip: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
  private
    fBuilder: TKMBuilder;
    fConfigButton: array [TKMBuilderConfiguration] of TButton;
    fStepButton: array [TKMBuilderStep] of TButton;
    fStepPanel: array [TKMBuilderStep] of TPanel;
    procedure CreateButtons;
    procedure ControlsEnable(aFlag: Boolean);
    procedure UpdateCheckboxes(aSteps: TKMBuilderStepSet);
    procedure HandleBuilderLog(aText: string);
    procedure HandleBuilderStepBegin(aStep: TKMBuilderStep);
    procedure HandleBuilderStepDone(aStep: TKMBuilderStep; aTimeMsec: Integer);
    procedure HandleBuilderDone;
    procedure HandleBuildMouseEnter(Sender: TObject);
    procedure HandleBuildMouseLeave(Sender: TObject);
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
  CreateButtons;
end;


procedure TForm1.CreateButtons;
begin
  pnlBuildConfig.Caption := '';
  for var I := Low(TKMBuilderConfiguration) to High(TKMBuilderConfiguration) do
  begin
    fConfigButton[I] := TButton.Create(pnlBuildConfig);
    fConfigButton[I].Parent := pnlBuildConfig;
    fConfigButton[I].Top := 28 * Ord(I);
    fConfigButton[I].Width := pnlBuildConfig.Width;
    fConfigButton[I].Height := 25;
    fConfigButton[I].Caption := BuilderConfigName[I];
    fConfigButton[I].Tag := Ord(I);
    fConfigButton[I].OnMouseEnter := HandleBuildMouseEnter;
    fConfigButton[I].OnMouseLeave := HandleBuildMouseLeave;
    fConfigButton[I].OnClick := btnBuildClick;
  end;

  pnlBuildStep.Caption := '';
  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
  begin
    fStepButton[I] := TButton.Create(pnlBuildStep);
    fStepButton[I].Parent := pnlBuildStep;
    fStepButton[I].Top := 28 * Ord(I);
    fStepButton[I].Width := pnlBuildStep.Width - 80;
    fStepButton[I].Height := 25;
    fStepButton[I].Caption := BuilderStepName[I];
    fStepButton[I].Tag := Ord(I);
    fStepButton[I].OnClick := btnStepClick;

    fStepPanel[I] := TPanel.Create(pnlBuildStep);
    fStepPanel[I].Parent := pnlBuildStep;
    fStepPanel[I].Left := pnlBuildStep.Width - 72;
    fStepPanel[I].Top := 28 * Ord(I);
    fStepPanel[I].Width := 72;
    fStepPanel[I].Height := 25;
    fStepPanel[I].Caption := '-';
    fStepPanel[I].Color := $CCCCCC;
    fStepPanel[I].BevelKind := bkFlat;
    fStepPanel[I].BevelOuter := bvNone;
    fStepPanel[I].ParentBackground := False;
    fStepPanel[I].ParentColor := False;
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
      lblResult7zip.Caption := fBuilder.BuildResult7zip;
    end);
end;


procedure TForm1.UpdateCheckboxes(aSteps: TKMBuilderStepSet);
begin
  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
    fStepButton[I].Visible := (I in aSteps) or (aSteps = []);
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


procedure TForm1.HandleBuildMouseEnter(Sender: TObject);
begin
  // Highlight checkboxes steps that will be executed
  var buildConfig := TKMBuilderConfiguration(TButton(Sender).Tag);
  UpdateCheckboxes(STEPS_OF_CONFIG[buildConfig]);
end;


procedure TForm1.HandleBuildMouseLeave(Sender: TObject);
begin
  // Highlight nothing
  UpdateCheckboxes([]);
end;


procedure TForm1.btnBuildClick(Sender: TObject);
begin
  fBuilder := TKMBuilder.Create(edGameName.Text, edBuildVersion.Text, HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  ControlsEnable(False);

  var buildConfig := TKMBuilderConfiguration(TButton(Sender).Tag);
  var steps: TKMBuilderStepSet := [];
  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
  begin
    if I in STEPS_OF_CONFIG[buildConfig] then
      steps := steps + [I];

    fStepPanel[I].Color := $808080;
  end;

  fBuilder.Perform(steps);
end;


procedure TForm1.btnStepClick(Sender: TObject);
begin
  if not Assigned(fBuilder) then
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
