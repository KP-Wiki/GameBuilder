unit UnitBuilder;
interface
uses
  System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  KM_BuilderManager;


type
  TForm1 = class(TForm)
    meLog: TMemo;
    btnStop: TButton;
    pnlBuildSteps: TPanel;
    pnlBuildScenarios: TPanel;
    meInfo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    btnBuildAllProjects: TButton;
    Label5: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnScenarioClick(Sender: TObject);
    procedure btnBuildAllProjectsClick(Sender: TObject);
  private
    fGame: TKMBuilderGame;
    fBuilderManager: TKMBuilderManager;
    fScenarioButton: array {Scenario} of TButton;
    fStepButton: array {Step} of TButton;
    fStepPanel: array {Step} of TPanel;
    procedure CreateButtons;
    procedure ControlsEnable(aFlag: Boolean);
    procedure UpdateStepVisibility(aScenario: Integer);
    procedure HandleBuilderLog(aText: string);
    procedure HandleBuilderStepBegin(aStep: Integer);
    procedure HandleBuilderStepDone(aStep: Integer; aTimeMsec: Integer);
    procedure HandleBuilderScenarioDone;
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
  if FileExists('KaM_Remake.dpr') then
    fGame := bgKMR
  else
  if FileExists('KnightsProvince.dpr') then
    fGame := bgKP;

  fBuilderManager := TKMBuilderManager.Create(fGame, HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderScenarioDone);

  meInfo.Text := fBuilderManager.GetInfo;

  CreateButtons;
end;


procedure TForm1.CreateButtons;
begin
  pnlBuildScenarios.Caption := '';
  SetLength(fScenarioButton, fBuilderManager.GetScenarioCount);
  for var I := 0 to fBuilderManager.GetScenarioCount - 1 do
  begin
    fScenarioButton[I] := TButton.Create(pnlBuildScenarios);
    fScenarioButton[I].Parent := pnlBuildScenarios;
    fScenarioButton[I].Top := 28 * Ord(I);
    fScenarioButton[I].Width := pnlBuildScenarios.Width;
    fScenarioButton[I].Height := 25;
    fScenarioButton[I].Caption := fBuilderManager.GetScenarioName(I);
    fScenarioButton[I].Tag := Ord(I);
    fScenarioButton[I].OnMouseEnter := HandleBuildMouseEnter;
    fScenarioButton[I].OnMouseLeave := HandleBuildMouseLeave;
    fScenarioButton[I].OnClick := btnScenarioClick;
  end;

  pnlBuildSteps.Caption := '';
  SetLength(fStepButton, fBuilderManager.GetStepCount);
  SetLength(fStepPanel, fBuilderManager.GetStepCount);
  for var I := 0 to fBuilderManager.GetStepCount - 1 do
  begin
    fStepButton[I] := TButton.Create(pnlBuildSteps);
    fStepButton[I].Parent := pnlBuildSteps;
    fStepButton[I].Top := 28 * Ord(I);
    fStepButton[I].Width := pnlBuildSteps.Width - 80;
    fStepButton[I].Height := 25;
    fStepButton[I].Caption := fBuilderManager.GetStepName(I);
    fStepButton[I].Tag := Ord(I);
    fStepButton[I].OnClick := btnStepClick;

    fStepPanel[I] := TPanel.Create(pnlBuildSteps);
    fStepPanel[I].Parent := pnlBuildSteps;
    fStepPanel[I].Left := pnlBuildSteps.Width - 72;
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


procedure TForm1.HandleBuilderStepBegin(aStep: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      meInfo.Text := fBuilderManager.GetInfo;

      meLog.Lines.Append(Format('>>>--- Step "%s"', [fBuilderManager.GetStepName(aStep)]));
      fStepPanel[aStep].Color := $80FFFF;
    end);
end;


procedure TForm1.HandleBuilderStepDone(aStep: Integer; aTimeMsec: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      meInfo.Text := fBuilderManager.GetInfo;

      meLog.Lines.Append(Format('>>>--- Done "%s"', [fBuilderManager.GetStepName(aStep)]));
      fStepPanel[aStep].Caption := Format('%.1fsec', [aTimeMsec / 1000]);
      fStepPanel[aStep].Color := $80FF80;
    end);
end;


procedure TForm1.UpdateStepVisibility(aScenario: Integer);
begin
  for var I := 0 to fBuilderManager.GetStepCount - 1 do
    fStepButton[I].Visible := (aScenario = -1) or fBuilderManager.GetScenarioContainsStep(aScenario, I);
end;


procedure TForm1.HandleBuilderScenarioDone;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      ControlsEnable(True);
      meLog.Lines.Append('Scenario done');
    end);
end;


procedure TForm1.HandleBuilderLog(aText: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      var sw := TStreamWriter.Create(ChangeFileExt(Application.ExeName, '.log'), True);
      sw.WriteLine(aText);
      sw.Free;

      meLog.Lines.Append(aText);
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
  var buildConfig := TButton(Sender).Tag;
  UpdateStepVisibility(buildConfig);
end;


procedure TForm1.HandleBuildMouseLeave(Sender: TObject);
begin
  UpdateStepVisibility(-1);
end;


procedure TForm1.btnBuildAllProjectsClick(Sender: TObject);
begin
  meLog.Lines.Append('>>>--- Building all projects ..');
  fBuilderManager.ExecuteWholeProjectGroup;
  meLog.Lines.Append('>>>--- Building all projects done');
end;


procedure TForm1.btnScenarioClick(Sender: TObject);
begin
  meInfo.Text := fBuilderManager.GetInfo;

  ControlsEnable(False);

  for var I := 0 to fBuilderManager.GetStepCount - 1 do
    fStepPanel[I].Color := $808080;

  var buildScenario := TButton(Sender).Tag;
  fBuilderManager.ExecuteScenario(buildScenario);
end;


procedure TForm1.btnStepClick(Sender: TObject);
begin
  meInfo.Text := fBuilderManager.GetInfo;

  ControlsEnable(False);
  var step := TButton(Sender).Tag;
  fBuilderManager.ExecuteStep(step);
end;


procedure TForm1.btnStopClick(Sender: TObject);
begin
  fBuilderManager.Stop;
end;


end.
