unit UnitBuilder;
interface
uses
  System.Classes,
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, Vcl.ExtCtrls,
  KM_BuilderCommon, KM_BuilderKMR, KM_BuilderKP;


type
  TForm1 = class(TForm)
    meLog: TMemo;
    btnStop: TButton;
    pnlBuildStep: TPanel;
    pnlBuildConfig: TPanel;
    meInfo: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;   procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStepClick(Sender: TObject);
    procedure btnBuildClick(Sender: TObject);
  private
    fBuilder: TKMBuilder;
    fConfigButton: array {Configuration} of TButton;
    fStepButton: array {Step} of TButton;
    fStepPanel: array {Step} of TPanel;
    procedure CreateButtons;
    procedure ControlsEnable(aFlag: Boolean);
    procedure UpdateCheckboxes(aConfig: Integer);
    procedure HandleBuilderLog(aText: string);
    procedure HandleBuilderStepBegin(aStep: Integer);
    procedure HandleBuilderStepDone(aStep: Integer; aTimeMsec: Integer);
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
  //todo: Decide on the run mode
  //fBuilder := TKMBuilderKP.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);
  fBuilder := TKMBuilderKMR.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  meInfo.Text := fBuilder.GetInfo;

  CreateButtons;
end;


procedure TForm1.CreateButtons;
begin
  pnlBuildConfig.Caption := '';
  SetLength(fConfigButton, fBuilder.GetConfigCount);
  for var I := 0 to fBuilder.GetConfigCount - 1 do
  begin
    fConfigButton[I] := TButton.Create(pnlBuildConfig);
    fConfigButton[I].Parent := pnlBuildConfig;
    fConfigButton[I].Top := 28 * Ord(I);
    fConfigButton[I].Width := pnlBuildConfig.Width;
    fConfigButton[I].Height := 25;
    fConfigButton[I].Caption := fBuilder.GetConfigName(I);
    fConfigButton[I].Tag := Ord(I);
    fConfigButton[I].OnMouseEnter := HandleBuildMouseEnter;
    fConfigButton[I].OnMouseLeave := HandleBuildMouseLeave;
    fConfigButton[I].OnClick := btnBuildClick;
  end;

  pnlBuildStep.Caption := '';
  SetLength(fStepButton, fBuilder.GetStepCount);
  SetLength(fStepPanel, fBuilder.GetStepCount);
  for var I := 0 to fBuilder.GetStepCount - 1 do
  begin
    fStepButton[I] := TButton.Create(pnlBuildStep);
    fStepButton[I].Parent := pnlBuildStep;
    fStepButton[I].Top := 28 * Ord(I);
    fStepButton[I].Width := pnlBuildStep.Width - 80;
    fStepButton[I].Height := 25;
    fStepButton[I].Caption := fBuilder.GetStepName(I);
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


procedure TForm1.HandleBuilderStepBegin(aStep: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      meLog.Lines.Append(Format('>>>--- Step "%s"', [fBuilder.GetStepName(aStep)]));
      fStepPanel[aStep].Color := $80FFFF;
    end);
end;


procedure TForm1.HandleBuilderStepDone(aStep: Integer; aTimeMsec: Integer);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      meLog.Lines.Append(Format('>>>--- Done "%s"', [fBuilder.GetStepName(aStep)]));
      fStepPanel[aStep].Caption := Format('%dms', [aTimeMsec]);
      fStepPanel[aStep].Color := $80FF80;

      meInfo.Text := fBuilder.GetInfo;
    end);
end;


procedure TForm1.UpdateCheckboxes(aConfig: Integer);
begin
  for var I := 0 to fBuilder.GetStepCount - 1 do
    fStepButton[I].Visible := (aConfig = -1) or fBuilder.GetConfigContainsStep(aConfig, I);
end;


procedure TForm1.HandleBuilderDone;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      ControlsEnable(True);
      meLog.Lines.Append('Task done');
    end);
end;


procedure TForm1.HandleBuilderLog(aText: string);
begin
  TThread.Synchronize(nil,
    procedure
    begin
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
  // Highlight checkboxes steps that will be executed
  var buildConfig := TButton(Sender).Tag;
  UpdateCheckboxes(buildConfig);
end;


procedure TForm1.HandleBuildMouseLeave(Sender: TObject);
begin
  // Highlight nothing
  UpdateCheckboxes(-1);
end;


procedure TForm1.btnBuildClick(Sender: TObject);
begin
  fBuilder.Free;
  //fBuilder := TKMBuilderKP.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);
  fBuilder := TKMBuilderKMR.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  meInfo.Text := fBuilder.GetInfo;

  ControlsEnable(False);

  var buildConfig := TButton(Sender).Tag;
  for var I := 0 to fBuilder.GetStepCount - 1 do
    fStepPanel[I].Color := $808080;

  fBuilder.ExecuteConfig(buildConfig);
end;


procedure TForm1.btnStepClick(Sender: TObject);
begin
  if not Assigned(fBuilder) then
    //fBuilder := TKMBuilderKP.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);
    fBuilder := TKMBuilderKMR.Create(HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);

  meInfo.Text := fBuilder.GetInfo;

  ControlsEnable(False);
  var step := TButton(Sender).Tag;
  fBuilder.ExecuteStep(step);
end;


procedure TForm1.btnStopClick(Sender: TObject);
begin
  fBuilder.Stop;
end;


end.
