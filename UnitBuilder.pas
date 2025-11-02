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
    procedure btnStep3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStep2Click(Sender: TObject);
    procedure btnStep4Click(Sender: TObject);
    procedure btnStep5Click(Sender: TObject);
    procedure btnStep6Click(Sender: TObject);
    procedure btnStep1Click(Sender: TObject);
    procedure btnBuildPack7zClick(Sender: TObject);
  private
    fBuilder: TKMBuilder;
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
  fStepPanel[bsStartBuild]  := pnlStep1;
  fStepPanel[bsCleanSource] := pnlStep2;
  fStepPanel[bsBuildExe]    := pnlStep3;
  fStepPanel[bsPatchExe]    := pnlStep4;
  fStepPanel[bsPackData]    := pnlStep5;
  fStepPanel[bsCopy]        := pnlStep6;

  fStepButton[bsStartBuild]  := btnStep1;
  fStepButton[bsCleanSource] := btnStep2;
  fStepButton[bsBuildExe]    := btnStep3;
  fStepButton[bsPatchExe]    := btnStep4;
  fStepButton[bsPackData]    := btnStep5;
  fStepButton[bsCopy]        := btnStep6;

  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
  begin
    fStepButton[I].Caption := BuilderStepName[I];
    fStepPanel[I].Color := $808080;
  end;

  fBuilder := TKMBuilder.Create('Alpha 13 wip', HandleBuilderLog, HandleBuilderStepBegin, HandleBuilderStepDone, HandleBuilderDone);
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
var
  I: Integer;
begin
  for I := 0 to ControlCount - 1 do
    if (Controls[I] is TButton) and (Controls[I] <> btnStop) then
      Controls[I].Enabled := aFlag;
end;


procedure TForm1.btnBuildPack7zClick(Sender: TObject);
begin
  ControlsEnable(False);

  for var I := Low(TKMBuilderStep) to High(TKMBuilderStep) do
    fStepPanel[I].Color := $808080;

  fBuilder.Perform([bsStartBuild, bsCleanSource, bsBuildExe, bsPatchExe, bsPackData, bsCopy]);
end;

procedure TForm1.btnStep1Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsStartBuild]);
end;


procedure TForm1.btnStep2Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsCleanSource]);
end;


procedure TForm1.btnStep3Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsBuildExe]);
end;


procedure TForm1.btnStep4Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsPatchExe]);
end;


procedure TForm1.btnStep5Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsPackData]);
end;


procedure TForm1.btnStep6Click(Sender: TObject);
begin
  ControlsEnable(False);
  fBuilder.Perform([bsCopy]);
end;


procedure TForm1.btnStopClick(Sender: TObject);
begin
  fBuilder.Stop;
end;


end.
