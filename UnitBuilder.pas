unit UnitBuilder;
{$I ..\..\KM_CompilerDirectives.inc}
interface
uses
  Vcl.Forms, Vcl.StdCtrls, Vcl.Controls, System.Classes,
  System.SysUtils,
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
    procedure ControlsEnable(aFlag: Boolean);
  end;


implementation
uses
  KM_Const,
  KromUtils;


{$R *.dfm}


{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := GAME_TITLE + ' - Builder (' + GAME_REVISION + ')';

  ExeDir := ExtractFilePath(Application.ExeName);

  fBuilder := TKMBuilder.Create('Alpha 13 wip',
    procedure(aText: string)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          Memo1.Lines.Append(aText);
        end);
    end,
    procedure (aStep: TKMBuilderStep; aTimeMsec: Integer)
    begin
      TThread.Synchronize(nil,
        procedure
        begin
          case aStep of
            bsStartBuild:   pnlStep1.Caption := Format('%dms', [aTimeMsec]);
            bsCleanSource:  pnlStep2.Caption := Format('%dms', [aTimeMsec]);
            bsBuildExe:     pnlStep3.Caption := Format('%dms', [aTimeMsec]);
            bsPatchExe:     pnlStep4.Caption := Format('%dms', [aTimeMsec]);
            bsPackData:     pnlStep5.Caption := Format('%dms', [aTimeMsec]);
            bsCopy:         pnlStep6.Caption := Format('%dms', [aTimeMsec]);
          end;

          ControlsEnable(True);
          Memo1.Lines.Append('Task done');
        end);
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
  //"C:\Program Files (x86)\Inno Setup 6\iscc.exe" "installer\InstallerFull.iss"
end;


end.
