program Builder;
uses
  Vcl.Forms,
  UnitBuilder in 'UnitBuilder.pas' {Form1},
  KM_BuilderKP in 'KM_BuilderKP.pas',
  KromUtils in 'common\KromUtils.pas';

{$R *.res}

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
