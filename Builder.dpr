program Builder;
uses
  Vcl.Forms,

  KromUtils in 'common\KromUtils.pas',

  UnitBuilder in 'UnitBuilder.pas' {Form1},

  KM_BuilderKMR in 'KM_BuilderKMR.pas',
  KM_BuilderKP in 'KM_BuilderKP.pas';

{$R *.res}

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
