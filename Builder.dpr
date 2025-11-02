program Builder;
uses
  Vcl.Forms,
  KM_Builder in 'KM_Builder.pas',
  UnitBuilder in 'UnitBuilder.pas' {Form1};

{$R *.res}

var
  Form1: TForm1;

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
