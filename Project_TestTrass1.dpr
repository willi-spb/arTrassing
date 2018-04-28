program Project_TestTrass1;

uses
  System.StartUpCopy,
  FMX.Forms,
  fm_TestTrass1 in 'fm_TestTrass1.pas' {Form1},
  FMX.arCurveClasses in '..\FMX.arCurveClasses.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
