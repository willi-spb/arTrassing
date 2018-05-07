program Project_TestTrass1;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.arCurveClasses in 'FMX.arCurveClasses.pas',
  fm_TestTrass1 in 'fm_TestTrass1.pas' {Form1};

{$R *.res}

begin
  {$IFDEF DEBUG}
     ReportMemoryLeaksOnShutdown:=true;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
