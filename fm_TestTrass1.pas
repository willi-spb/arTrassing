unit fm_TestTrass1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts, FMX.ExtCtrls,
  FMX.Objects;

type
  TForm1 = class(TForm)
    btn1: TButton;
    img1: TImage;
    procedure btn1Click(Sender: TObject);
  private
    { Private declarations }
   // LL:TAlphaColor
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses FMX.arCurveClasses;

procedure TForm1.btn1Click(Sender: TObject);
var LCv:TcvCurve;
    Lx,Ly:single;
    i:integer;
begin
 LCv:=TcvCurve.Create(0,TAlphaColorRec.Maroon,1,TStrokeDash.Solid,TmarkType.mpArrow,8);
 Lx:=12; Ly:=200;
 for I :=0 to 20 do
  begin
    Lx:=Lx+2; Ly:=Ly-4;
    LCv.Points.Add(TcvPoint.Create(Lx,Ly));
  end;
  Lcv.MarkFill.Kind:=TBrushKind.Solid;
  Lcv.MarkFilled:=true;

  img1.Bitmap:=TBitMap.Create(Trunc(img1.Width),Trunc(img1.Height));
  LCv.ScalePoints(img1.Bitmap,LCv.GetAreaRect);
  img1.Bitmap.Canvas.BeginScene;
  try
   LCv.DrawPointLines;
   LCv.DrawPtMarks;
  finally
    img1.Bitmap.Canvas.EndScene;
  end;

end;

end.
