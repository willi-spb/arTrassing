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
    lbl1: TLabel;
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
var LCv:TarCurve;
    Lx,Ly:single;
    i:integer;
    LArea:TarChartArea;
begin
 LCv:=TarCurve.Create(0,TAlphaColorRec.Maroon,1,TStrokeDash.Solid,TmarkType.mpArrow,8);
 LArea:=TarChartArea.Create(TAlphaColorRec.Darkgray,TAlphaColorRec.Black,TStrokeDash.Dash,0.7);
 Lx:=50; Ly:=0;
 for I :=0 to 20 do
  begin
    LCv.Points.Add(TcvPoint.Create(i,Lx,Ly));
    Lx:=Lx+20; Ly:=Ly+6;
  end;
  Lcv.MarkFill.Kind:=TBrushKind.Solid;
  Lcv.MarkFilled:=true;
  Lcv.TitleColor:=TAlphaColorRec.Green;

  img1.Bitmap:=TBitMap.Create(Trunc(img1.Width),Trunc(img1.Height));
  LArea.Curves.Add(Lcv);
  LArea.SetCanvas(img1.Bitmap.Canvas);
  ///
  LArea.SetAreaParam(RectF(0,0,500,200),img1.Bitmap.BoundsF);
//  LCv.ScalePoints(LCv.GetAreaRect,LArea.GetActiveArea);
//  LCv.ScalePoints(RectF(0,0,500,200),LArea.GetActiveArea);

  LArea.AddCurve(1,TAlphaColorRec.Darkcyan,1,TStrokeDash.Solid,TmarkType.mpRect,5);
  LArea.Curves.Last.MarkFilled:=true;
   Lx:=-20; Ly:=0;
   for I :=0 to 200 do
    begin
     Larea.Curves.Last.Points.Add(TcvPoint.Create(i,Lx,Ly));
     Lx:=Lx+2; Ly:=Ly+0.6;
    end;
   Larea.Curves.Last.TitleType:=ttYValue;
   Larea.Curves.Last.MarkedEvery(20);
   Larea.Curves.Last.MarkedSeveral([5,10,15]);
  img1.Bitmap.Canvas.BeginScene;
  try
  //  LCv.DrawPointLines;
  //   LCv.DrawPtMarks;
   LCv.TitleType:=ttXYvalue;
   LCv.TitleHorzAlign:=TTextAlign.Center;

   LArea.RedrawAll;
   LArea.ApplyAutoMargins;
   LArea.SetAreaParam(RectF(0,0,500,200),img1.Bitmap.BoundsF);
   LArea.RedrawAll;
   ///
  finally
    img1.Bitmap.Canvas.EndScene;
  end;

end;

end.

