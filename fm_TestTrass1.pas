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
    tmr1: TTimer;
    procedure btn1Click(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure tmr1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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

uses FMX.arCurveClasses, Math;

var LArea:TarChartArea;
    LBM:TBitmap;

procedure TForm1.btn1Click(Sender: TObject);
var LCv:TarCurve;
    Lx,Ly:single;
    i:integer;
  //  LArea:TarChartArea;
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
  ///
  ///  отрицательная ширина - это значение отступа от соседнего графика в единицах
  //Lcv.SetColumnsType(-5,TAlphaColorRec.Yellow,0.6);
    Lcv.SetColumnsType(26,TAlphaColorRec.Yellow,0.6);
  ///
  LBM:=TBitMap.Create(Trunc(img1.Width),Trunc(img1.Height));
  img1.Bitmap:=LBM;
  LArea.Curves.Add(Lcv);
  LArea.SetCanvas(img1.Bitmap.Canvas);
  ///
  LArea.SetAreaParams(RectF(0,0,500,200),img1.Bitmap.BoundsF);
  LArea.SetAxisDividerValues(0,8);
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
   LArea.ApplyAutoMargins(25,4);
   LArea.ResetAreaParams;
   LArea.SetAxisTextLabels('Привет, %','Среднеквадратическое отклонение от значения, ККал.');
   LArea.RedrawAll;
   ///
  finally
    img1.Bitmap.Canvas.EndScene;
  end;
 btn1.Tag:=1;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 if Assigned(LBM) then
    LBM.Free;
  if Assigned(LArea) then
     LArea.Free;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
 if btn1.Tag=1 then
  begin
   tmr1.Enabled:=false;
   tmr1.Enabled:=true;
  end;
end;

procedure TForm1.tmr1Timer(Sender: TObject);
begin
  lbl1.Text:=IntToStr(Random(10000))+' FFFFFFFF';
 tmr1.Enabled:=false;
{ if Assigned(LBM)=false then exit;
 LBM.SetSize(Trunc(img1.Width),Trunc(img1.Height));
 img1.Bitmap:=LBM;
 LArea.SetCanvas(img1.Bitmap.Canvas);
 img1.Bitmap.Canvas.BeginScene;
 try
  LArea.ResetCanvasArea(img1.Bitmap.BoundsF);
  LArea.RedrawAll;
  LArea.ResetAutoMargins;
  LArea.ResetAreaParams;
  LArea.RedrawAll;
 finally
   img1.Bitmap.Canvas.EndScene;
 end;
 }
  LArea.PaintToBitmap(LBM,RectF(0,0,img1.Width,img1.Height),false);
  img1.Bitmap:=LBM;
end;

end.

