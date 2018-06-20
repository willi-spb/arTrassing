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
 if btn1.Tag=1 then Exit;

// пробный вариант - создать отдельно объект кривая а затем прицепить его к графику
 LCv:=TarCurve.Create(0,TAlphaColorRec.Maroon,1,TStrokeDash.Solid,TmarkType.mpArrow,8);
 // создать график с параметрами, отличными от умолчаний
 LArea:=TarChartArea.Create(TAlphaColorRec.Darkgray,TAlphaColorRec.Black,TStrokeDash.Dash,0.7);
 // добавить к кривой - см. выше точки - используем линейную зависимость
 Lx:=50; Ly:=0;
 for I :=0 to 20 do
  begin
    LCv.Points.Add(TcvPoint.Create(i,Lx,Ly));
    Lx:=Lx+20; Ly:=Ly+6;
  end;
  // выставим параметры для кривой
  Lcv.MarkFill.Kind:=TBrushKind.Solid;
  Lcv.MarkFilled:=true;
  Lcv.TitleColor:=TAlphaColorRec.Navy;
  ///
  /// v.1 - выставить столбцы - т.е. сделать не график кривой, а набор столбиков - меняем тип кривой
  ///  отрицательная ширина - это значение отступа от соседнего графика в единицах
  //Lcv.SetColumnsType(,TAlphaColorRec.Yellow,0.6);
    Lcv.SetColumnsType(26,TAlphaColorRec.Yellow,4,0.6);
  ///
  /// v.2 - выставить кривую в виде уровней - меняем тип кривой
    LCv.SetLevelsType(TAlphaColorRec.Yellow,0.5);
    /// заполнить кривую снизу цветом заполнения и прозрачностью
    LCv.BottomFilled:=true;
  ///
  ///  создать битмап, на который будем рисовать
  LBM:=TBitMap.Create(Trunc(img1.Width),Trunc(img1.Height));
  /// назначить созданный битмап картинке
  img1.Bitmap:=LBM;
  /// добавить отдельносозданную кривую к графику
  LArea.Curves.Add(Lcv);
  /// hide Lcv Curve
 //  Lcv.Enabled:=false;
  /// переназначить указатели Canvas на тот, что в BitMap
  LArea.SetCanvas(img1.Bitmap.Canvas);
  ///
  /// задать размеры и точку вывода на битмапе графика
  LArea.SetAreaParams(RectF(0,0,500,200),img1.Bitmap.BoundsF);
  /// задать параметры деления для осей
  LArea.SetAxisDividerValues(0,8);
//  LCv.ScalePoints(LCv.GetAreaRect,LArea.GetActiveArea);
//  LCv.ScalePoints(RectF(0,0,500,200),LArea.GetActiveArea);

/// штатный вариант - к созданному графику добавляется (создается) новая кривая
  LArea.AddCurve(1,TAlphaColorRec.Darkcyan,1,TStrokeDash.Solid,TmarkType.mpRect,5);
/// для последней кривой графика задаются параметры
  LArea.Curves.Last.MarkFilled:=true;
  /// Filled
  with LArea.Curves.Last do
    begin
       Fill.Color:=TAlphaColorRec.Aqua;
       FillOpacity:=0.8;
       BottomFilled:=true;
    end;
  ///
{ ///  заполнить новую кривую графика точками по линейной зависисмости, задать параметры
   Lx:=-20; Ly:=0;
   for I :=0 to 200 do
    begin
     Larea.Curves.Last.Points.Add(TcvPoint.Create(i,Lx,Ly));
     Lx:=Lx+2; Ly:=Ly+0.6;
    end;
  }
  /// синусоида - для теста
   Lx:=-20; Ly:=0;
   for I :=0 to 300 do
    begin
     Larea.Curves.Last.Points.Add(TcvPoint.Create(i,Lx,Ly));
     Lx:=Lx+2; Ly:=120+50*Sin(Lx*0.04);
    end;

   Larea.Curves.Last.TitleType:=ttYValue;
   Larea.Curves.Last.MarkedEvery(20);
/// выводить только указанные по номерам надписи точек кривой
   Larea.Curves.Last.MarkedSeveral([5,10,15]);
/// начало прорисовки
  img1.Bitmap.Canvas.BeginScene;
  try
  //  LCv.DrawPointLines;
  //   LCv.DrawPtMarks;
   LCv.TitleType:=ttXYvalue;
   LCv.TitleHorzAlign:=TTextAlign.Center;

   LArea.RedrawAll;
   // после первой перерисовки скорректировать границы с учетом минимальных отступов
   LArea.ApplyAutoMargins(25,4);
   // пересчитать размеры области кривых графика с учетом отступов
   LArea.ResetAreaParams;
   // вывести произвольные надписи в местах отступов для названий осей
   LArea.SetAxisTextLabels('Привет, %','Среднеквадратическое отклонение от значения, ККал.');
   /// поместить кривую с номером 0 вперед при рисовании (порядок прорисовки)
   LArea.BringNumToFront(0);
   // снова перерисовать -
   // вторая перерисовка необходима, т.к. следует учитывать чтот надписи могут выходить за
   // пределы вывода графика - поэтому сначала выводим надписи, вычисляя их координаты на Canvas,
   // затем - если необходимо уменьшаем область собственно вывода кривых и перестраиваем их точки на Canvas
   LArea.RedrawAll;
   ///
  finally
    img1.Bitmap.Canvas.EndScene;
  end;
 /// тест
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
 // повторить цикл вывода с учетом изменения размера картинки
  LArea.PaintToBitmap(LBM,RectF(0,0,img1.Width,img1.Height),false);
  img1.Bitmap:=LBM;
end;

end.

