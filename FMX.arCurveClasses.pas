unit FMX.arCurveClasses;

interface

uses System.Classes, System.Generics.Collections, System.Types, System.UITypes, FMX.Graphics;

type

 TmarkType=(mpNone,mpCircle,mpUpTriangle,mpTriangle,mpRect,mpRhomb,mpCross,mpDiagCross,mpArrow,mpCustom);
 TTitleType=(ttNone,ttXValue,ttYValue,ttXYvalue,ttTitle,ttDesc,ttCustom);

 TcvPoint=Class(TObject)
   private
    function GetPt(Index:integer):single;
    procedure SetPt(Index:integer; Value:single);
   public
    Title,Desc:string;
    Num,Sign:integer;
    DataPoint,Point:TPointF;
    constructor Create(aDtX,aDtY:single);
    function GetAreaRect(aDim:single):TRectF;
    function GetPtAreaRect(aDim:single):TRectF;
    ///
    property X:single  index 1 read GetPt write SetPt;
    property Y:single  index 2 read GetPt write SetPt;
    property ptX:single  index 3 read GetPt write SetPt;
    property ptY:single  index 4 read GetPt write SetPt;
 End;

TcvCurve=Class(TObject)
  private
   FMarkType:TmarkType;
   FMarkSize:single;
   FmarkVisibled,FmarkFilled:boolean;
   FTitleType:TTitleType;
   FNum:integer;
   FBrush,FMarkBrush:TStrokeBrush;
   FMarkFill:TBrush;
   FMarkFont:TFont;
   FOpacity:single;
   FShiftPt,FScalePt:TPointF;
  public
    Points:TObjectList<TcvPoint>;
    Cv:TCanvas;
    constructor Create(aNum:integer; aColor:TAlphaColor; aThickness:single=1;
                       aDash:TStrokeDash=TStrokeDash.Solid; aMarkType:TmarkType=TmarkType.mpNone; aMarkSize:single=4);
    destructor Destroy; override;
  ///
    procedure DrawPtMark(aIndex:integer);
    procedure DrawPtMarks;
    procedure DrawPointLines;
    procedure DrawTitles;
  ///
  ///
    function GetAreaRect:TrectF;
  ///
    procedure ScalePoints(const aBM:TBitmap; aAreaRect:TRectF);
  ///
  property MarkFill:TBrush read FMarkFill;
  property MarkFont:TFont read FMarkFont;
  property MarkBrush:TStrokeBrush read FMarkBrush;
  property Brush:TStrokeBrush read FBrush;
  property MarkFilled:boolean read FmarkFilled write FmarkFilled;
End;





implementation

uses  FMX.Types, System.SysUtils, System.Math.Vectors;

{ TcvPoint }

constructor TcvPoint.Create(aDtX, aDtY: single);
begin
 inherited Create;
 DataPoint:=PointF(aDtX,aDtY);
end;

function TcvPoint.GetPtAreaRect(aDim: single): TRectF;
var LDim:single;
begin
  LDim:=Abs(0.5*ADim);
  if LDim<0.1 then LDim:=0.1;
  Result:=RectF(Point.X-LDim,Point.Y-LDim,Point.X+LDim,Point.Y+LDim);
end;

function TcvPoint.GetAreaRect(aDim: single): TRectF;
var LDim:single;
begin
  LDim:=Abs(ADim);
  Result:=RectF(DataPoint.X-LDim,DataPoint.Y-LDim,DataPoint.X+LDim,DataPoint.Y+LDim);
end;


function TcvPoint.GetPt(Index: integer): single;
begin
 case Index of
  1: Result:=DataPoint.X;
  2: Result:=DataPoint.Y;
  3: Result:=Point.X;
  4: Result:=Point.Y;
  else Result:=0;
 end;
end;

procedure TcvPoint.SetPt(Index: integer; Value: single);
begin
  case Index of
  1: DataPoint.X:=Value;
  2: DataPoint.Y:=Value;
  3: Point.X:=Value;
  4: Point.Y:=Value;
 end;
end;

{ TcvCurve }

constructor TcvCurve.Create(aNum: integer; aColor: TAlphaColor;
  aThickness: single; aDash:TStrokeDash; aMarkType: TmarkType; aMarkSize:single);
begin
  inherited Create;
  FBrush:=TStrokeBrush.Create(TBrushKind.Solid,aColor);
  FBrush.Kind:=TBrushKind.Solid;
  FBrush.Thickness:=aThickness;
  FBrush.Dash:=aDash;
  Points:=TObjectList<TcvPoint>.Create(true);
  FMarkType:=aMarkType;
  FNum:=aNum;
  FOpacity:=1;
  FMarkSize:=Abs(aMarkSize);
  FMarkBrush:=TStrokeBrush.Create(TBrushKind.Solid,aColor);
  FMarkBrush.Kind:=TBrushKind.Solid;
  FMarkFill:=TBrush.Create(TBrushKind.None,aColor);
  FMarkFill.Kind:=TBrushKind.Solid;
  FMarkFont:=TFont.Create;
 // FMarkFont.Color:=
end;

destructor TcvCurve.Destroy;
begin
  Points.Free;
  FBrush.Free;
  FMarkBrush.free;
  FMarkFill.Free;
  FMarkFont.Free;
  inherited;
end;

procedure TcvCurve.DrawPointLines;
var LPt:TcvPoint;
    i:integer;
    L_A:TPolygon;
begin
  if Points.Count=0 then exit;
  SetLength(L_A,Points.Count);
  i:=0;
  for Lpt in Points do
    begin
     L_A[i]:=Lpt.Point;
     Inc(i);
    end;
   ///
   with CV do
     begin
      Stroke.Assign(FBrush);
      DrawPolygon(L_A,FOpacity);
     end;
   SetLength(L_A,0);
end;

procedure TcvCurve.DrawPtMark(aIndex: integer);
var Lpt:TcvPoint;
    Lpg:TPolygon;
    L_Rad,L_cr:single;
    L_Corn:TCorners;
begin
  if FMarkType<>TmarkType.mpNone then
    with CV do
     begin
      Lpt:=Points.Items[aIndex];
      if Assigned(Lpt)=false then exit;
      L_Rad:=0.5*FMarkSize;
      L_cr:=0.1*FMarkSize;
      Stroke.Assign(FMarkBrush);
      Fill.Assign(FMarkFill);
      Font.Assign(FMarkFont);
      L_Corn:=[TCorner.TopLeft,TCorner.TopRight, TCorner.BottomLeft, TCorner.BottomRight];
      ///
      if FmarkFilled=false then
        case FMarkType of
          mpCircle: DrawEllipse(Lpt.GetPtAreaRect(FMarkSize),FOpacity,FMarkBrush);
          mpUpTriangle:
           DrawPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_Rad),PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_Rad),PointF(Lpt.ptX,Lpt.ptY+L_Rad)],FOpacity);
          mpTriangle:
           DrawPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_Rad),PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_Rad),PointF(Lpt.ptX,Lpt.ptY-L_Rad)],FOpacity);
          mpRect: DrawRect(Lpt.GetPtAreaRect(FMarkSize),1,1,L_Corn, FOpacity,FMarkBrush,TCornerType.Round);
          mpRhomb:
           DrawPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY),
                        PointF(Lpt.ptX,Lpt.ptY-L_Rad),
                        PointF(Lpt.ptX+L_Rad,Lpt.ptY),
                        PointF(Lpt.ptX,Lpt.ptY+L_Rad)],FOpacity);

      mpCross: DrawPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY-L_Rad),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY-L_Rad),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY+L_Rad),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY+L_Rad),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY+L_cr)],FOpacity);
      mpDiagCross: DrawPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_Rad+L_cr),
                                PointF(Lpt.ptX-L_Rad+L_cr,Lpt.ptY-L_Rad),
                                PointF(Lpt.ptX,Lpt.ptY-L_cr),
                                PointF(Lpt.ptX+L_Rad-L_cr,Lpt.ptY-L_Rad),
                                PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_Rad+L_cr),
                                PointF(Lpt.ptX+L_cr,Lpt.ptY),
                                PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_Rad-L_cr),
                                PointF(Lpt.ptX+L_Rad-L_cr,Lpt.ptY+L_Rad),
                                PointF(Lpt.ptX,Lpt.ptY+L_cr),
                                PointF(Lpt.ptX-L_Rad+L_cr,Lpt.ptY+L_Rad),
                                PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_Rad-L_cr),
                                PointF(Lpt.ptX-L_cr,Lpt.ptY)],FOpacity);
      mpArrow:    DrawPolygon([PointF(Lpt.ptX-L_cr,Lpt.ptY-2*L_Rad),
                               PointF(Lpt.ptX+L_cr,Lpt.ptY-2*L_Rad),
                               PointF(Lpt.ptX+L_cr,Lpt.ptY-1.5*L_rad+L_cr),
                               PointF(Lpt.ptX+L_Rad,Lpt.ptY-1.5*L_Rad),
                               PointF(Lpt.ptX,Lpt.ptY),
                               PointF(Lpt.ptX-L_Rad,Lpt.ptY-1.5*L_Rad),
                               PointF(Lpt.ptX-L_cr,Lpt.ptY-1.5*L_rad+L_cr)],FOpacity);
      mpCustom:  begin

                 end;
      end { case }
     else
       case FMarkType of
          mpCircle: FillEllipse(Lpt.GetPtAreaRect(FMarkSize),FOpacity,FMarkBrush);
          mpUpTriangle:
           FillPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_Rad),PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_Rad),PointF(Lpt.ptX,Lpt.ptY+L_Rad)],FOpacity);
          mpTriangle:
           FillPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_Rad),PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_Rad),PointF(Lpt.ptX,Lpt.ptY-L_Rad)],FOpacity);
          mpRect: DrawRect(Lpt.GetPtAreaRect(FMarkSize),1,1,L_Corn, FOpacity,FMarkBrush,TCornerType.Round);
          mpRhomb:
           FillPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY),
                        PointF(Lpt.ptX,Lpt.ptY-L_Rad),
                        PointF(Lpt.ptX+L_Rad,Lpt.ptY),
                        PointF(Lpt.ptX,Lpt.ptY+L_Rad)],FOpacity);

      mpCross: FillPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY-L_Rad),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY-L_Rad),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_cr),
                              PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY+L_cr),
                              PointF(Lpt.ptX+L_cr,Lpt.ptY+L_Rad),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY+L_Rad),
                              PointF(Lpt.ptX-L_cr,Lpt.ptY+L_cr)],FOpacity);
      mpDiagCross: FillPolygon([PointF(Lpt.ptX-L_Rad,Lpt.ptY-L_Rad+L_cr),
                                PointF(Lpt.ptX-L_Rad+L_cr,Lpt.ptY-L_Rad),
                                PointF(Lpt.ptX,Lpt.ptY-L_cr),
                                PointF(Lpt.ptX+L_Rad-L_cr,Lpt.ptY-L_Rad),
                                PointF(Lpt.ptX+L_Rad,Lpt.ptY-L_Rad+L_cr),
                                PointF(Lpt.ptX+L_cr,Lpt.ptY),
                                PointF(Lpt.ptX+L_Rad,Lpt.ptY+L_Rad-L_cr),
                                PointF(Lpt.ptX+L_Rad-L_cr,Lpt.ptY+L_Rad),
                                PointF(Lpt.ptX,Lpt.ptY+L_cr),
                                PointF(Lpt.ptX-L_Rad+L_cr,Lpt.ptY+L_Rad),
                                PointF(Lpt.ptX-L_Rad,Lpt.ptY+L_Rad-L_cr),
                                PointF(Lpt.ptX-L_cr,Lpt.ptY)],FOpacity);
      mpArrow:    FillPolygon([PointF(Lpt.ptX-L_cr,Lpt.ptY-2*L_Rad),
                               PointF(Lpt.ptX+L_cr,Lpt.ptY-2*L_Rad),
                               PointF(Lpt.ptX+L_cr,Lpt.ptY-1.5*L_rad+L_cr),
                               PointF(Lpt.ptX+L_Rad,Lpt.ptY-1.5*L_Rad),
                               PointF(Lpt.ptX,Lpt.ptY),
                               PointF(Lpt.ptX-L_Rad,Lpt.ptY-1.5*L_Rad),
                               PointF(Lpt.ptX-L_cr,Lpt.ptY-1.5*L_rad+L_cr)],FOpacity);
      mpCustom:  begin

                 end;

     end;
     end;
end;

procedure TcvCurve.DrawPtMarks;
var i:integer;
begin
  i:=0;
  while i<Points.Count do
    begin
      DrawPtMark(i);
      Inc(i);
    end;
end;

procedure TcvCurve.DrawTitles;
var LPt:TcvPoint;
    i:integer;
    L_Title:String;
    Lx,Ly,L_Rad,LW:single;
    L_Rect:TRectf;
begin
  if (Points.Count=0) or (FTitleType=ttNone) then exit;
  with Cv do
   begin
    Font.Assign(FMarkFont);
    L_Rad:=0.5*FMarkSize;
    for Lpt in Points do
     begin
      case FTitleType of
        ttXValue: L_Title:=FloatToStr(Lpt.X);
        ttYValue: L_Title:=FloatToStr(Lpt.Y);
        ttXYvalue:  L_Title:=Format('X=%f Y=%f',[Lpt.X,Lpt.Y]);
        ttTitle: L_Title:=Lpt.Title;
        ttDesc: L_Title:=Lpt.Desc;
        ttCustom:
           begin

           end;
      end;
    if L_Title<>'' then
     begin
       Lx:=LPt.ptX;
       Ly:=LPt.ptY-TextHeight(L_Title)-L_Rad;
       LW:=TextWidth(L_Title);
       L_Rect:=RectF(Lx-L_rad-0.5*LW,Ly,Lx+L_rad+0.5*LW,LPt.ptY);
       Cv.FillText(L_Rect,L_Title,False,FOpacity,[],TTextAlign.Center,TTextAlign.Center);
     end;
    end;
   end;
end;

function TcvCurve.GetAreaRect: TrectF;
var LxMin,LXMax,LYMin,Lymax:single;
    Lpt:TcvPoint;
begin
  LxMax:=-1e38; LxMin:=1e38;
  LyMax:=-1e38; LyMin:=1e38;
  for Lpt in Points do
     begin
       if Lpt.X>LXMax then
          LXMax:=Lpt.X;
       if Lpt.X<LXMin then
          LXMin:=Lpt.X;
       if Lpt.Y>LYMax then
          LYMax:=Lpt.Y;
       if Lpt.Y<LYMin then
          LYMin:=Lpt.Y;
     end;
  Result:=RectF(LxMin,LYMin,LXMax,Lymax);
end;

procedure TcvCurve.ScalePoints(const aBM: TBitmap; aAreaRect:TRectF);
var Lrect,LAreaRect:Trectf;
    Lpt:TcvPoint;
begin
  LAreaRect:=aAreaRect;// GetAreaRect;
  FShiftPt:=LAreaRect.TopLeft;
  LRect:=RectF(0,0,LAreaRect.Right-LAreaRect.Left,LAreaRect.Bottom-LAreaRect.Top);
  FScalePt:=PointF(aBM.Width/Lrect.Width,aBM.Height/LRect.Height);
  for Lpt in Points do
    begin
      Lpt.Point:=PointF((Lpt.DataPoint.X-FShiftPt.X)*FScalePt.X,(Lpt.DataPoint.Y-FShiftPt.Y)*FScalePt.Y);
      Lpt.Title:='';
    end;
  Cv:=aBm.Canvas;
end;

end.
