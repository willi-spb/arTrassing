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
    Num,Sign,GroupId:integer;
    DataPoint,Point:TPointF;
    constructor Create(aNum:integer; aDtX,aDtY:single; aGroupId:integer=0);
    function GetAreaRect(aDim:single):TRectF;
    function GetPtAreaRect(aDim:single):TRectF;
    ///
    property X:single  index 1 read GetPt write SetPt;
    property Y:single  index 2 read GetPt write SetPt;
    property ptX:single  index 3 read GetPt write SetPt;
    property ptY:single  index 4 read GetPt write SetPt;
 End;

TOnPointDrawMarkEvent=procedure(aSndr:TObject; aCv:TCanvas; aIndex:integer; const acvPt:TcvPoint; aFillFlag:boolean) of object;
TOnPointDrawTitleEvent=procedure(aSndr:TObject; aCv:TCanvas; const acvPt:TcvPoint) of object;

TarCurve=Class(TObject)
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
   FScalePt:TPointF;
   FActiveArea:TRectF;
   FOnPointDrawMarkEvent:TOnPointDrawMarkEvent;
   FOnPointDrawTitleEvent:TOnPointDrawTitleEvent;
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
    function GetAreaRect:TrectF;
  ///
    procedure ScalePoints(aDataArea,aAreaCvRect:TRectF);
  ///
  property MarkFill:TBrush read FMarkFill;
  property MarkFont:TFont read FMarkFont;
  property MarkBrush:TStrokeBrush read FMarkBrush;
  property Brush:TStrokeBrush read FBrush;
  property MarkFilled:boolean read FmarkFilled write FmarkFilled;
  property OnPointDrawMarkEvent:TOnPointDrawMarkEvent read FOnPointDrawMarkEvent write FOnPointDrawMarkEvent;
  property OnPointDrawTitleEvent:TOnPointDrawTitleEvent read FOnPointDrawTitleEvent write FOnPointDrawTitleEvent;
End;

TarAxis=class(TObject)
  private
   FVerticalFlag:boolean;
   FEnabled:boolean;
   FMin,FMax,FInterval,FOrtoValue:single;
  public
    Labels:TObjectList<TcvPoint>;
    function RecalcLabels(apMin, apMax, aInterval,aOrtoValue:single):boolean;
    constructor Create(aVerticalFlag:boolean; apMin, apMax, aInterval, aOrtoValue:single);
    destructor Destroy; override;
  ///
   property Min:single read FMin;
   property Max:single read FMax;
   property Interval:single read FInterval;
   property OrtoValue:single read FOrtoValue;
   property VerticalFlag:boolean read FVerticalFlag;
   property Enabled:boolean read FEnabled;
end;

TarChartArea=class(TObject)
  private
    FMarginRect:TRectf;
    FDataArea,FCanvasArea:TRectF;
    FXYCoeffs:TPointF;
    FLabelBrush,FLineBrush,FGridBrush:TStrokeBrush;
    FLabelFont:TFont;
  public
    CV:TCanvas;
    AxisX,AxisY:TarAxis;
    function GetActiveArea:TRectF;
    procedure SetAreaParam(const aDataArea,aAreaCvRect:TRectF);
    function DrawAxisLabels(aVertcalFlag:boolean):boolean;
    function DrawGrid(aVertcalFlag:boolean):boolean;
    function DrawFrame:boolean;
    constructor Create(aLabelColor,aLineColor,aGridColor:TAlphaColor;
                       aGridDash:TStrokeDash=TStrokeDash.Solid; aLabelThickness:single=1);
    destructor Destroy; override;
    property MarginRect:TRectF read FMarginRect;
  ///
    property LabelBrush:TStrokeBrush read FLabelBrush;
    property LineBrush:TStrokeBrush read FLineBrush;
    property GridBrush:TStrokeBrush read FGridBrush;
    property LabelFont:TFont read FLabelFont;
end;



implementation

uses  FMX.Types, System.SysUtils, System.Math, System.Math.Vectors;

{ TcvPoint }

constructor TcvPoint.Create(aNum:integer; aDtX, aDtY: single; aGroupId:integer=0);
begin
 inherited Create;
 Num:=aNum;
 GroupId:=aGroupId;
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

{ TarCurve }

constructor TarCurve.Create(aNum: integer; aColor: TAlphaColor;
  aThickness: single; aDash:TStrokeDash; aMarkType: TmarkType; aMarkSize:single);
begin
  inherited Create;
  FOnPointDrawMarkEvent:=nil;
  FOnPointDrawTitleEvent:=nil;
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
  FMarkFill:=TBrush.Create(TBrushKind.None,aColor);
  FMarkFill.Kind:=TBrushKind.Solid;
  FMarkFont:=TFont.Create;
  FActiveArea:=RectF(0,0,100,100);
 // FMarkFont.Color:=
end;

destructor TarCurve.Destroy;
begin
  Points.Free;
  FBrush.Free;
  FMarkBrush.free;
  FMarkFill.Free;
  FMarkFont.Free;
  inherited;
end;


procedure TarCurve.DrawPointLines;
var LPt:TcvPoint;
    i:integer;
    L_A:TPolygon;
    LState:TCanvasSaveState;
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
     LState:=SaveState;
     try
      IntersectClipRect(FActiveArea);
      Stroke.Assign(FBrush);
      DrawPolygon(L_A,FOpacity);
     finally
      RestoreState(LState);
     end;
    end;
   SetLength(L_A,0);
end;

procedure TarCurve.DrawPtMark(aIndex: integer);
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
                    if Assigned(FOnPointDrawMarkEvent) then
                       FOnPointDrawMarkEvent(Self,CV,aIndex,Lpt,false);
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
                   if Assigned(FOnPointDrawMarkEvent) then
                       FOnPointDrawMarkEvent(Self,CV,aIndex,Lpt,true);
                 end;

     end;
     end;
end;

procedure TarCurve.DrawPtMarks;
var i:integer;
begin
  i:=0;
  while i<Points.Count do
    begin
      DrawPtMark(i);
      Inc(i);
    end;
end;

procedure TarCurve.DrawTitles;
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
             L_Title:='';
             if Assigned(FOnPointDrawTitleEvent) then
                       FOnPointDrawTitleEvent(Self,CV,Lpt);
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

function TarCurve.GetAreaRect: TrectF;
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

procedure TarCurve.ScalePoints(aDataArea,aAreaCvRect:TRectF);
var Lrect:Trectf;
    Lpt:TcvPoint;
begin
  LRect:=RectF(0,0,aDataArea.Right-aDataArea.Left,aDataArea.Bottom-aDataArea.Top);
  FScalePt:=PointF(0,0);
  if (Lrect.Width>0) and (LRect.Height>0) then
       FScalePt:=PointF(aAreaCvRect.Width/Lrect.Width,aAreaCvRect.Height/LRect.Height);
  for Lpt in Points do
    begin
      Lpt.Point:=PointF(aAreaCvRect.Left+(Lpt.DataPoint.X-aDataArea.Left)*FScalePt.X,
                        aAreaCvRect.Bottom-(Lpt.DataPoint.Y-aDataArea.Top)*FScalePt.Y);
      Lpt.Title:='';
    end;
  FActiveArea:=aAreaCvRect; // !
end;

{ TarAxis }

constructor TarAxis.Create(aVerticalFlag: boolean; apMin, apMax, aInterval, aOrtoValue:single);
begin
  inherited Create;
  FEnabled:=false;
  FInterval:=0.1;
  Fmin:=0;
  Fmax:=1;
  FVerticalFlag:=aVerticalFlag;
  Labels:=TObjectList<TcvPoint>.Create(true);
  RecalcLabels(apMin,apMax,aInterval,aOrtoValue);
end;


destructor TarAxis.Destroy;
begin
  Labels.Free;
  inherited;
end;

function TarAxis.RecalcLabels(apMin, apMax, aInterval,aOrtoValue:single):boolean;
var LMin,LMax,LV:single;
    i,LDiv:integer;
    Lpt:TcvPoint;
begin
  Result:=false;
  Labels.Clear;
  ///
  if (aInterval>0) and (Abs(apMax-apMin)/aInterval<=100) then
    begin
      LMin:=Floor(apMin/aInterval)*aInterval;
      LMax:=Ceil(apMax/aInterval)*aInterval;
      LDiv:=Round(Abs(LMax-LMin)/aInterval);
      FMin:=LMin;
      Fmax:=LMin+LDiv*aInterval;
      FInterval:=aInterval;
      FEnabled:=true;
    end
  else FEnabled:=false;
  Result:=FEnabled;
  if Result then
    begin
      LV:=FMin; i:=0;
      while LV<=FMax do
        begin
         if FVerticalFlag then
             Lpt:=TcvPoint.Create(i,aOrtoValue,LV,2)
         else
             Lpt:=TcvPoint.Create(i,LV,aOrtoValue,1);
         Labels.Add(Lpt);
         Inc(i);
         LV:=LV+FInterval;
        end;
    end;
end;

{ TarChartArea }

constructor TarChartArea.Create(aLabelColor,aLineColor,aGridColor:TAlphaColor;
                       aGridDash:TStrokeDash=TStrokeDash.Solid; aLabelThickness:single=1);
begin
  inherited Create;
  AxisX:=nil;
  AxisY:=nil;
  FLabelBrush:=TStrokeBrush.Create(TBrushKind.Solid,aLabelColor);
  FLineBrush:=TStrokeBrush.Create(TBrushKind.Solid,aLineColor);
  FGridBrush:=TStrokeBrush.Create(TBrushKind.Solid,aGridColor);
  FGridBrush.Thickness:=0.5;
  FGridBrush.Dash:=aGridDash;
  if FGridBrush.Dash=TStrokeDash.Dash then
     FGridBrush.SetCustomDash([3/FGridBrush.Thickness,5/FGridBrush.Thickness],0);
  FLabelFont:=TFont.Create;
end;

destructor TarChartArea.Destroy;
begin
  if Assigned(AxisX) then
     AxisX.Free;
  if Assigned(AxisY) then
     AxisY.Free;
  ///
  FLabelBrush.Free;
  FLineBrush.Free;
  FGridBrush.Free;
  FLabelFont.Free;
  ///
  inherited;
end;


function TarChartArea.DrawAxisLabels(aVertcalFlag: boolean): boolean;
var Lpt:TcvPoint;
begin
  Result:=false;
 if aVertcalFlag then
  begin
   if (Assigned(AxisY)) and (FXYCoeffs.Y<>0) and (AxisY.Enabled=true) and (AxisY.Labels.Count>0) then
       with CV do
       begin
        for Lpt in AxisY.Labels do
            DrawLine(PointF(Lpt.Point.X-6,Lpt.Point.Y),Lpt.Point,1,FLabelBrush);
        DrawLine(AxisY.Labels.First.Point,AxisY.Labels.Last.Point,1,FLineBrush);
        Result:=true;
       end;
  end
 else
   if (Assigned(AxisX)) and (FXYCoeffs.X<>0) and (AxisX.Enabled=true) and (AxisX.Labels.Count>0) then
      with CV do
       begin
         for Lpt in AxisX.Labels do
            DrawLine(Lpt.Point,PointF(Lpt.Point.X,Lpt.Point.Y+6),1,FLabelBrush);
        DrawLine(AxisX.Labels.First.Point,AxisX.Labels.Last.Point,1,FLineBrush);
        Result:=true;
      end;
end;

function TarChartArea.DrawFrame: boolean;
var LRect:TRectF;
begin
  Result:=false;
  LRect:=GetActiveArea;
  CV.DrawLine(PointF(Lrect.Left,Lrect.Top),PointF(Lrect.Right,Lrect.Top),1,FLineBrush);
  CV.DrawLine(PointF(Lrect.Right,Lrect.Top),PointF(Lrect.Right,Lrect.Bottom),1,FLineBrush);
  Result:=true;
end;

function TarChartArea.DrawGrid(aVertcalFlag: boolean): boolean;
var LState:TCanvasSaveState;
    Lpt:TcvPoint;
    LRect:TRectF;
begin
 Result:=false;
 LRect:=GetActiveArea;
 LState:=CV.SaveState;
 try
  Cv.IntersectClipRect(LRect);
  if aVertcalFlag then
    begin
     if (Assigned(AxisY)) and (FXYCoeffs.Y<>0) and (AxisY.Enabled=true) and (AxisY.Labels.Count>0) then
      with CV do
       begin
        for Lpt in AxisY.Labels do
         if (Lpt<>AxisY.Labels.First) and (Lpt<>AxisY.Labels.Last) then
            DrawLine(Lpt.Point,PointF(Lrect.Right,Lpt.Point.Y),1,FGridBrush);
        Result:=true;
      end;
    end
  else
   if (Assigned(AxisX)) and (FXYCoeffs.X<>0) and (AxisX.Enabled=true) and (AxisX.Labels.Count>0) then
      with CV do
       begin
         for Lpt in AxisX.Labels do
          if (Lpt<>AxisX.Labels.First) and (Lpt<>AxisX.Labels.Last) then
             DrawLine(PointF(Lpt.Point.X,Lrect.Top),Lpt.Point,1,FGridBrush);
        Result:=true;
      end;
 finally
   CV.RestoreState(LState);
 end;
end;

function TarChartArea.GetActiveArea: TRectF;
begin
 Result:=Rectf(FCanvasArea.Left+FMarginRect.Left,
                      FCanvasArea.Top+FMarginRect.Top,
                      FCanvasArea.Right-FMarginRect.Right,
                      FCanvasArea.Bottom-FMarginRect.Bottom);
end;


procedure  TarChartArea.SetAreaParam(const aDataArea,aAreaCvRect:TRectF);
var Lpt:TcvPoint;
    LptX,LPtY:single;
    LCoeff:TPointF;
begin
  FMarginRect:=RectF(40,10,10,20);
  FCanvasArea:=aAreaCvRect;
  FDataArea:=aDataArea;
  FXYCoeffs:=PointF(0,0);
  if (FDataArea.Width>0) and (FCanvasArea.Width-FMarginRect.Left-FMarginRect.Right>1) then
     FXYCoeffs.X:=(FCanvasArea.Width-FMarginRect.Left-FMarginRect.Right)/FDataArea.Width;
  if (FDataArea.Height>0) and (FCanvasArea.Height-FMarginRect.Top-FMarginRect.Bottom>1) then
     FXYCoeffs.Y:=(FCanvasArea.Height-FMarginRect.Top-FMarginRect.Bottom)/FDataArea.Height;
  ///
  if Assigned(AxisX)=false then
     AxisX:=TarAxis.Create(false,FdataArea.left,FdataArea.Right,Abs(FDataArea.Width)/10,aDataArea.Bottom)
  else
    AxisX.RecalcLabels(FdataArea.left,FdataArea.Right,Abs(FDataArea.Width)/10,aDataArea.Bottom);
  if Assigned(AxisY)=false then
     AxisY:=TarAxis.Create(true,FdataArea.Top,FdataArea.Bottom,Abs(FDataArea.Height)/10,aDataArea.Left)
  else
    AxisY.RecalcLabels(FdataArea.Top,FdataArea.Bottom,Abs(FDataArea.Height)/10,aDataArea.Left);
  ///
   LCoeff:=PointF(AxisX.Interval*FXYCoeffs.X,AxisY.Interval*FXYCoeffs.Y);
   LptX:=FCanvasArea.Left+FMarginRect.Left;
   LptY:=FCanvasArea.Bottom-FMarginRect.Bottom;
   if (AxisX.Enabled) and (FXYCoeffs.X<>0) then
    for Lpt in AxisX.Labels do
        begin
          Lpt.Point:=PointF(LptX,LptY);
          LptX:=Lptx+LCoeff.X;
        end;
    LptX:=FCanvasArea.Left+FMarginRect.Left;
    LptY:=FCanvasArea.Bottom-FMarginRect.Bottom;
   if (AxisY.Enabled) and (FXYCoeffs.Y<>0) then
    for Lpt in AxisY.Labels do
        begin
          Lpt.Point:=PointF(LptX,LptY);
          LptY:=LptY-LCoeff.Y;
        end;
end;

end.

