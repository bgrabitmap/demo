unit umain;

{
 Ported from https://codepen.io/enxaneta/pen/WQPdvV
 v0.1 by Lainz
  - working demo
 v0.2 by Circular
  - resize circle
  - full resolution on MacOS
  - DPI size
  - refactor
  - updown to choose nb of cusps
 v0.3 by Circular
  - render only moving parts
  - stop when loop complete
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Spin,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, BCTrackbarUpdown,
  Math, BGRAClasses;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCTrackbarUpdownNCusped: TFloatSpinEdit;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BCTrackbarUpdownNCuspedChange(Sender: TObject; AByUser: boolean);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    toReset: boolean;
    nR, r, px, phi: Double;
    center: TPointF;
    buffer: TBGRABitmap;
    cursorBox: TRectF;
    procedure ResetShape;
    function UpdateShape(Bitmap: TBGRABitmap): TRectF;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

function HypocycloidCenterAt(phi, nR, r: Double): TPointF;
begin
  result.x := (nR - r) * cos(phi);
  result.y := (nR - r) * sin(phi);
end;

function HypocycloidAt(phi, nR, r: Double): TPointF;
begin
  result.x := (nR - r) * cos(phi) + r * cos((nR - r) / r * phi);
  result.y := (nR - r) * sin(phi) - r * sin((nR - r) / r * phi);
end;

function BoundingBox(ARectF: TRectF; Margin: Integer = 1): TRect;
begin
  if ARectF.IsEmpty then
    result := EmptyRect
  else
  with ARectF do
    result := BGRAClasses.Rect(floor(Left) - Margin, floor(Top) - Margin,
                               ceil(Right) + Margin, ceil(Bottom) + Margin);
end;

{ TfrmMain }

function TfrmMain.UpdateShape(Bitmap: TBGRABitmap): TRectF;
const phiStep = 0.01;
var
  ptCenter, pt, ptBefore: TPointF;
  r1, r2: Double;
  lineBox, cursorBoxBefore: TRectF;
  phiMod: Double;
begin
  // restore buffer under cursor
  cursorBoxBefore := cursorBox;
  if Assigned(buffer) and not cursorBox.IsEmpty then
  begin
    Bitmap.ClipRect := BoundingBox(cursorBox);
    Bitmap.PutImage(0, 0, buffer, dmSet);
    Bitmap.NoClip;
    cursorBox := EmptyRectF;
  end;

  if phi >= 2*Pi-phiStep/2 then
  begin
    // if loop is complete, then stops
    if VectLen(HypocycloidAt(phi, nR, r) - HypocycloidAt(0, nR, r)) < 2*px then
       exit(cursorBoxBefore);
  end;

  with Bitmap.Canvas2D do
  begin
    lineWidth := 1*px;

    // draw the curve
    ptBefore := HypocycloidAt(phi, nR, r) + center;
    phi += phiStep;
    pt := HypocycloidAt(phi, nR, r) + center;
    beginPath();
    moveTo(ptBefore);
    lineTo(pt);
    strokeStyle(clRed);
    save();
    lineWidth := 3*px;
    stroke();
    lineBox := RectF(ptBefore.x - lineWidth/2, ptBefore.y - lineWidth/2,
                     ptBefore.x + lineWidth/2, ptBefore.y + lineWidth/2)
               .Union(RectF(pt.x - lineWidth/2, pt.y - lineWidth/2,
                            pt.x + lineWidth/2, pt.y + lineWidth/2));
    restore();

    // save buffer without cursor
    if Assigned(buffer) and not lineBox.IsEmpty then
    begin
      buffer.ClipRect := BoundingBox(lineBox);
      buffer.PutImage(0, 0, Bitmap, dmSet);
      buffer.NoClip;
    end;

    // draw the cursor
    ptCenter := HypocycloidCenterAt(phi, nR, r) + center;
    strokeStyle(BGRA(51, 51, 51));
    beginPath();
    r1 := r + 0.5*px; // taking pen width into account
    circle(ptCenter.x, ptCenter.y, r);
    moveTo(ptCenter);
    lineTo(pt);
    stroke();

    beginPath();
    r2 := 3*px;
    circle(pt.x, pt.y, r2);
    fill();

    cursorBox := RectF(ptCenter.x - r1, ptCenter.y - r1, ptCenter.x + r1, ptCenter.y + r1)
                 .Union(RectF(pt.x - r2, pt.y - r2, pt.x + r2, pt.y + r2));
  end;

  result := lineBox.Union(cursorBoxBefore).Union(cursorBox);
end;


procedure TfrmMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if toReset then
  begin
    // compute according to bitmap size at the last moment,
    // because component size may not be bitmap size on MacOS
    center := PointF(Bitmap.Width / 2,
                     Bitmap.Height / 2);
    nR := Math.Min(Bitmap.Width, Bitmap.Height) * 0.4;
    r := nR / BCTrackbarUpdownNCusped.Value;

    // determine CSS pixel size
    px := Scale96ToForm(1) * GetCanvasScaleFactor;

    phi := 0;
    toReset := false;
    FreeAndNil(buffer);
    buffer := TBGRABitmap.Create(Bitmap);
    with buffer.Canvas2D do
    begin
      lineWidth := 1*px;

      // bounding circle
      strokeStyle(clBlue);
      beginPath();
      circle(center.x, center.y, nR);
      stroke();
    end;
    cursorBox := EmptyRectF;
  end;

  if Assigned(buffer) then
    Bitmap.PutImage(0, 0, buffer, dmSet);
  UpdateShape(Bitmap);
end;

procedure TfrmMain.BCTrackbarUpdownNCuspedChange(Sender: TObject;
  AByUser: boolean);
begin
  if AByUser then ResetShape;
end;

procedure TfrmMain.BGRAVirtualScreen1Resize(Sender: TObject);
begin
  ResetShape;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  buffer := nil;
  cursorBox := EmptyRectF;
  ResetShape;
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(buffer);
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
var
  modifyBox: TRectF;
begin
  if Assigned(BGRAVirtualScreen1.Bitmap) and not toReset then
  begin
    modifyBox := UpdateShape(BGRAVirtualScreen1.Bitmap);
    if not IsEmptyRectF(modifyBox) then
       BGRAVirtualScreen1.InvalidateBitmap(BoundingBox(modifyBox));
  end;
end;

procedure TfrmMain.ResetShape;
begin
  toReset := true;
  BGRAVirtualScreen1.DiscardBitmap;
end;

end.

