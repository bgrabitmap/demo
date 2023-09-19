unit umain;

{
 Ported from https://codepen.io/enxaneta/pen/WQPdvV
 by Lainz
 v0.1
  - working demo
 v0.2
  - resize circle
  - full resolution on MacOS
  - DPI size
  - refactor
  - updown to choose nb of cusps
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, BCTrackbarUpdown,
  Math;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BCTrackbarUpdownNCusped: TBCTrackbarUpdown;
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BCTrackbarUpdownNCuspedChange(Sender: TObject; AByUser: boolean);
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    toReset: boolean;
    nR, r, px, phi: Double;
    center: TPointF;
    procedure ResetShape;
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

{ TfrmMain }

procedure TfrmMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  t: Double = 0;
  ptCenter, pt: TPointF;
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
  end;

  with Bitmap.Canvas2D do
  begin
    lineWidth := 1*px;

    // bounding circle
    strokeStyle(clBlue);
    beginPath();
    circle(center.x, center.y, nR);
    stroke();

    // draw the cursor
    ptCenter := HypocycloidCenterAt(phi, nR, r) + center;
    pt := HypocycloidAt(phi, nR, r) + center;

    strokeStyle(BGRA(51, 51, 51));
    beginPath();
    circle(ptCenter.x, ptCenter.y, r);
    moveTo(ptCenter);
    lineTo(pt);
    stroke();

    beginPath();
    circle(pt.x, pt.y, 3*px);
    fill();

    // draw the curve
    beginPath();
    while (t <= phi) do
    begin
      pt := HypocycloidAt(t, nR, r) + center;
      lineTo(pt);
      t+=0.01;
    end;
    strokeStyle(clRed);
    save();
    lineWidth := 3*px;
    stroke();
    restore();
  end;

  // next position
  if (phi >= 2 * PI) then
    phi := 0
  else
    phi += 0.01;
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
  ResetShape;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  BGRAvirtualScreen1.DiscardBitmap;
end;

procedure TfrmMain.ResetShape;
begin
  toReset := true;
end;

end.

