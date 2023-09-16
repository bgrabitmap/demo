unit umain;

{
 Ported from https://codepen.io/enxaneta/pen/WQPdvV
 by Lainz
 v0.1
  - working demo
}


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BGRABitmapTypes, BCTypes, Math;

const
  nCusped = 3; // 1,2,3,4,5,6,7,...
  nR = 100;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure BGRAVirtualScreen1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
     cw, ch, cx, cy, rad, r, x, y, hx, hy, phi: Double;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  t: Double = 0;
begin
  if (phi >= 2 * PI) then
    phi := 0
  else
    phi += 0.01;

  Bitmap.Fill(clWhite);
  with Bitmap.Canvas2D do
  begin
    strokeStyle(clBlue);
    beginPath();
    arc(cx, cy, nR, 0, 2 * PI);
    stroke();

    strokeStyle(BGRA(51, 51, 51));
    beginPath();
    arc(cx + (nR - r) * cos(phi), cy + (nR - r) * sin(phi), 2, 0, 2 * PI);
    stroke();
    beginPath();
    arc(cx + (nR - r) * cos(phi), cy + (nR - r) * sin(phi), r, 0, r * PI);

    x := cx + (nR - r) * cos(phi) + r * cos((nR - r) / r * phi);
    y := cy + (nR - r) * sin(phi) - r * sin((nR - r) / r * phi);

    beginPath();
    moveTo(cx + (nR - r) * cos(phi), cy + (nR - r) * sin(phi));
    lineTo(x, y);
    stroke();
    beginPath();
    arc(x, y, 3, 0, 2 * PI);
    fill();

    beginPath();

    while (t <= phi) do
    begin
      hx := cx + (nR - r) * cos(t) + r * cos((nR - r) / r * t);
      hy := cy + (nR - r) * sin(t) - r * sin((nR - r) / r * t);
      lineTo(hx, hy);
      t+=0.01;
    end;

    save();
    strokeStyle(clRed);
    lineWidth := 3;
    stroke();
    restore();
  end;
end;

procedure TfrmMain.BGRAVirtualScreen1Resize(Sender: TObject);
begin
  cw := Width;
  cx := cw / 2;
  ch := Height;
  cy := ch / 2;
  rad := PI / 180;
  r := nR / nCusped;
  phi := 0;
end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  BGRAvirtualScreen1.DiscardBitmap;
end;

end.

