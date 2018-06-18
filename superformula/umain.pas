unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Math;

type

  { TfrmSuperFormula }

  TfrmSuperFormula = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    t: double;
    procedure OnIdle(Sender: TObject; var Done: boolean);
    function r(theta, a, b, m, n1, n2, n3: double): double;
    function mouseX: integer;
    function mouseY: integer;
  public

  end;

var
  frmSuperFormula: TfrmSuperFormula;

implementation

{$R *.lfm}

{ TfrmSuperFormula }

procedure TfrmSuperFormula.Timer1Timer(Sender: TObject);
begin
  t += 0.1;
end;

function TfrmSuperFormula.r(theta, a, b, m, n1, n2, n3: double): double;
begin
  { Preventing Exceptions }
  if a = 0 then
    a := MinDouble;
  if b = 0 then
    b := MinDouble;
  if m = 0 then
    m := MinDouble;
  if n1 = 0 then
    n1 := MinDouble;
  if n2 = 0 then
    n2 := MinDouble;
  if n3 = 0 then
    n3 := MinDouble;
  try
    { Actual Formula }
    Result := power(power(abs(cos(m * theta / 4) / a), n2) +
      power(abs(sin(m * theta / 4) / b), n3), -1 / n1)
  except
    on e: Exception do
      Result := 1;
  end;
end;

procedure TfrmSuperFormula.OnIdle(Sender: TObject; var Done: boolean);
begin
  Done := False;
  BGRAVirtualScreen1.DiscardBitmap;
end;

function TfrmSuperFormula.mouseX: integer;
begin
  { If you want to control the formula with mouse positions }
  Result := ScreenToClient(Mouse.CursorPos).x;
end;

function TfrmSuperFormula.mouseY: integer;
begin
  { If you want to control the formula with mouse positions }
  Result := ScreenToClient(Mouse.CursorPos).y;
end;

procedure TfrmSuperFormula.BGRAVirtualScreen1Redraw(Sender: TObject;
  Bitmap: TBGRABitmap);
var
  theta: double;
  rad: double;
  x: double = 0;
  y: double = 0;
begin
  theta := 0.01; // prevent starting line
  Bitmap.Canvas2D.resetTransform;
  Bitmap.Canvas2D.translate(Width div 2, Height div 2);
  Bitmap.Canvas2D.lineWidth := 1;
  Bitmap.Canvas2D.strokeStyle(BGRABlack);
  Bitmap.Canvas2D.beginPath;
  while (theta <= 2 * pi) do
  begin
    {
      If you want to control the formula with mouse positions replace with
      mouseX / 100
      mouseY / 100
    }
    rad := r(theta, 1, // a - size and control spikes
      1, // b - size and control spikes
      6, // m - number of spikes
      0.5, // n1 - roundness
      sin(t) * 0.5 + 0.5, // n2 - shape
      cos(t) * 0.5 + 0.5 // n3 - shape
      );
    x := rad * cos(theta) * (Width div 4);
    y := rad * sin(theta) * (Height div 4);
    Bitmap.Canvas2D.lineTo(x, y);
    theta += 0.01; // resolution of the drawing
  end;
  Bitmap.Canvas2D.closePath; // prevent holes
  Bitmap.Canvas2D.stroke;
end;

procedure TfrmSuperFormula.FormCreate(Sender: TObject);
begin
  t := 0;
  Application.OnIdle := @OnIdle;
  {$ifdef Windows}
  DoubleBuffered := True;
  {$endif}
end;

end.
