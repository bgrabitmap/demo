unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, BGRAVirtualScreen, BGRABitmap, BCTypes,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  sky: TBGRABitmap;
  clouds: TBGRABitmap;
begin
  // draw background (z order 0)
  sky := TBGRABitmap.Create(0,0);
  sky.LoadFromResource('SKY');
  BGRAReplace(sky, sky.Resample(BGRAVirtualScreen1.Width, BGRAVirtualScreen1.Height, rmSimpleStretch));
  Bitmap.PutImage(0,0,sky, dmSet);
  sky.Free;
  // draw elements (z order 1)
  Bitmap.EllipseAntialias(100, 100, 10, 10, BGRA(255, 255, 0), 1, BGRA(255, 255, 0));
  // draw another bitmap on top of everything (z order 2)
  clouds := TBGRABitmap.Create(0, 0);
  clouds.LoadFromResource('CLOUDS');
  BGRAReplace(clouds, clouds.Resample(BGRAVirtualScreen1.Width, BGRAVirtualScreen1.Height, rmSimpleStretch));
  Bitmap.PutImage(0, 0, clouds, dmDrawWithTransparency, 200);
  clouds.Free;
end;

end.

