unit umain;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes;

type

  { TForm1 }

  TProp = record
    thecolor: byte;
    x, y: single;
  end;

  { TData }

  TData = record
    data: array of TProp;
    procedure Push(aData: TProp);
    function Pop: TProp;
  end;

  TForm1 = class(TForm)
    BGRAVirtualScreen1: TBGRAVirtualScreen;
    Timer1: TTimer;
    procedure BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    zoom: single;
    thecolor: Byte;
    x, y: single;
    myData: TData;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TData }

procedure TData.Push(aData: TProp);
begin
  SetLength(data, Length(data)+1);
  data[Length(data)-1] := aData;
end;

function TData.Pop: TProp;
begin
  Result := data[Length(data)-1];
  SetLength(data, Length(data)-1);
end;

{ TForm1 }

procedure TForm1.BGRAVirtualScreen1Redraw(Sender: TObject; Bitmap: TBGRABitmap);

procedure Translate(toX, toY: single);
begin
  x += toX;
  y += toY;
end;

procedure push();
var
  prop: TProp;
begin
  prop.thecolor:=thecolor;
  prop.x := x;
  prop.y := y;
  mydata.Push(prop);
end;

procedure pop();
var
  prop: TProp;
begin
  prop := mydata.Pop;
  x := prop.x;
  y := prop.y;
  thecolor := prop.thecolor;
end;

procedure Ellipse(ax, ay, aw, ah: single);
begin
  ax += x;
  ay += y;
  Bitmap.EllipseAntialias(ax, ay, aw / 2, ah / 2,BGRA(thecolor, thecolor, thecolor), 1, BGRA(thecolor, thecolor, thecolor));
end;

procedure Circles(w: single);
begin
  if (w > 15) then
  begin
    thecolor += 10;
    Ellipse(w / 2, 0, w, w);
    Circles(w / 2);
    push();
    Translate(w, 0);
    Ellipse(w / 2, 0, w, w);
    Circles(w / 2);
    pop();
  end;
end;

begin
  Bitmap.Fill(BGRABlack);
  thecolor := 0;
  x := 0;
  y := 0;
  Translate(0, Height / 2);
  circles(width * zoom);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  zoom := 1;
  thecolor := 0;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  zoom *= 1.015;
  if (zoom > 2) then
    zoom := 1;
    BGRAVirtualScreen1.DiscardBitmap;
end;

end.

