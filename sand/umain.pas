unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  BGRAVirtualScreen, BGRABitmap, BCTypes, Math, BGRABitmapTypes, LCLType;

const
  SPEED = 1;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    BGRAVirtualScreenMain: TBGRAVirtualScreen;
    TimerMain: TTimer;
    procedure BGRAVirtualScreenMainMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure BGRAVirtualScreenMainMouseMove(Sender: TObject;
      Shift: TShiftState; X, Y: integer);
    procedure BGRAVirtualScreenMainMouseUp(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure BGRAVirtualScreenMainRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
  private
    sand: TBGRABitmap;
    size: integer;
    md: boolean;
    m: TPoint;
    hsla: double;
    h: integer;
    w: integer;
    gx: integer;
    gy: integer;
    scaleX: double;
    scaleY: double;
    procedure putSand(const x: integer; const y: integer);
    function isWhite(const x: integer; const y: integer): boolean;
    procedure setWhite; inline;
    procedure setSandColor(const x: integer; const y: integer); inline;
    function canBottom: boolean;
    function canBottomLeft: boolean;
    function canBottomRight: boolean;
    procedure pathBottom;
    procedure pathBottomLeft;
    procedure pathBottomRight;
  public

  end;

var
  frmMain: TfrmMain;

implementation

{$R *.lfm}

{ TfrmMain }

procedure TfrmMain.BGRAVirtualScreenMainRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  y: integer = 0;
  x: integer = 0;
begin
  if (md) then
    for y := m.y - size to m.y + size do
      for x := m.x - size to m.x + size do
        if (y > 0) and (y < h) and (x > 0) and (x < w) then
          if (randomRange(0, 2) = 1) then
            putSand(x, y);
  for y := h - 1 downto 0 do
    for x := 0 to w - 1 do
    begin
      gx := x;
      gy := y;
      if (not isWhite(gx, gy)) then
        if (gy + speed < h) then
          if (canBottomLeft() and canBottomRight()) then
          begin
            if (randomRange(0, 2) = 1) then
              pathBottomRight()
            else
              pathBottomLeft();
          end
          else if (canBottomLeft()) then
            pathBottomLeft()
          else if (canBottomRight()) then
            pathBottomRight()
          else if (canBottom()) then
            pathBottom();
    end;
  bitmap.StretchPutImage(Rect(0, 0, bitmap.Width, bitmap.Height), sand, dmSet);
end;

procedure TfrmMain.BGRAVirtualScreenMainMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin
  m := Point(round(X * scaleX), round(Y * scaleY));
end;

procedure TfrmMain.BGRAVirtualScreenMainMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  md := False;
end;

procedure TfrmMain.BGRAVirtualScreenMainMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  md := True;
  m := Point(round(X * scaleX), round(Y * scaleY));
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  sand := TBGRABitmap.Create(Screen.Width div 2, Screen.Height div 2, clWhite);
  size := round(Screen.Width / 48);
  h := sand.Height;
  w := sand.Width;
  md := False;
  m := Point(0, 0);
  hsla := randomRange(0, MaxWord);
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  sand.Free;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
    Close;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  scaleX := w / BGRAVirtualScreenMain.Width;
  scaleY := h / BGRAVirtualScreenMain.Height;
end;

procedure TfrmMain.TimerMainTimer(Sender: TObject);
begin
  BGRAVirtualScreenMain.DiscardBitmap;
end;

procedure TfrmMain.putSand(const x: integer; const y: integer);
var
  p: THSLAPixel;
begin
  if isWhite(x, y) then
  begin
    p.hue := round(hsla);
    p.saturation := randomRange(32768, 65535 - (32768 div 2));
    p.lightness := 32768;
    p.alpha := 65535;
    sand.SetPixel(x, y, p);
    hsla := hsla + 1 / size;
    if (hsla > 65535) then
      hsla := 0;
  end;
end;

function TfrmMain.isWhite(const x: integer; const y: integer): boolean;
begin
  Result := sand.GetPixel(x, y) = BGRAWhite;
end;

procedure TfrmMain.setWhite;
begin
  sand.SetPixel(gx, gy, clWhite);
end;

procedure TfrmMain.setSandColor(const x: integer; const y: integer);
begin
  sand.SetPixel(x, y, sand.GetPixel(gx, gy));
end;

function TfrmMain.canBottom: boolean;
begin
  Result := isWhite(gx, gy + speed);
end;

function TfrmMain.canBottomLeft: boolean;
begin
  Result := isWhite(gx - speed, gy + speed);
end;

function TfrmMain.canBottomRight: boolean;
begin
  Result := isWhite(gx + speed, gy + speed);
end;

procedure TfrmMain.pathBottom;
begin
  setSandColor(gx, gy + speed);
  setWhite;
end;

procedure TfrmMain.pathBottomLeft;
begin
  setSandColor(gx - speed, gy + speed);
  setWhite;
end;

procedure TfrmMain.pathBottomRight;
begin
  setSandColor(gx + speed, gy + speed);
  setWhite;
end;

end.
