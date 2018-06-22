unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Math, Types;

type

  { TfrmSuperFormula }

  TfrmSuperFormula = class(TForm)
    cbLineColor: TColorButton;
    cbFillColor: TColorButton;
    edValueA: TFloatSpinEdit;
    edValueB: TFloatSpinEdit;
    edValueM: TFloatSpinEdit;
    edValueN1: TFloatSpinEdit;
    edValueN2: TFloatSpinEdit;
    edValueN3: TFloatSpinEdit;
    edResolution: TFloatSpinEdit;
    edMultiplier: TFloatSpinEdit;
    edLineWidth: TFloatSpinEdit;
    lblFillColor: TLabel;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    lblMultiplier: TLabel;
    lblResolution: TLabel;
    lblValueN3: TLabel;
    lblValueN2: TLabel;
    lblValueN1: TLabel;
    lblValueM: TLabel;
    lblValueB: TLabel;
    lblValueA: TLabel;
    pnlControls: TPanel;
    vsPreview: TBGRAVirtualScreen;
    procedure edValueAChange(Sender: TObject);
    procedure vsPreviewClick(Sender: TObject);
    procedure vsPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure vsPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    function r(theta, a, b, m, n1, n2, n3: double): double;
  public

  end;

var
  frmSuperFormula: TfrmSuperFormula;

implementation

{$R *.lfm}

{ TfrmSuperFormula }

function TfrmSuperFormula.r(theta, a, b, m, n1, n2, n3: double): double;
begin
  { Preventing Exceptions }
  if a <= 0 then
    a := MinDouble;
  if b <= 0 then
    b := MinDouble;
  if m <= 0 then
    m := MinDouble;
  if n1 <= 0 then
    n1 := MinDouble;
  if n2 <= 0 then
    n2 := MinDouble;
  if n3 <= 0 then
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

procedure TfrmSuperFormula.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  theta: double;
  rad: double;
  x: double = 0;
  y: double = 0;
begin
  Bitmap.DrawCheckers(Rect(0, 0, Bitmap.Width, Bitmap.Height), BGRA(230, 230, 230), clWhite);
  try
    theta := edResolution.Value;
    Bitmap.Canvas2D.resetTransform;
    Bitmap.Canvas2D.translate(vsPreview.Width div 2, vsPreview.Height div 2);
    Bitmap.Canvas2D.lineWidth := edLineWidth.Value;
    Bitmap.Canvas2D.strokeStyle(cbLineColor.ButtonColor);
    Bitmap.Canvas2D.fillStyle(cbFillColor.ButtonColor);
    Bitmap.Canvas2D.beginPath;
    while (theta <= 2 * pi) do
    begin
      rad := r(theta, edValueA.Value,
        edValueB.Value,
        edValueM.Value,
        edValueN1.Value,
        edValueN2.Value,
        edValueN3.Value
        );
      x := rad * cos(theta) * edMultiplier.Value;
      y := rad * sin(theta) * edMultiplier.Value;
      Bitmap.Canvas2D.lineTo(x, y);
      theta += edResolution.Value;
    end;
    Bitmap.Canvas2D.closePath;
    Bitmap.Canvas2D.fill;
    if edLineWidth.Value > 0 then
       Bitmap.Canvas2D.stroke;
  except
    on e: Exception do ;
  end;
end;

procedure TfrmSuperFormula.edValueAChange(Sender: TObject);
begin
  vsPreview.DiscardBitmap;
end;

procedure TfrmSuperFormula.vsPreviewClick(Sender: TObject);
begin
  cbFillColor.Click;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  edMultiplier.Value := edMultiplier.Value - 25;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  edMultiplier.Value := edMultiplier.Value + 25;
end;

procedure TfrmSuperFormula.FormCreate(Sender: TObject);
begin
  {$ifdef Windows}
  DoubleBuffered := True;
  {$endif}
  cbLineColor.Constraints.MinHeight := edValueA.Height;
  cbFillColor.Constraints.MinHeight := edValueA.Height;
end;

end.
