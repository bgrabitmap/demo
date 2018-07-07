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

  public

  end;

var
  frmSuperFormula: TfrmSuperFormula;
  MAX_LOG: Double = 0.0;
  MIN_LOG: Double = 0.0;
  MAX_R: Double = 100.0;

implementation

function SafePower(a, b: Double; out c: Double): Boolean;
var
  tmp: Double;
begin
  Result := true;
  if (a < 0) then
    raise Exception.Create('1st argument of Power() must not be negative');
  if (a = 0) then begin
    if (b = 0) then
      raise Exception.Create('Both arguments of Power() must not be zero.');
    c := 0;
    exit;
  end;

  if MAX_LOG = 0.0 then
    MAX_LOG := ln(MaxDouble);
  if MIN_LOG = 0.0 then
    MIN_LOG := ln(MinDouble);

  // ln(a^b) = b ln(a)
  tmp := b * ln(a);
  if tmp > MAX_LOG then
    Result := false
  else
  if tmp < MIN_LOG then
    c := 0.0
  else
    c := exp(tmp);
end;

function r(theta, a, b, m, n1, n2, n3: double): double;
const
  EPS = 1E-9;
var
  c, pc, s, ps: Double;
begin
  if (a = 0) then
    raise Exception.Create('a must not be zero.');
  if (b = 0) then
    raise Exception.Create('b must not be zero');
  if (m = 0) then
    raise Exception.Create('m must not be zero');
  if (n1 = 0) then
    raise Exception.Create('n1 must not be zero');
  if (n2 = 0) then
    raise Exception.Create('n2 must not be zero');
  if (n3 = 0) then
    raise Exception.Create('n3 must not be zero');

  c := abs(cos(m * theta / 4) / a);
  if c < EPS then
    pc := 0
  else
  if not SafePower(c, n2, pc) then begin
    Result := MAX_R;
    exit;
  end;

  s := abs(sin(m * theta / 4) / b);
  if s < EPS then
    ps := 0
  else
  if not SafePower(s, n3, ps) then begin
    Result := MAX_R;
    exit;
  end;

  if pc + ps < EPS then
    Result := 0
  else
  if not SafePower(pc + ps, -1/n1, Result) then
    Result := MAX_R;

  if Result > MAX_R then Result := MAX_R;
end;

{$R *.lfm}

{ TfrmSuperFormula }

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
