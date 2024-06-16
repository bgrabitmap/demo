unit BGRASuperformulaOriginal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BGRALayerOriginal, BGRABitmap, BGRABitmapTypes, Math;

const
    MaxDenominator = 20;

type
  { TBGRALayerSuperformulaOriginal }

  TBGRALayerSuperformulaOriginal = class(TBGRALayerCustomOriginal)
  private
    FSpikeOverlap: boolean;
    Fa: double;
    Fb: double;
    FFillColor: TBGRAPixel;
    FLineColor: TBGRAPixel;
    FLineWidth: double;
    Fm: double;
    FMRational: boolean;
    FMultiplier: double;
    Fn1: double;
    Fn2: double;
    Fn3: double;
    function FloatToFraction(ARatio: single; out num, denom: integer; AMaxDenominator: integer): string;
    function GetRadius: double;
    procedure SetA(AValue: double);
    procedure SetB(AValue: double);
    procedure SetFillColor(AValue: TBGRAPixel);
    procedure SetLineColor(AValue: TBGRAPixel);
    procedure SetLineWidth(AValue: double);
    procedure SetM(AValue: double);
    procedure SetMultiplier(AValue: double);
    procedure SetN1(AValue: double);
    procedure SetN2(AValue: double);
    procedure SetN3(AValue: double);
    procedure SetMRational(AValue: boolean);
    procedure SetSpikeOverlap(AValue: boolean);
  public
    constructor Create; override;
    procedure Render(ADest: TBGRABitmap; AMatrix: TAffineMatrix;
      ADraft: boolean); override;
    function GetRenderBounds(ADestRect: TRect; {%H-}AMatrix: TAffineMatrix): TRect; override;
    procedure GetMFraction(out ANumerator, ADenominator: integer);
    procedure LoadFromStorage(AStorage: TBGRACustomOriginalStorage); override;
    procedure SaveToStorage(AStorage: TBGRACustomOriginalStorage); override;
    class function StorageClassName: RawByteString; override;
    property SpikeOverlap: boolean read FSpikeOverlap write SetSpikeOverlap;
    property a: double read Fa write SetA;
    property b: double read Fb write SetB;
    property m: double read Fm write SetM;
    property mRational: boolean read FMRational write SetMRational;
    property n1: double read Fn1 write SetN1;
    property n2: double read Fn2 write SetN2;
    property n3: double read Fn3 write SetN3;
    property Radius: double read GetRadius;
    property LineWidth: double read FLineWidth write SetLineWidth;
    property LineColor: TBGRAPixel read FLineColor write SetLineColor;
    property FillColor: TBGRAPixel read FFillColor write SetFillColor;
    property Multiplier: double read FMultiplier write SetMultiplier;
  end;

implementation

var
  MAX_LOG: double = 0.0;
  MIN_LOG: double = 0.0;
  MAX_R: double = 100.0;

function SafePower(a, b: double; out c: double): boolean;
var
  tmp: double;
begin
  Result := True;
  if (a < 0) then
    raise Exception.Create('1st argument of Power() must not be negative');
  if (a = 0) then
  begin
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
    Result := False
  else
  if tmp < MIN_LOG then
    c := 0.0
  else
    c := exp(tmp);
end;

function ComputeR(theta, a, b, m, n1, n2, n3: double): double;
const
  EPS = 1E-9;
var
  c, pc, s, ps: double;
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
  if not SafePower(c, n2, pc) then
  begin
    Result := MAX_R;
    exit;
  end;

  s := abs(sin(m * theta / 4) / b);
  if s < EPS then
    ps := 0
  else
  if not SafePower(s, n3, ps) then
  begin
    Result := MAX_R;
    exit;
  end;

  if pc + ps < EPS then
    Result := 0
  else
  if not SafePower(pc + ps, -1 / n1, Result) then
    Result := MAX_R;

  if Result > MAX_R then
    Result := MAX_R;
end;

{ TBGRALayerSuperformulaOriginal }

procedure TBGRALayerSuperformulaOriginal.SetA(AValue: double);
begin
  if Fa = AValue then
    Exit;
  Fa := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetB(AValue: double);
begin
  if Fb = AValue then
    Exit;
  Fb := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetFillColor(AValue: TBGRAPixel);
begin
  if FFillColor = AValue then
    Exit;
  FFillColor := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetLineColor(AValue: TBGRAPixel);
begin
  if FLineColor = AValue then
    Exit;
  FLineColor := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetLineWidth(AValue: double);
begin
  if FLineWidth = AValue then
    Exit;
  FLineWidth := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetM(AValue: double);
begin
  if Fm = AValue then
    Exit;
  Fm := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetMultiplier(AValue: double);
begin
  if FMultiplier = AValue then
    Exit;
  FMultiplier := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetMRational(AValue: boolean);
begin
  if FMRational=AValue then Exit;
  FMRational:=AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetN1(AValue: double);
begin
  if Fn1 = AValue then
    Exit;
  Fn1 := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetN2(AValue: double);
begin
  if Fn2 = AValue then
    Exit;
  Fn2 := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetN3(AValue: double);
begin
  if Fn3 = AValue then
    Exit;
  Fn3 := AValue;
  NotifyChange;
end;

procedure TBGRALayerSuperformulaOriginal.SetSpikeOverlap(AValue: boolean);
begin
  if FSpikeOverlap=AValue then Exit;
  FSpikeOverlap:=AValue;
  NotifyChange;
end;

constructor TBGRALayerSuperformulaOriginal.Create;
begin
  inherited Create;
  FSpikeOverlap:= true;
  Fa := 1;
  Fb := 1;
  Fm := 24;
  Fn1 := 2;
  Fn2 := 1;
  Fn3 := 2;
  FLineWidth := 0;
  FLineColor := $00804000;
  FFillColor := $00C08000;
  FMultiplier := 200;
  FMRational:= true;
end;

function TBGRALayerSuperformulaOriginal.FloatToFraction(ARatio: single; out num,
  denom: integer; AMaxDenominator: integer): string;

  procedure InvFrac;
  var temp: integer;
  begin
    temp := num;
    num := denom;
    denom := temp;
  end;

  procedure AddFrac(AValue: integer);
  begin
    inc(num, AValue*denom);
  end;

const MaxDev = 6;
var
  dev: array[1..MaxDev] of integer;
  devCount, i: integer;
  curVal, remain: Single;

begin
  if ARatio < 0 then ARatio := -ARatio;
  curVal := ARatio;
  devCount := 0;
  repeat
    inc(devCount);
    dev[devCount] := trunc(curVal);
    remain := frac(curVal);
    if abs(remain) < 1e-3 then break;
    if devCount = MaxDev then
    begin
      if remain > 0.5 then inc(dev[devCount]);
      break;
    end;
    curVal := 1/remain;
  until false;
  repeat
    num := dev[devCount];
    denom := 1;
    for i := devCount-1 downto 1 do
    begin
      InvFrac;
      AddFrac(dev[i]);
    end;
    if ((num >= denom) and (denom <= AMaxDenominator))
       or ((num < denom) and (num <= AMaxDenominator))
       or (devCount = 1) then break;
    dec(devCount);
  until false;
  result := IntToStr(num)+':'+IntToStr(denom);
end;

function TBGRALayerSuperformulaOriginal.GetRadius: double;
begin
  result := 1;
end;

procedure TBGRALayerSuperformulaOriginal.Render(ADest: TBGRABitmap;
  AMatrix: TAffineMatrix; ADraft: boolean);
var
  i, num, denom, precision, turns: integer;
  r, theta, usedM, approxM, correction: double;
begin
  try
    ADest.Canvas2D.save;
    ADest.Canvas2D.antialiasing := not ADraft;
    ADest.Canvas2D.transform(AMatrix);
    ADest.Canvas2D.lineWidth := FLineWidth;
    ADest.Canvas2D.strokeStyle(FLineColor);
    ADest.Canvas2D.fillStyle(FFillColor);
    ADest.Canvas2D.beginPath;
    FloatToFraction(m, num, denom, MaxDenominator);
    approxM := num/denom;
    precision := max(num * 50, 90);
    if precision > 3000 then
      precision := (3000 div num)*num;
    if mRational then
    begin
      usedM := approxM;
      correction := 1;
    end else
    begin
      usedM:= m;
      correction := approxM / m;
    end;
    turns := denom * (1 + integer(SpikeOverlap and odd(num) and ((a <> b) or (n2 <> n3))));
    for i := 0 to precision * turns do
    begin
      theta := i * 2 * Pi * correction / precision;
      r := ComputeR(theta, a, b, usedM, n1, n2, n3) * multiplier;
      ADest.Canvas2D.lineTo(r * cos(theta), r * sin(theta));
    end;
    ADest.Canvas2D.closePath;
    ADest.Canvas2D.fill;
    if FLineWidth > 0 then
      ADest.Canvas2D.stroke;
    ADest.Canvas2D.restore;
  except
    on e: Exception do ;
  end;
end;

function TBGRALayerSuperformulaOriginal.GetRenderBounds(ADestRect: TRect;
  AMatrix: TAffineMatrix): TRect;
begin
  result := ADestRect;
end;

procedure TBGRALayerSuperformulaOriginal.GetMFraction(out ANumerator,
  ADenominator: integer);
begin
  FloatToFraction(m, ANumerator, ADenominator, MaxDenominator);
end;

procedure TBGRALayerSuperformulaOriginal.LoadFromStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  colorArray: ArrayOfTBGRAPixel;
begin
  FSpikeOverlap:= AStorage.BoolDef['spike-overlap', false];
  Fa := AStorage.Float['a'];
  Fb := AStorage.Float['b'];
  Fm := AStorage.Float['m'];
  FMRational:= AStorage.BoolDef['m-rational', true];
  Fn1 := AStorage.Float['n1'];
  Fn2 := AStorage.Float['n2'];
  Fn3 := AStorage.Float['n3'];
  FMultiplier := AStorage.Float['multiplier'];
  FLineWidth := AStorage.Float['linewidth'];

  colorArray := AStorage.ColorArray['colors'];
  FLineColor := colorArray[0];
  FFillColor := colorArray[1];
end;

procedure TBGRALayerSuperformulaOriginal.SaveToStorage(
  AStorage: TBGRACustomOriginalStorage);
var
  colorArray: ArrayOfTBGRAPixel;
begin
  AStorage.Bool['spike-overlap'] := FSpikeOverlap;
  AStorage.Float['a'] := Fa;
  AStorage.Float['b'] := Fb;
  AStorage.Float['m'] := Fm;
  AStorage.Bool['m-rational'] := FMRational;
  AStorage.Float['n1'] := Fn1;
  AStorage.Float['n2'] := Fn2;
  AStorage.Float['n3'] := Fn3;
  AStorage.Float['multiplier'] := FMultiplier;
  AStorage.Float['linewidth'] := FLineWidth;

  setlength(colorArray, 2);
  colorArray[0] := FLineColor;
  colorArray[1] := FFillColor;
  AStorage.ColorArray['colors'] := colorArray;
end;

class function TBGRALayerSuperformulaOriginal.StorageClassName: RawByteString;
begin
  Result := 'superformula';
end;

initialization
  RegisterLayerOriginal(TBGRALayerSuperformulaOriginal);

end.
