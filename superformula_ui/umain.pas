unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Spin, BGRAVirtualScreen, BGRABitmap, BCTypes, BGRABitmapTypes, Math, Types,
  BGRALayerOriginal, BGRAStreamLayers, BGRALayers, BGRASuperformulaOriginal;

type

  { TfrmSuperFormula }

  TfrmSuperFormula = class(TForm)
    cbLineColor: TColorButton;
    cbFillColor: TColorButton;
    cbMRational: TCheckBox;
    cbSpikeOverlap: TCheckBox;
    ColorDialog1: TColorDialog;
    edValueA: TFloatSpinEdit;
    edValueB: TFloatSpinEdit;
    edValueM: TFloatSpinEdit;
    edValueN1: TFloatSpinEdit;
    edValueN2: TFloatSpinEdit;
    edValueN3: TFloatSpinEdit;
    edMultiplier: TFloatSpinEdit;
    edLineWidth: TFloatSpinEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblFillColor: TLabel;
    lblLineColor: TLabel;
    lblLineWidth: TLabel;
    lblMultiplier: TLabel;
    lblValueN3: TLabel;
    lblValueN2: TLabel;
    lblValueN1: TLabel;
    lblValueM: TLabel;
    lblValueB: TLabel;
    lblValueA: TLabel;
    pnlControls: TPanel;
    vsPreview: TBGRAVirtualScreen;
    procedure cbFillColorColorChanged(Sender: TObject);
    procedure cbMRationalChange(Sender: TObject);
    procedure cbLineColorColorChanged(Sender: TObject);
    procedure cbSpikeOverlapChange(Sender: TObject);
    procedure edLineWidthChange(Sender: TObject);
    procedure edMultiplierChange(Sender: TObject);
    procedure edValueAChange(Sender: TObject);
    procedure edValueBChange(Sender: TObject);
    procedure edValueMChange(Sender: TObject);
    procedure edValueN1Change(Sender: TObject);
    procedure edValueN2Change(Sender: TObject);
    procedure edValueN3Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure vsPreviewClick(Sender: TObject);
    procedure vsPreviewMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure vsPreviewMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure FormCreate(Sender: TObject);
  private
    FLayers: TBGRALayeredBitmap;
    FSuperformula: TBGRALayerSuperformulaOriginal;
    procedure OriginalChange(ASender: TObject; AOriginal: TBGRALayerCustomOriginal; var ADiff: TBGRAOriginalDiff);
    procedure UpdateFraction;
  end;

var
  frmSuperFormula: TfrmSuperFormula;

implementation

uses BGRATransform;

{$R *.lfm}

{ TfrmSuperFormula }

procedure TfrmSuperFormula.vsPreviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  zoom: single;
  matrix: TAffineMatrix;
begin
  zoom := DesignTimePPI / 96 * GetCanvasScaleFactor;
  matrix := AffineMatrixTranslation(Bitmap.Width/2, Bitmap.Height/2) *
      AffineMatrixScale(zoom, zoom);
  if not Assigned(FLayers) then
  begin
    FLayers := TBGRALayeredBitmap.Create(Bitmap.Width, Bitmap.Height);
    Flayers.OnOriginalChange := @OriginalChange;
    FLayers.AddLayerFromOwnedOriginal(FSuperformula, matrix);
    FLayers.RenderOriginalsIfNecessary;
  end else
  if (Bitmap.Width <> FLayers.Width) or (Bitmap.Height <> FLayers.Height) then
  begin
    FLayers.SetSize(Bitmap.Width, Bitmap.Height);
    FLayers.LayerOriginalMatrix[0] := matrix;
    FLayers.RenderLayerFromOriginal(0);
  end else
  begin
    FLayers.LayerOriginalMatrix[0] := matrix;
    FLayers.RenderOriginalsIfNecessary;
  end;

  FLayers.Draw(Bitmap, 0, 0);
end;

procedure TfrmSuperFormula.edValueAChange(Sender: TObject);
begin
  FSuperformula.a := edValueA.Value;
end;

procedure TfrmSuperFormula.edMultiplierChange(Sender: TObject);
begin
  FSuperformula.Multiplier := edMultiplier.Value;
end;

procedure TfrmSuperFormula.edLineWidthChange(Sender: TObject);
begin
  FSuperformula.LineWidth := edLineWidth.Value;
end;

procedure TfrmSuperFormula.cbLineColorColorChanged(Sender: TObject);
begin
  FSuperformula.LineColor := cbLineColor.ButtonColor;
end;

procedure TfrmSuperFormula.cbSpikeOverlapChange(Sender: TObject);
begin
  FSuperformula.SpikeOverlap:= cbSpikeOverlap.Checked;
end;

procedure TfrmSuperFormula.cbFillColorColorChanged(Sender: TObject);
begin
  FSuperformula.FillColor := cbFillColor.ButtonColor;
end;

procedure TfrmSuperFormula.cbMRationalChange(Sender: TObject);
begin
  FSuperformula.mRational:= cbMRational.Checked;
end;

procedure TfrmSuperFormula.edValueBChange(Sender: TObject);
begin
  FSuperformula.b := edValueB.Value;
end;

procedure TfrmSuperFormula.edValueMChange(Sender: TObject);
begin
  FSuperformula.m := edValueM.Value;
end;

procedure TfrmSuperFormula.edValueN1Change(Sender: TObject);
begin
  FSuperformula.n1 := edValueN1.Value;
end;

procedure TfrmSuperFormula.edValueN2Change(Sender: TObject);
begin
  FSuperformula.n2 := edValueN2.Value;
end;

procedure TfrmSuperFormula.edValueN3Change(Sender: TObject);
begin
  FSuperformula.n3 := edValueN3.Value;
end;

procedure TfrmSuperFormula.FormDestroy(Sender: TObject);
begin
  FLayers.Free;
end;

procedure TfrmSuperFormula.vsPreviewClick(Sender: TObject);
begin
  ColorDialog1.Color := vsPreview.Color;
  if ColorDialog1.Execute then
    vsPreview.Color := ColorDialog1.Color;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin
  edMultiplier.Value := edMultiplier.Value - 25;
end;

procedure TfrmSuperFormula.vsPreviewMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
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

  FSuperformula := TBGRALayerSuperformulaOriginal.Create;

  if FileExists(Application.Location + 'superformula.data') then
    FSuperformula.LoadFromFile(Application.Location + 'superformula.data');

  cbLineColor.ButtonColor := FSuperformula.LineColor;
  cbFillColor.ButtonColor := FSuperformula.FillColor;
  cbSpikeOverlap.Checked := FSuperformula.SpikeOverlap;
  edValueA.Value := FSuperformula.a;
  edValueB.Value := FSuperformula.b;
  edValueM.Value := FSuperformula.m;
  UpdateFraction;
  cbMRational.Checked:= FSuperformula.mRational;
  edValueN1.Value := FSuperformula.n1;
  edValueN2.Value := FSuperformula.n2;
  edValueN3.Value := FSuperformula.n3;
  edMultiplier.Value := FSuperformula.Multiplier;
  edLineWidth.Value := FSuperformula.LineWidth;
end;

procedure TfrmSuperFormula.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FSuperformula.SaveToFile(Application.Location + 'superformula.data');
end;

procedure TfrmSuperFormula.OriginalChange(ASender: TObject;
  AOriginal: TBGRALayerCustomOriginal; var ADiff: TBGRAOriginalDiff);
begin
  vsPreview.DiscardBitmap;
  UpdateFraction;
end;

procedure TfrmSuperFormula.UpdateFraction;
var
  Num, Denom: integer;
begin
  FSuperformula.GetMFraction(Num, Denom);
  cbMRational.Caption := IntToStr(Num) + '/' + IntToStr(Denom);
end;

end.
