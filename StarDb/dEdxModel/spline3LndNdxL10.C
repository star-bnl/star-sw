TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_spline3")) return 0;
  Int_t nrows = 1;
  St_spline3 *tableSet = new St_spline3("spline3LndNdxL10",nrows);
  spline3_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.nknots = 14;
  Double_t X[14] = {-2.05,-1.5,-1,-0.5,0,0.25,0.5,0.75,1,1.5,2,3,4,5.15};
  Double_t Y[14] = {10.8534,9.17952,7.2945,5.35299,3.83234,3.49396,3.39275,3.41151,3.46255,3.59647,3.67184,3.69744,3.70087,3.70114};
  for (Int_t i = 0; i < 14; i++) {row.Xknots[i] = X[i]; row.Yknots[i] = Y[i];}
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
