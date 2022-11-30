TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrection",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize()); // /hlt/cephfs/fisyak/TpcRS_2021.COL.NoPad/Fit2 $ root.exe Eta3G4EYpion.root
  row.idx   = 1;                         //   FitP->Draw("mu-muJ:y*y>>o","i&&j&&mu>-0.4&&abs(x)>40.5","prof")
  row.nrows = nrows;
  row.min =   0.0;
  row.max =   1.6;
  row.npar =            4;//  o->Fit("pol3","e")
  row.a[0] =  2.97324e-02;
  row.a[1] = -1.43924e-01;
  row.a[2] =  9.77592e-02;
  row.a[3] = -2.53291e-02;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;                         // FitP->Draw("mu-muJ:y*y>>i","i&&j&&mu>-0.4&&abs(x)<40.5","prof")
  row.nrows = nrows;
  row.min =   0.0;
  row.max =   1.6;
  row.npar =            4;// i->Fit("pol3","e")
  row.a[0] =  7.83824e-02;
  row.a[1] = -3.14868e-01;
  row.a[2] =  2.52456e-01;
  row.a[3] = -6.88254e-02;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
