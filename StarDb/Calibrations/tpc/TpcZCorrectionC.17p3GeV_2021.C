TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 17p3GeV_2021
  row.a[0] =    -0.065549;
  row.a[1] =     0.003502;
  row.a[2] =  -8.3782e-05;
  row.a[3] =     1.08e-06;
  row.a[4] =  -7.4239e-09;
  row.a[5] =   2.5624e-11;
  row.a[6] =  -3.5149e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 17p3GeV_2021
  row.a[0] =    -0.037927;
  row.a[1] =    0.0063568;
  row.a[2] =   -0.0001094;
  row.a[3] =   8.0899e-07;
  row.a[4] =  -2.8531e-09;
  row.a[5] =   3.9298e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
