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
  row.npar =            5;// 17p3GeV_2021
  row.a[0] =     0.034183;
  row.a[1] =   4.4597e-05;
  row.a[2] =  -4.3133e-06;
  row.a[3] =   2.3854e-08;
  row.a[4] =   -4.827e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 17p3GeV_2021
  row.a[0] =     0.051685;
  row.a[1] =    0.0035564;
  row.a[2] =  -5.3521e-05;
  row.a[3] =   2.0482e-07;
  row.a[4] =  -2.1111e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
