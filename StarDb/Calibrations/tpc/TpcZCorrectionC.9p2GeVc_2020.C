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
  row.npar =            2;// 9p2GeVc_2020
  row.a[0] =     0.019808;
  row.a[1] =  -9.1359e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVc_2020
  row.a[0] =    -0.095314;
  row.a[1] =    0.0068826;
  row.a[2] =  -9.3683e-05;
  row.a[3] =   4.6247e-07;
  row.a[4] =   -7.979e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
