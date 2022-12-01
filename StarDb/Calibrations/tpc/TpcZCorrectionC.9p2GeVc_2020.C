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
  row.npar =            5;// 9p2GeVc_2020
  row.a[0] =     0.039934;
  row.a[1] =   6.5266e-05;
  row.a[2] =  -6.8107e-06;
  row.a[3] =   4.3838e-08;
  row.a[4] =  -9.0988e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVc_2020
  row.a[0] =     0.055253;
  row.a[1] =    0.0038934;
  row.a[2] =  -6.1243e-05;
  row.a[3] =   2.6095e-07;
  row.a[4] =  -3.3586e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
