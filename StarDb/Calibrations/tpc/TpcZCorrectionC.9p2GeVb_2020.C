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
  row.npar =            2;// 9p2GeVb_2020
  row.a[0] =     0.026315;
  row.a[1] =  -0.00014798;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVb_2020
  row.a[0] =    -0.092762;
  row.a[1] =    0.0070475;
  row.a[2] =  -9.6984e-05;
  row.a[3] =   4.8101e-07;
  row.a[4] =  -8.3218e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
