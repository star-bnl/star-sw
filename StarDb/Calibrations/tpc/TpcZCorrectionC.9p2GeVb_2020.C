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
  row.npar =            5;// 9p2GeVb_2020
  row.a[0] =     0.044211;
  row.a[1] =   1.9146e-05;
  row.a[2] =  -6.4991e-06;
  row.a[3] =   4.0465e-08;
  row.a[4] =   -8.133e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVb_2020
  row.a[0] =      0.05973;
  row.a[1] =    0.0035327;
  row.a[2] =  -5.5856e-05;
  row.a[3] =     2.28e-07;
  row.a[4] =  -2.6813e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
