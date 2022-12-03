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
  row.npar =            5;// 9p2GeVabc_2020
  row.a[0] =     0.043683;
  row.a[1] =  -1.1268e-06;
  row.a[2] =   -6.505e-06;
  row.a[3] =   4.2906e-08;
  row.a[4] =  -8.9214e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVabc_2020
  row.a[0] =     0.060486;
  row.a[1] =     0.003716;
  row.a[2] =  -5.8667e-05;
  row.a[3] =   2.4576e-07;
  row.a[4] =  -3.0439e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
