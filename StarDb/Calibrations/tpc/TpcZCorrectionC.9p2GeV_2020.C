TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeV_2020
  row.a[0] =     0.031701;
  row.a[1] =  -0.00014533;
  row.a[2] =  -7.8557e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeV_2020
  row.a[0] =    -0.066132;
  row.a[1] =    0.0058274;
  row.a[2] =  -9.9491e-05;
  row.a[3] =   7.3481e-07;
  row.a[4] =  -2.7036e-09;
  row.a[5] =   3.9651e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeV_2020
  row.a[0] =     0.035196;
  row.a[1] =  -0.00017767;
  row.a[2] =  -7.8416e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeV_2020
  row.a[0] =    -0.092728;
  row.a[1] =    0.0073093;
  row.a[2] =  -0.00012053;
  row.a[3] =   8.5247e-07;
  row.a[4] =  -2.9777e-09;
  row.a[5] =   4.1397e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
