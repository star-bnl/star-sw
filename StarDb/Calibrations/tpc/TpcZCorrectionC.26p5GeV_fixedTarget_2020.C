TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.049905;
  row.a[1] =    0.0033929;
  row.a[2] =  -9.4148e-05;
  row.a[3] =   1.2791e-06;
  row.a[4] =  -8.5619e-09;
  row.a[5] =   2.7275e-11;
  row.a[6] =  -3.3699e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.017251;
  row.a[1] =     0.009604;
  row.a[2] =  -0.00031601;
  row.a[3] =   4.9702e-06;
  row.a[4] =   -4.075e-08;
  row.a[5] =   1.6174e-10;
  row.a[6] =  -2.4568e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.07218;
  row.a[1] =    0.0052483;
  row.a[2] =  -0.00012724;
  row.a[3] =   1.6498e-06;
  row.a[4] =  -1.1802e-08;
  row.a[5] =   4.3029e-11;
  row.a[6] =  -6.2214e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.10428;
  row.a[1] =     0.016112;
  row.a[2] =  -0.00044291;
  row.a[3] =    6.248e-06;
  row.a[4] =  -4.8628e-08;
  row.a[5] =   1.9179e-10;
  row.a[6] =  -2.9681e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
