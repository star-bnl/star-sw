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
  row.npar =            7;// FXT_2019
  row.a[0] =    -0.031349;
  row.a[1] =    0.0027647;
  row.a[2] =  -6.4442e-05;
  row.a[3] =   6.4911e-07;
  row.a[4] =  -3.1084e-09;
  row.a[5] =   6.0022e-12;
  row.a[6] =  -2.0606e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =    0.0088699;
  row.a[1] =    0.0093894;
  row.a[2] =   -0.0003195;
  row.a[3] =   4.4832e-06;
  row.a[4] =  -3.1282e-08;
  row.a[5] =    1.059e-10;
  row.a[6] =   -1.391e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =      -0.1403;
  row.a[1] =     0.006369;
  row.a[2] =  -0.00015754;
  row.a[3] =   2.0897e-06;
  row.a[4] =  -1.4685e-08;
  row.a[5] =   5.1956e-11;
  row.a[6] =   -7.296e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =     -0.14647;
  row.a[1] =     0.011928;
  row.a[2] =  -0.00028735;
  row.a[3] =    3.561e-06;
  row.a[4] =   -2.497e-08;
  row.a[5] =   9.1876e-11;
  row.a[6] =  -1.3585e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
