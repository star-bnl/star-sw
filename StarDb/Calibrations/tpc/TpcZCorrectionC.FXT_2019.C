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
  row.a[0] =    -0.028368;
  row.a[1] =    0.0018182;
  row.a[2] =  -4.1023e-05;
  row.a[3] =   4.1674e-07;
  row.a[4] =  -1.8829e-09;
  row.a[5] =   3.0109e-12;
  row.a[6] =   4.7991e-17;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =    -0.047839;
  row.a[1] =     0.010378;
  row.a[2] =  -0.00033617;
  row.a[3] =    4.878e-06;
  row.a[4] =  -3.6427e-08;
  row.a[5] =   1.3363e-10;
  row.a[6] =  -1.9064e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =    -0.058591;
  row.a[1] =    0.0039961;
  row.a[2] =  -9.0278e-05;
  row.a[3] =   1.1092e-06;
  row.a[4] =  -7.7483e-09;
  row.a[5] =   2.8275e-11;
  row.a[6] =  -4.1436e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2019
  row.a[0] =     -0.19088;
  row.a[1] =     0.017888;
  row.a[2] =  -0.00044277;
  row.a[3] =   5.6792e-06;
  row.a[4] =  -4.1078e-08;
  row.a[5] =   1.5442e-10;
  row.a[6] =  -2.3167e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
