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
  row.npar =            7;// FXT_2020
  row.a[0] =    -0.011984;
  row.a[1] =    0.0020988;
  row.a[2] =  -5.5464e-05;
  row.a[3] =   4.7174e-07;
  row.a[4] =  -7.0672e-10;
  row.a[5] =  -7.6895e-12;
  row.a[6] =   2.4399e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =      0.04291;
  row.a[1] =    0.0070666;
  row.a[2] =  -0.00024935;
  row.a[3] =   3.5398e-06;
  row.a[4] =  -2.5101e-08;
  row.a[5] =   8.6404e-11;
  row.a[6] =  -1.1559e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =     -0.19777;
  row.a[1] =     0.010905;
  row.a[2] =  -0.00027878;
  row.a[3] =    3.613e-06;
  row.a[4] =  -2.4551e-08;
  row.a[5] =   8.3723e-11;
  row.a[6] =  -1.1329e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =    -0.078807;
  row.a[1] =    0.0090892;
  row.a[2] =  -0.00023663;
  row.a[3] =   3.2076e-06;
  row.a[4] =  -2.4679e-08;
  row.a[5] =   9.8104e-11;
  row.a[6] =  -1.5396e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
