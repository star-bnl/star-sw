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
  row.a[0] =    -0.018487;
  row.a[1] =    0.0023515;
  row.a[2] =  -6.4445e-05;
  row.a[3] =   7.6323e-07;
  row.a[4] =  -4.1474e-09;
  row.a[5] =   9.4311e-12;
  row.a[6] =  -6.2446e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =    -0.072356;
  row.a[1] =     0.015774;
  row.a[2] =  -0.00052205;
  row.a[3] =    7.889e-06;
  row.a[4] =  -6.1048e-08;
  row.a[5] =   2.3054e-10;
  row.a[6] =  -3.3674e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =    -0.028247;
  row.a[1] =    0.0033697;
  row.a[2] =  -7.5603e-05;
  row.a[3] =   8.8589e-07;
  row.a[4] =   -5.969e-09;
  row.a[5] =   2.1057e-11;
  row.a[6] =  -2.9858e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2020
  row.a[0] =     -0.11613;
  row.a[1] =      0.01729;
  row.a[2] =   -0.0004652;
  row.a[3] =   6.3421e-06;
  row.a[4] =  -4.7843e-08;
  row.a[5] =   1.8435e-10;
  row.a[6] =  -2.8063e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
