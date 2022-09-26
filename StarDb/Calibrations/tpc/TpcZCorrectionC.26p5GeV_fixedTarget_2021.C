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
  row.npar =            3;// 26p5GeV_fixedTarget_2021
  row.a[0] =    0.0012855;
  row.a[1] =   0.00044869;
  row.a[2] =  -3.2035e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2021
  row.a[0] =    -0.027833;
  row.a[1] =    0.0081217;
  row.a[2] =  -0.00028402;
  row.a[3] =   4.3285e-06;
  row.a[4] =  -3.3238e-08;
  row.a[5] =   1.2391e-10;
  row.a[6] =  -1.7871e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2021
  row.a[0] =      0.37192;
  row.a[1] =     0.005615;
  row.a[2] =  -0.00029605;
  row.a[3] =   4.3469e-06;
  row.a[4] =  -3.2774e-08;
  row.a[5] =   1.2581e-10;
  row.a[6] =  -1.9145e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2021
  row.a[0] =       18.593;
  row.a[1] =    -0.098114;
  row.a[2] =  -0.00077568;
  row.a[3] =   4.6453e-06;
  row.a[4] =   -1.893e-08;
  row.a[5] =   1.8712e-10;
  row.a[6] =  -5.4248e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
