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
  row.npar =            3;// 11p5GeV_2020
  row.a[0] =     0.042976;
  row.a[1] =  -0.00023539;
  row.a[2] =  -8.7001e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 11p5GeV_2020
  row.a[0] =    -0.017721;
  row.a[1] =    0.0039978;
  row.a[2] =  -7.0451e-05;
  row.a[3] =   4.9309e-07;
  row.a[4] =  -1.6725e-09;
  row.a[5] =   2.2066e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 11p5GeV_2020
  row.a[0] =     0.046555;
  row.a[1] =  -0.00027492;
  row.a[2] =  -7.9828e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 11p5GeV_2020
  row.a[0] =   -0.0033272;
  row.a[1] =    0.0036479;
  row.a[2] =  -6.0846e-05;
  row.a[3] =   3.6064e-07;
  row.a[4] =   -9.582e-10;
  row.a[5] =   9.0037e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
