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
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =     0.042504;
  row.a[1] =  -6.4722e-06;
  row.a[2] =  -2.1739e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 26p5GeV_fixedTarget_2020
  row.a[0] =      0.23112;
  row.a[1] =   -0.0017297;
  row.a[2] =   1.5675e-05;
  row.a[3] =  -2.9299e-07;
  row.a[4] =   1.6743e-09;
  row.a[5] =  -3.0046e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =    0.0095982;
  row.a[1] =   0.00020738;
  row.a[2] =    -1.94e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.055725;
  row.a[1] =     0.006743;
  row.a[2] =   -9.978e-05;
  row.a[3] =   4.8437e-07;
  row.a[4] =  -6.1828e-10;
  row.a[5] =   -7.806e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
