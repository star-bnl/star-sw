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
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =     0.046745;
  row.a[1] =  -0.00012261;
  row.a[2] =  -1.5125e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7.3GeV_fixedTarget_2019
  row.a[0] =       0.2248;
  row.a[1] =   -0.0027346;
  row.a[2] =   3.7654e-05;
  row.a[3] =    -5.08e-07;
  row.a[4] =   2.7195e-09;
  row.a[5] =  -4.9359e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =   -0.0026338;
  row.a[1] =   0.00033799;
  row.a[2] =  -2.0853e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.068142;
  row.a[1] =     0.006412;
  row.a[2] =   -8.967e-05;
  row.a[3] =   3.9471e-07;
  row.a[4] =  -2.4903e-10;
  row.a[5] =  -1.3565e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
