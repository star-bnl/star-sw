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
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =     0.067575;
  row.a[1] =  -0.00026336;
  row.a[2] =  -1.6914e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 13p5GeV_fixedTarget_2020
  row.a[0] =      0.24077;
  row.a[1] =   -0.0027116;
  row.a[2] =   3.6446e-05;
  row.a[3] =  -4.9188e-07;
  row.a[4] =   2.5659e-09;
  row.a[5] =  -4.5165e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =     0.028489;
  row.a[1] =   7.0319e-05;
  row.a[2] =  -2.0933e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 13p5GeV_fixedTarget_2020
  row.a[0] =     -0.03606;
  row.a[1] =    0.0056441;
  row.a[2] =  -7.5945e-05;
  row.a[3] =   2.4577e-07;
  row.a[4] =   4.6505e-10;
  row.a[5] =  -2.6294e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
