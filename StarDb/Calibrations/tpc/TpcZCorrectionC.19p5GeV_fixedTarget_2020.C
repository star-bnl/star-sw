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
  row.npar =            3;// 19p5GeV_fixedTarget_2020
  row.a[0] =     0.067641;
  row.a[1] =  -0.00022621;
  row.a[2] =  -1.9437e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 19p5GeV_fixedTarget_2020
  row.a[0] =      0.24625;
  row.a[1] =   -0.0025878;
  row.a[2] =   3.2352e-05;
  row.a[3] =  -4.3965e-07;
  row.a[4] =   2.2602e-09;
  row.a[5] =   -3.881e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 19p5GeV_fixedTarget_2020
  row.a[0] =     0.031654;
  row.a[1] =    1.829e-05;
  row.a[2] =  -1.9188e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 19p5GeV_fixedTarget_2020
  row.a[0] =    -0.036114;
  row.a[1] =    0.0056432;
  row.a[2] =  -7.6024e-05;
  row.a[3] =   2.4963e-07;
  row.a[4] =   4.2908e-10;
  row.a[5] =  -2.5379e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
