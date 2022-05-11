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
  row.npar =            3;// 7p3GeV_fixedTarget_2020
  row.a[0] =     0.067281;
  row.a[1] =  -0.00025481;
  row.a[2] =  -1.7891e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p3GeV_fixedTarget_2020
  row.a[0] =      0.25605;
  row.a[1] =   -0.0036307;
  row.a[2] =   5.3952e-05;
  row.a[3] =  -6.5904e-07;
  row.a[4] =   3.3224e-09;
  row.a[5] =  -5.8049e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p3GeV_fixedTarget_2020
  row.a[0] =     0.031597;
  row.a[1] =  -1.2732e-06;
  row.a[2] =  -1.7224e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p3GeV_fixedTarget_2020
  row.a[0] =    -0.026929;
  row.a[1] =    0.0047878;
  row.a[2] =  -5.0896e-05;
  row.a[3] =  -4.4385e-08;
  row.a[4] =   1.9247e-09;
  row.a[5] =  -5.2898e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
