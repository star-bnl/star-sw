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
  row.npar =            3;// 31p2GeV_fixedTarget_2020
  row.a[0] =     0.071715;
  row.a[1] =  -0.00023215;
  row.a[2] =  -2.1115e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 31p2GeV_fixedTarget_2020
  row.a[0] =      0.23782;
  row.a[1] =   -0.0017112;
  row.a[2] =   1.3931e-05;
  row.a[3] =  -2.7061e-07;
  row.a[4] =   1.5409e-09;
  row.a[5] =  -2.7364e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31p2GeV_fixedTarget_2020
  row.a[0] =     0.033906;
  row.a[1] =   -3.776e-06;
  row.a[2] =  -1.9622e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.034488;
  row.a[1] =    0.0061113;
  row.a[2] =  -9.0261e-05;
  row.a[3] =   4.0249e-07;
  row.a[4] =  -2.9201e-10;
  row.a[5] =  -1.2842e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
