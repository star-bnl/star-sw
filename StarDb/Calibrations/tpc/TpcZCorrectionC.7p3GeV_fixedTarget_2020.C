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
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =    0.0086404;
  row.a[1] =    0.0015876;
  row.a[2] =  -4.4013e-05;
  row.a[3] =   4.1865e-07;
  row.a[4] =  -1.6554e-09;
  row.a[5] =    1.446e-12;
  row.a[6] =   3.1851e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =    0.0083438;
  row.a[1] =     0.010282;
  row.a[2] =  -0.00036037;
  row.a[3] =   5.2512e-06;
  row.a[4] =  -3.8084e-08;
  row.a[5] =   1.3385e-10;
  row.a[6] =  -1.8248e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =    -0.089119;
  row.a[1] =    0.0041229;
  row.a[2] =  -0.00011068;
  row.a[3] =   1.5724e-06;
  row.a[4] =  -1.1606e-08;
  row.a[5] =   4.2354e-11;
  row.a[6] =  -6.0557e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =     -0.15803;
  row.a[1] =     0.014259;
  row.a[2] =  -0.00035826;
  row.a[3] =    4.536e-06;
  row.a[4] =  -3.2019e-08;
  row.a[5] =   1.1759e-10;
  row.a[6] =  -1.7301e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
