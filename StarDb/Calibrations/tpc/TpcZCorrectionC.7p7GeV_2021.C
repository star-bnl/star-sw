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
  row.npar =            3;// 7p7GeV_2021
  row.a[0] =      0.01731;
  row.a[1] =   -8.492e-06;
  row.a[2] =  -9.5361e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2021
  row.a[0] =    0.0048486;
  row.a[1] =     0.003154;
  row.a[2] =  -5.3261e-05;
  row.a[3] =   3.2283e-07;
  row.a[4] =  -9.4024e-10;
  row.a[5] =   1.1097e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2021
  row.a[0] =     0.019485;
  row.a[1] =  -3.3934e-05;
  row.a[2] =  -8.7877e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2021
  row.a[0] =     0.015399;
  row.a[1] =    0.0026149;
  row.a[2] =  -3.6692e-05;
  row.a[3] =    1.148e-07;
  row.a[4] =   1.2642e-10;
  row.a[5] =  -8.0028e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
