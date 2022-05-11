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
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =     0.036843;
  row.a[1] =   2.2533e-05;
  row.a[2] =  -2.1695e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 70GeV_fixedTarget_2021
  row.a[0] =      0.23396;
  row.a[1] =    -0.002121;
  row.a[2] =   2.4251e-05;
  row.a[3] =  -3.6471e-07;
  row.a[4] =   1.9588e-09;
  row.a[5] =  -3.4535e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =   -0.0028934;
  row.a[1] =    0.0003794;
  row.a[2] =  -2.6131e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 70GeV_fixedTarget_2021
  row.a[0] =    -0.059742;
  row.a[1] =    0.0063783;
  row.a[2] =  -8.8764e-05;
  row.a[3] =    3.667e-07;
  row.a[4] =  -7.6306e-11;
  row.a[5] =   -1.694e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
