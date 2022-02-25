TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  12.0;
  row.max = 220.0;
  row.npar =            5;// AuAu200_2019
  row.type = 20;
  row.a[0] =    -0.026642;
  row.a[1] =   0.00040864;
  row.a[2] =    -1.73e-06;
  row.a[3] =   2.9204e-09;
  row.a[4] =  -8.2241e-12;
  row.a[5] =       2.9421;
  row.a[6] =     -0.27701;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  12.0;
  row.max = 220.0;
  row.npar =            5;// AuAu200_2019
  row.type = 20;
  row.a[0] =     -0.11965;
  row.a[1] =    0.0090877;
  row.a[2] =  -0.00013051;
  row.a[3] =   6.3338e-07;
  row.a[4] =   -1.003e-09;
  row.a[5] =       2.4533;
  row.a[6] =     -0.21144;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
