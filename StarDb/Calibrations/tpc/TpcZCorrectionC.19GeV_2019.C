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
  row.npar =            3;// 19GeV_2019
  row.a[0] =      0.01079;
  row.a[1] =   6.2112e-06;
  row.a[2] =  -6.3471e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 19GeV_2019
  row.a[0] =    -0.021678;
  row.a[1] =    0.0033774;
  row.a[2] =  -6.1582e-05;
  row.a[3] =   4.5358e-07;
  row.a[4] =  -1.5942e-09;
  row.a[5] =   2.1676e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 19GeV_2019
  row.a[0] =     0.011426;
  row.a[1] =   3.4483e-05;
  row.a[2] =  -8.6976e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 19GeV_2019
  row.a[0] =     -0.03328;
  row.a[1] =    0.0040861;
  row.a[2] =  -6.9048e-05;
  row.a[3] =   4.6185e-07;
  row.a[4] =  -1.4442e-09;
  row.a[5] =   1.7026e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
