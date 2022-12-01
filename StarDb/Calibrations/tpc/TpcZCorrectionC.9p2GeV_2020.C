TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2020
  row.a[0] =     0.038802;
  row.a[1] =   0.00028279;
  row.a[2] =  -1.0458e-05;
  row.a[3] =   6.5959e-08;
  row.a[4] =  -1.3997e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2020
  row.a[0] =     0.053671;
  row.a[1] =    0.0037747;
  row.a[2] =  -5.8379e-05;
  row.a[3] =   2.4143e-07;
  row.a[4] =  -3.0207e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
