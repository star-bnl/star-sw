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
  row.npar =            2;// 7p7GeV_2020
  row.a[0] =     0.015787;
  row.a[1] =  -8.3502e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =     -0.10648;
  row.a[1] =    0.0069241;
  row.a[2] =  -9.3894e-05;
  row.a[3] =   4.6827e-07;
  row.a[4] =  -8.1839e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
