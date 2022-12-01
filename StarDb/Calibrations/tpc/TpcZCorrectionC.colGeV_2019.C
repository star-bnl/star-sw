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
  row.npar =            5;// colGeV_2019
  row.a[0] =      0.06265;
  row.a[1] =  -0.00073307;
  row.a[2] =   1.5836e-06;
  row.a[3] =   8.0793e-09;
  row.a[4] =  -3.1872e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// colGeV_2019
  row.a[0] =    -0.058473;
  row.a[1] =     0.006105;
  row.a[2] =  -7.8884e-05;
  row.a[3] =   3.2402e-07;
  row.a[4] =  -3.9928e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
