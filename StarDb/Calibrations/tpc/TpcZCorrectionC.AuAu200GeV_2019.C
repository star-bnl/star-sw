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
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =      0.14998;
  row.a[1] =   -0.0043205;
  row.a[2] =   4.5173e-05;
  row.a[3] =  -1.9875e-07;
  row.a[4] =   3.1314e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =     -0.11271;
  row.a[1] =    0.0067239;
  row.a[2] =   -9.019e-05;
  row.a[3] =   4.4395e-07;
  row.a[4] =  -7.4091e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
