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
  row.npar =            3;// AuAu200GeV_2019
  row.a[0] =    -0.060889;
  row.a[1] =   0.00078376;
  row.a[2] =  -2.0677e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =     -0.11542;
  row.a[1] =    0.0059462;
  row.a[2] =   -9.486e-05;
  row.a[3] =   5.8958e-07;
  row.a[4] =  -1.2144e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
