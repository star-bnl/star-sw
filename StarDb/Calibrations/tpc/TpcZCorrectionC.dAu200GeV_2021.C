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
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =      0.19386;
  row.a[1] =   -0.0062842;
  row.a[2] =   6.8759e-05;
  row.a[3] =  -3.1349e-07;
  row.a[4] =   5.1523e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =     -0.11594;
  row.a[1] =      0.00694;
  row.a[2] =  -9.9173e-05;
  row.a[3] =   5.3364e-07;
  row.a[4] =  -9.9421e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
