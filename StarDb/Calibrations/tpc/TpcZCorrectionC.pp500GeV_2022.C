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
  row.npar =            5;// pp500GeV_2022
  row.a[0] =     -0.01902;
  row.a[1] =    0.0012669;
  row.a[2] =  -1.7378e-05;
  row.a[3] =   9.2038e-08;
  row.a[4] =  -1.7943e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// pp500GeV_2022
  row.a[0] =   -0.0052596;
  row.a[1] =   0.00092929;
  row.a[2] =   2.8958e-06;
  row.a[3] =  -1.3393e-07;
  row.a[4] =   4.6988e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
