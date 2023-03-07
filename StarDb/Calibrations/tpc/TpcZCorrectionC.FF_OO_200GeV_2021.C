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
  row.npar =            5;// FF_OO_200GeV_2021
  row.a[0] =     0.020564;
  row.a[1] =  -0.00040436;
  row.a[2] =   4.3741e-07;
  row.a[3] =   3.0378e-08;
  row.a[4] =  -1.1551e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// FF_OO_200GeV_2021
  row.a[0] =    -0.073739;
  row.a[1] =    0.0053338;
  row.a[2] =   -7.585e-05;
  row.a[3] =   3.8176e-07;
  row.a[4] =    -6.38e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
