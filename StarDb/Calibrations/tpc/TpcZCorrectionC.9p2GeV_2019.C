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
  row.npar =            2;// 9p2GeV_2019
  row.a[0] =    -0.032723;
  row.a[1] =    0.0001273;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =    -0.084295;
  row.a[1] =    0.0047306;
  row.a[2] =   -7.466e-05;
  row.a[3] =   4.4488e-07;
  row.a[4] =  -9.0145e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
