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
  row.npar =            4;// 7p7GeV_2019
  row.a[0] =     0.049786;
  row.a[1] =   -0.0010033;
  row.a[2] =   6.0445e-06;
  row.a[3] =  -1.1855e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =    -0.081596;
  row.a[1] =     0.005046;
  row.a[2] =  -5.7337e-05;
  row.a[3] =   2.0818e-07;
  row.a[4] =  -2.0226e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
