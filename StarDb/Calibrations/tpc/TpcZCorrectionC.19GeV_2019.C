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
  row.npar =            5;// 19GeV_2019
  row.a[0] =     0.037592;
  row.a[1] =  -1.4044e-05;
  row.a[2] =  -5.0668e-06;
  row.a[3] =   3.5708e-08;
  row.a[4] =  -7.8686e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 19GeV_2019
  row.a[0] =    -0.032951;
  row.a[1] =     0.005801;
  row.a[2] =  -7.5442e-05;
  row.a[3] =   2.9804e-07;
  row.a[4] =   -3.447e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
