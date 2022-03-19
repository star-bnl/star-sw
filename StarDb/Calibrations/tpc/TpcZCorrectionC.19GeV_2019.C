TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.0;
  row.max = 207.0;
  row.npar =            3;// 19GeV_2019
  row.a[0] =     -0.02073;
  row.a[1] =   0.00032242;
  row.a[2] =  -1.8445e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  25.0;
  row.max = 207.0;
  row.npar =            6;// 19GeV_2019
  row.a[0] =    -0.052827;
  row.a[1] =    0.0044516;
  row.a[2] =  -6.4918e-05;
  row.a[3] =   3.7007e-07;
  row.a[4] =  -1.1205e-09;
  row.a[5] =   1.6333e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
