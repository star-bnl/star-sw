TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.6;
  row.max = 208.0;
  row.npar =            3;// pp500GeV_2022
  row.a[0] =    -0.041287;
  row.a[1] =    0.0006361;
  row.a[2] =  -2.4743e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            4;// pp500GeV_2022
  row.a[0] =    -0.094712;
  row.a[1] =    0.0026388;
  row.a[2] =  -2.0486e-05;
  row.a[3] =   4.8343e-08;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
