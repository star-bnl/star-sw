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
  row.a[0] =     0.036994;
  row.a[1] =  -4.5791e-05;
  row.a[2] =  -4.5582e-06;
  row.a[3] =    3.273e-08;
  row.a[4] =  -7.2342e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 19GeV_2019
  row.a[0] =    -0.044663;
  row.a[1] =    0.0060961;
  row.a[2] =  -7.9402e-05;
  row.a[3] =   3.2294e-07;
  row.a[4] =  -4.0034e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
