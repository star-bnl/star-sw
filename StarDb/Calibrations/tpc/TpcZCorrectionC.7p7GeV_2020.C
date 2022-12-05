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
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =     0.021282;
  row.a[1] =   0.00032589;
  row.a[2] =   -8.125e-06;
  row.a[3] =   4.9855e-08;
  row.a[4] =  -1.0662e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =     0.030934;
  row.a[1] =    0.0040094;
  row.a[2] =  -5.8648e-05;
  row.a[3] =   2.3923e-07;
  row.a[4] =  -2.9444e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
