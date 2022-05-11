TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeVc_2020
  row.a[0] =     0.015993;
  row.a[1] =  -2.3826e-05;
  row.a[2] =   -7.523e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeVc_2020
  row.a[0] =    -0.070611;
  row.a[1] =    0.0053669;
  row.a[2] =   -8.501e-05;
  row.a[3] =   5.8007e-07;
  row.a[4] =  -1.9698e-09;
  row.a[5] =   2.6794e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeVc_2020
  row.a[0] =     0.019275;
  row.a[1] =  -4.7957e-05;
  row.a[2] =  -7.7329e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeVc_2020
  row.a[0] =    -0.075096;
  row.a[1] =    0.0057293;
  row.a[2] =   -8.623e-05;
  row.a[3] =   5.3765e-07;
  row.a[4] =  -1.6397e-09;
  row.a[5] =   1.9987e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
