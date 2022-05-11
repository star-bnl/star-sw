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
  row.npar =            3;// 7p7GeV_2021
  row.a[0] =     0.014847;
  row.a[1] =   1.0452e-05;
  row.a[2] =  -9.3952e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2021
  row.a[0] =     -0.01594;
  row.a[1] =    0.0034435;
  row.a[2] =  -5.4775e-05;
  row.a[3] =   3.3478e-07;
  row.a[4] =  -1.0058e-09;
  row.a[5] =   1.2267e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2021
  row.a[0] =     0.017326;
  row.a[1] =     -2.1e-05;
  row.a[2] =  -8.4514e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2021
  row.a[0] =    -0.020817;
  row.a[1] =    0.0037993;
  row.a[2] =  -5.6193e-05;
  row.a[3] =   2.9104e-07;
  row.a[4] =  -6.3872e-10;
  row.a[5] =   4.4486e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
