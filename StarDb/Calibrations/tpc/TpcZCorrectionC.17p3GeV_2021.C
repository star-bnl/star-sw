TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 17p3GeV_2021
  row.a[0] =    -0.032249;
  row.a[1] =    0.0028914;
  row.a[2] =  -7.4343e-05;
  row.a[3] =   9.5771e-07;
  row.a[4] =  -6.5515e-09;
  row.a[5] =   2.2696e-11;
  row.a[6] =  -3.1519e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 17p3GeV_2021
  row.a[0] =     -0.16156;
  row.a[1] =     0.010958;
  row.a[2] =   -0.0001826;
  row.a[3] =   1.3622e-06;
  row.a[4] =  -5.3085e-09;
  row.a[5] =   1.0471e-11;
  row.a[6] =  -8.1623e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 17p3GeV_2021
  row.a[0] =     -0.02174;
  row.a[1] =    0.0026282;
  row.a[2] =  -7.1185e-05;
  row.a[3] =    9.381e-07;
  row.a[4] =  -6.4931e-09;
  row.a[5] =   2.2614e-11;
  row.a[6] =  -3.1469e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 17p3GeV_2021
  row.a[0] =     -0.18664;
  row.a[1] =     0.014594;
  row.a[2] =  -0.00029767;
  row.a[3] =    3.068e-06;
  row.a[4] =  -1.8539e-08;
  row.a[5] =   6.1689e-11;
  row.a[6] =  -8.5857e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
