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
  row.npar =            5;// 31p2GeV_fixedTarget_2020
  row.a[0] =     0.059518;
  row.a[1] =   -0.0021419;
  row.a[2] =   2.9551e-05;
  row.a[3] =  -1.5462e-07;
  row.a[4] =   2.3536e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.032883;
  row.a[1] =     0.012131;
  row.a[2] =   -0.0004022;
  row.a[3] =   6.1926e-06;
  row.a[4] =  -4.9308e-08;
  row.a[5] =   1.9113e-10;
  row.a[6] =  -2.8539e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.023197;
  row.a[1] =    0.0033222;
  row.a[2] =  -9.0228e-05;
  row.a[3] =   1.2934e-06;
  row.a[4] =  -1.0191e-08;
  row.a[5] =   4.0184e-11;
  row.a[6] =  -6.1804e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =      -1.1573;
  row.a[1] =     0.091164;
  row.a[2] =   -0.0024218;
  row.a[3] =   3.1821e-05;
  row.a[4] =  -2.2242e-07;
  row.a[5] =   7.8747e-10;
  row.a[6] =  -1.1084e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
