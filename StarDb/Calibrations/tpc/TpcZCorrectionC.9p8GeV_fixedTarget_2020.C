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
  row.npar =            6;// 9p8GeV_fixedTarget_2020
  row.a[0] =   -0.0084275;
  row.a[1] =    0.0019453;
  row.a[2] =  -5.2433e-05;
  row.a[3] =   5.4882e-07;
  row.a[4] =  -2.4812e-09;
  row.a[5] =   3.9245e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 9p8GeV_fixedTarget_2020
  row.a[0] =    -0.054233;
  row.a[1] =     0.013275;
  row.a[2] =   -0.0004435;
  row.a[3] =   6.6758e-06;
  row.a[4] =  -5.1258e-08;
  row.a[5] =   1.9172e-10;
  row.a[6] =  -2.7729e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 9p8GeV_fixedTarget_2020
  row.a[0] =   -0.0061699;
  row.a[1] =     0.001976;
  row.a[2] =  -5.2651e-05;
  row.a[3] =   7.6662e-07;
  row.a[4] =  -6.3137e-09;
  row.a[5] =   2.5977e-11;
  row.a[6] =  -4.1296e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 9p8GeV_fixedTarget_2020
  row.a[0] =     -0.10085;
  row.a[1] =     0.015257;
  row.a[2] =  -0.00040201;
  row.a[3] =   5.4099e-06;
  row.a[4] =  -4.0782e-08;
  row.a[5] =   1.5782e-10;
  row.a[6] =  -2.4127e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
