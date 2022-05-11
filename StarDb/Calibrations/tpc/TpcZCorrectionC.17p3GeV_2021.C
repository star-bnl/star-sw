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
  row.npar =            3;// 17p3GeV_2021
  row.a[0] =      0.01266;
  row.a[1] =   3.9718e-05;
  row.a[2] =  -1.0008e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 17p3GeV_2021
  row.a[0] =    -0.014133;
  row.a[1] =    0.0035028;
  row.a[2] =  -6.3335e-05;
  row.a[3] =   4.5384e-07;
  row.a[4] =   -1.572e-09;
  row.a[5] =    2.128e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 17p3GeV_2021
  row.a[0] =     0.015398;
  row.a[1] =   7.9988e-06;
  row.a[2] =  -9.2632e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 17p3GeV_2021
  row.a[0] =   -0.0050203;
  row.a[1] =     0.002785;
  row.a[2] =  -4.0911e-05;
  row.a[3] =   1.8052e-07;
  row.a[4] =  -1.8113e-10;
  row.a[5] =  -3.8966e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
