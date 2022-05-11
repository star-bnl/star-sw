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
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =     0.049268;
  row.a[1] =  -0.00012076;
  row.a[2] =  -1.6129e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 31GeV_fixedTarget_2019
  row.a[0] =      0.25273;
  row.a[1] =   -0.0032319;
  row.a[2] =   4.3396e-05;
  row.a[3] =  -5.2085e-07;
  row.a[4] =   2.5795e-09;
  row.a[5] =  -4.4079e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =   -0.0025282;
  row.a[1] =    0.0003754;
  row.a[2] =  -2.3491e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 31GeV_fixedTarget_2019
  row.a[0] =    -0.052288;
  row.a[1] =    0.0047634;
  row.a[2] =  -2.9469e-05;
  row.a[3] =   -3.478e-07;
  row.a[4] =   3.4747e-09;
  row.a[5] =  -7.9678e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
