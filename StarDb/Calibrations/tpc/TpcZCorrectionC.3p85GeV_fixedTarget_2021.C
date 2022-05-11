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
  row.npar =            3;// 3p85GeV_fixedTarget_2021
  row.a[0] =     0.043946;
  row.a[1] =  -8.7062e-05;
  row.a[2] =  -1.8341e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 3p85GeV_fixedTarget_2021
  row.a[0] =      0.22757;
  row.a[1] =    -0.002513;
  row.a[2] =   3.0045e-05;
  row.a[3] =  -4.2879e-07;
  row.a[4] =   2.3184e-09;
  row.a[5] =  -4.1941e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 3p85GeV_fixedTarget_2021
  row.a[0] =     0.020266;
  row.a[1] =  -7.3137e-06;
  row.a[2] =  -9.7105e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 3p85GeV_fixedTarget_2021
  row.a[0] =    -0.058121;
  row.a[1] =    0.0068246;
  row.a[2] =  -0.00010292;
  row.a[3] =   5.3923e-07;
  row.a[4] =   -1.001e-09;
  row.a[5] =   1.4291e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
