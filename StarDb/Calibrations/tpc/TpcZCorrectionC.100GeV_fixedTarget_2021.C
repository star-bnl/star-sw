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
  row.npar =            5;// 100GeV_fixedTarget_2021
  row.a[0] =     0.057912;
  row.a[1] =   -0.0028578;
  row.a[2] =    4.196e-05;
  row.a[3] =  -2.2203e-07;
  row.a[4] =   3.6294e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 100GeV_fixedTarget_2021
  row.a[0] =     0.003689;
  row.a[1] =    0.0065215;
  row.a[2] =   -0.0002126;
  row.a[3] =   3.4117e-06;
  row.a[4] =  -2.8635e-08;
  row.a[5] =   1.1482e-10;
  row.a[6] =  -1.7439e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 100GeV_fixedTarget_2021
  row.a[0] =    -0.048446;
  row.a[1] =    0.0034882;
  row.a[2] =  -9.0955e-05;
  row.a[3] =   1.3152e-06;
  row.a[4] =  -1.0345e-08;
  row.a[5] =   4.0552e-11;
  row.a[6] =  -6.1998e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 100GeV_fixedTarget_2021
  row.a[0] =     -0.17411;
  row.a[1] =     0.021066;
  row.a[2] =  -0.00057942;
  row.a[3] =   8.1355e-06;
  row.a[4] =  -6.2377e-08;
  row.a[5] =   2.4176e-10;
  row.a[6] =    -3.68e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
