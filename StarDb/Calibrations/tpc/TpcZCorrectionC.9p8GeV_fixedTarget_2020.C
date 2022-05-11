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
  row.npar =            3;// 9p8GeV_fixedTarget_2020
  row.a[0] =      0.06675;
  row.a[1] =  -0.00022508;
  row.a[2] =  -1.9643e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p8GeV_fixedTarget_2020
  row.a[0] =      0.24342;
  row.a[1] =   -0.0022308;
  row.a[2] =   2.0906e-05;
  row.a[3] =  -3.2476e-07;
  row.a[4] =   1.7951e-09;
  row.a[5] =  -3.2335e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p8GeV_fixedTarget_2020
  row.a[0] =     0.032593;
  row.a[1] =  -1.0007e-05;
  row.a[2] =  -1.7638e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p8GeV_fixedTarget_2020
  row.a[0] =     -0.03766;
  row.a[1] =    0.0060709;
  row.a[2] =  -8.6554e-05;
  row.a[3] =   3.5096e-07;
  row.a[4] =  -1.5707e-11;
  row.a[5] =  -1.7873e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
