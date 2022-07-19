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
  row.npar =            6;// 7.3GeV_fixedTarget_2019
  row.a[0] =     -0.10515;
  row.a[1] =    0.0066687;
  row.a[2] =  -0.00014528;
  row.a[3] =   1.4033e-06;
  row.a[4] =   -6.163e-09;
  row.a[5] =   9.9873e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.043379;
  row.a[1] =    0.0095217;
  row.a[2] =   -0.0003063;
  row.a[3] =   4.4571e-06;
  row.a[4] =  -3.3549e-08;
  row.a[5] =   1.2424e-10;
  row.a[6] =  -1.7899e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =      -0.0644;
  row.a[1] =    0.0051877;
  row.a[2] =  -0.00015309;
  row.a[3] =   2.2274e-06;
  row.a[4] =  -1.6906e-08;
  row.a[5] =   6.3799e-11;
  row.a[6] =  -9.4427e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =      -1.4301;
  row.a[1] =     0.087501;
  row.a[2] =    -0.001993;
  row.a[3] =   2.3203e-05;
  row.a[4] =  -1.4781e-07;
  row.a[5] =   4.8834e-10;
  row.a[6] =  -6.5281e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
