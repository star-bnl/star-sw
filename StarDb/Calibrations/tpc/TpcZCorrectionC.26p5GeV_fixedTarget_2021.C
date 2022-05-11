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
  row.npar =            3;// 26p5GeV_fixedTarget_2021
  row.a[0] =     0.040724;
  row.a[1] =   4.4452e-05;
  row.a[2] =  -2.1326e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 26p5GeV_fixedTarget_2021
  row.a[0] =      0.19072;
  row.a[1] =  -6.9703e-05;
  row.a[2] =   -7.592e-06;
  row.a[3] =  -1.3767e-07;
  row.a[4] =   1.2029e-09;
  row.a[5] =  -2.5147e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 26p5GeV_fixedTarget_2021
  row.a[0] =      0.01444;
  row.a[1] =   0.00012966;
  row.a[2] =  -1.5229e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 26p5GeV_fixedTarget_2021
  row.a[0] =    -0.046864;
  row.a[1] =     0.005364;
  row.a[2] =  -8.2047e-05;
  row.a[3] =   4.5241e-07;
  row.a[4] =  -1.0045e-09;
  row.a[5] =   5.8953e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
