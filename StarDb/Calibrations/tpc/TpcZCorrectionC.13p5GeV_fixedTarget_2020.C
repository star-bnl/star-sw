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
  row.npar =            6;// 13p5GeV_fixedTarget_2020
  row.a[0] =     -0.03726;
  row.a[1] =    0.0037101;
  row.a[2] =  -9.5198e-05;
  row.a[3] =   1.0299e-06;
  row.a[4] =  -4.9421e-09;
  row.a[5] =   8.5356e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 13p5GeV_fixedTarget_2020
  row.a[0] =    -0.022372;
  row.a[1] =    0.0099635;
  row.a[2] =  -0.00032884;
  row.a[3] =   4.9432e-06;
  row.a[4] =  -3.8491e-08;
  row.a[5] =   1.4649e-10;
  row.a[6] =  -2.1552e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 13p5GeV_fixedTarget_2020
  row.a[0] =   -0.0059204;
  row.a[1] =    0.0021891;
  row.a[2] =  -6.2803e-05;
  row.a[3] =   9.0873e-07;
  row.a[4] =  -7.1382e-09;
  row.a[5] =   2.7922e-11;
  row.a[6] =  -4.2583e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 13p5GeV_fixedTarget_2020
  row.a[0] =    -0.045166;
  row.a[1] =     0.012894;
  row.a[2] =  -0.00035661;
  row.a[3] =   4.9271e-06;
  row.a[4] =  -3.7756e-08;
  row.a[5] =   1.4745e-10;
  row.a[6] =  -2.2664e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
