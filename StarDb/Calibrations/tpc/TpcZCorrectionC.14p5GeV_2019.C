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
  row.npar =            7;// 14p5GeV_2019
  row.a[0] =    -0.089193;
  row.a[1] =    0.0039643;
  row.a[2] =   -9.116e-05;
  row.a[3] =   1.1339e-06;
  row.a[4] =  -7.3831e-09;
  row.a[5] =   2.4028e-11;
  row.a[6] =  -3.1154e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =    -0.064387;
  row.a[1] =    0.0042328;
  row.a[2] =  -6.3722e-05;
  row.a[3] =   3.8634e-07;
  row.a[4] =  -8.0292e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            6;// 14p5GeV_2019
  row.a[0] =     -0.06635;
  row.a[1] =    0.0021586;
  row.a[2] =   -3.636e-05;
  row.a[3] =   3.3419e-07;
  row.a[4] =  -1.4183e-09;
  row.a[5] =   2.1642e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =    -0.064803;
  row.a[1] =    0.0049704;
  row.a[2] =  -7.6357e-05;
  row.a[3] =   4.5772e-07;
  row.a[4] =  -9.3734e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
