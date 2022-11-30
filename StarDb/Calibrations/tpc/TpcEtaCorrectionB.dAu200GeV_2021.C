TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.3;
  row.max =   1.3;
  row.npar =            6;// dAu200GeV_2021
  row.a[0] =    -0.020493;
  row.a[1] =     0.010042;
  row.a[2] =      0.18949;
  row.a[3] =     -0.40703;
  row.a[4] =      0.68657;
  row.a[5] =     -0.47764;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.6;
  row.max =   1.8;
  row.npar =            7;// dAu200GeV_2021
  row.a[0] =     0.011309;
  row.a[1] =   -0.0098652;
  row.a[2] =     -0.38765;
  row.a[3] =      0.15116;
  row.a[4] =       1.1221;
  row.a[5] =      -1.1923;
  row.a[6] =      0.32871;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.3;
  row.npar =            9;// dAu200GeV_2021
  row.a[0] =    -0.015877;
  row.a[1] =     0.065048;
  row.a[2] =      0.40363;
  row.a[3] =     -0.68877;
  row.a[4] =      -4.8055;
  row.a[5] =      -6.7575;
  row.a[6] =       -1.404;
  row.a[7] =       3.1159;
  row.a[8] =       1.5346;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.5;
  row.npar =            9;// dAu200GeV_2021
  row.a[0] =    -0.003759;
  row.a[1] =    -0.009942;
  row.a[2] =   -0.0092306;
  row.a[3] =       1.2096;
  row.a[4] =      0.99101;
  row.a[5] =       -3.645;
  row.a[6] =       -6.058;
  row.a[7] =      -3.2572;
  row.a[8] =     -0.59467;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
