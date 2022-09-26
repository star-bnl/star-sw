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
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.026321;
  row.a[1] =    0.0025289;
  row.a[2] =  -8.1839e-05;
  row.a[3] =    1.087e-06;
  row.a[4] =     -6.6e-09;
  row.a[5] =   1.7458e-11;
  row.a[6] =  -1.5417e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.014071;
  row.a[1] =     0.011641;
  row.a[2] =   -0.0004034;
  row.a[3] =   6.0593e-06;
  row.a[4] =  -4.5739e-08;
  row.a[5] =     1.68e-10;
  row.a[6] =   -2.397e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.11478;
  row.a[1] =    0.0039189;
  row.a[2] =  -9.0408e-05;
  row.a[3] =   1.1706e-06;
  row.a[4] =  -7.9757e-09;
  row.a[5] =   2.7273e-11;
  row.a[6] =  -3.7196e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.11337;
  row.a[1] =      0.01184;
  row.a[2] =  -0.00031389;
  row.a[3] =   4.2799e-06;
  row.a[4] =   -3.256e-08;
  row.a[5] =   1.2737e-10;
  row.a[6] =  -1.9686e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
