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
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =       0.0402;
  row.a[1] =   4.7219e-05;
  row.a[2] =  -2.3033e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 100GeV_fixedTarget_2021
  row.a[0] =      0.21629;
  row.a[1] =   -0.0011172;
  row.a[2] =   3.1484e-06;
  row.a[3] =  -1.5775e-07;
  row.a[4] =   1.0043e-09;
  row.a[5] =  -1.7882e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =     0.012296;
  row.a[1] =   0.00020495;
  row.a[2] =  -2.0543e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 100GeV_fixedTarget_2021
  row.a[0] =    -0.040237;
  row.a[1] =    0.0052069;
  row.a[2] =  -5.9497e-05;
  row.a[3] =   3.4656e-08;
  row.a[4] =   1.6132e-09;
  row.a[5] =  -4.8493e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
