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
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =    0.0021096;
  row.a[1] =  -8.9536e-06;
  row.a[2] =   4.7835e-06;
  row.a[3] =  -1.3391e-07;
  row.a[4] =   1.4108e-09;
  row.a[5] =  -6.3851e-12;
  row.a[6] =    1.018e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =     -0.16788;
  row.a[1] =     0.017107;
  row.a[2] =  -0.00047546;
  row.a[3] =   6.1902e-06;
  row.a[4] =  -4.2122e-08;
  row.a[5] =   1.4351e-10;
  row.a[6] =  -1.9332e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =    -0.037067;
  row.a[1] =    0.0031818;
  row.a[2] =  -7.5131e-05;
  row.a[3] =   9.1267e-07;
  row.a[4] =  -6.1554e-09;
  row.a[5] =   2.1593e-11;
  row.a[6] =  -3.0492e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =    -0.076889;
  row.a[1] =    0.0096423;
  row.a[2] =  -0.00022159;
  row.a[3] =   2.6891e-06;
  row.a[4] =  -1.9228e-08;
  row.a[5] =   7.2548e-11;
  row.a[6] =  -1.0887e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
