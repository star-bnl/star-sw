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
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.029147;
  row.a[1] =    0.0048785;
  row.a[2] =  -0.00015721;
  row.a[3] =   2.1428e-06;
  row.a[4] =   -1.428e-08;
  row.a[5] =   4.5337e-11;
  row.a[6] =  -5.5246e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =      0.03628;
  row.a[1] =    0.0088779;
  row.a[2] =  -0.00035213;
  row.a[3] =   5.7126e-06;
  row.a[4] =  -4.5595e-08;
  row.a[5] =   1.7441e-10;
  row.a[6] =  -2.5657e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.061982;
  row.a[1] =    0.0013673;
  row.a[2] =  -2.8079e-05;
  row.a[3] =   4.2813e-07;
  row.a[4] =  -3.6025e-09;
  row.a[5] =   1.4888e-11;
  row.a[6] =  -2.3845e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 31p2GeV_fixedTarget_2020
  row.a[0] =    -0.088578;
  row.a[1] =     0.011495;
  row.a[2] =  -0.00030287;
  row.a[3] =   3.9935e-06;
  row.a[4] =  -2.9413e-08;
  row.a[5] =   1.1218e-10;
  row.a[6] =  -1.7024e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
