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
  row.npar =            6;// 70GeV_fixedTarget_2021
  row.a[0] =    -0.022273;
  row.a[1] =    0.0010172;
  row.a[2] =  -3.0055e-05;
  row.a[3] =   4.0953e-07;
  row.a[4] =  -2.2895e-09;
  row.a[5] =   4.2843e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =    -0.027164;
  row.a[1] =     0.009486;
  row.a[2] =  -0.00031159;
  row.a[3] =   4.9054e-06;
  row.a[4] =  -4.0031e-08;
  row.a[5] =   1.5773e-10;
  row.a[6] =  -2.3767e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =    -0.032441;
  row.a[1] =    0.0025814;
  row.a[2] =  -7.2382e-05;
  row.a[3] =   1.0825e-06;
  row.a[4] =  -8.6488e-09;
  row.a[5] =   3.4179e-11;
  row.a[6] =  -5.2614e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 70GeV_fixedTarget_2021
  row.a[0] =     -0.48614;
  row.a[1] =     0.039625;
  row.a[2] =   -0.0010174;
  row.a[3] =   1.3287e-05;
  row.a[4] =  -9.4649e-08;
  row.a[5] =   3.4502e-10;
  row.a[6] =  -5.0088e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
