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
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.02528;
  row.a[1] =    0.0025594;
  row.a[2] =  -6.6434e-05;
  row.a[3] =   8.2692e-07;
  row.a[4] =  -5.4826e-09;
  row.a[5] =   1.7973e-11;
  row.a[6] =  -2.2995e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =   -0.0038213;
  row.a[1] =    0.0099481;
  row.a[2] =   -0.0003339;
  row.a[3] =   4.7056e-06;
  row.a[4] =  -3.3373e-08;
  row.a[5] =    1.159e-10;
  row.a[6] =  -1.5734e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.22592;
  row.a[1] =     0.012912;
  row.a[2] =  -0.00034489;
  row.a[3] =   4.6716e-06;
  row.a[4] =   -3.311e-08;
  row.a[5] =    1.176e-10;
  row.a[6] =  -1.6522e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.16662;
  row.a[1] =     0.013791;
  row.a[2] =  -0.00035303;
  row.a[3] =    4.587e-06;
  row.a[4] =  -3.2834e-08;
  row.a[5] =   1.2083e-10;
  row.a[6] =   -1.766e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
