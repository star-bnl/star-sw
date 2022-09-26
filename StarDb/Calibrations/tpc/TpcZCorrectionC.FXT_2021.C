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
  row.npar =            7;// FXT_2021
  row.a[0] =     0.025865;
  row.a[1] =    -0.001358;
  row.a[2] =   5.5193e-05;
  row.a[3] =  -1.0182e-06;
  row.a[4] =   8.7905e-09;
  row.a[5] =  -3.5916e-11;
  row.a[6] =   5.5694e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =     0.011104;
  row.a[1] =    0.0078047;
  row.a[2] =  -0.00025638;
  row.a[3] =   3.4025e-06;
  row.a[4] =  -2.2222e-08;
  row.a[5] =   6.9312e-11;
  row.a[6] =  -8.2045e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =    -0.099004;
  row.a[1] =    0.0044037;
  row.a[2] =  -0.00010331;
  row.a[3] =   1.3288e-06;
  row.a[4] =  -9.1963e-09;
  row.a[5] =   3.2291e-11;
  row.a[6] =   -4.525e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =    -0.078973;
  row.a[1] =    0.0061826;
  row.a[2] =  -0.00012321;
  row.a[3] =   1.3217e-06;
  row.a[4] =  -8.8608e-09;
  row.a[5] =   3.3212e-11;
  row.a[6] =  -5.1041e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
