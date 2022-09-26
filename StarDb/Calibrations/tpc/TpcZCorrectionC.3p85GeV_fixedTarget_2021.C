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
  row.a[0] =    0.0092938;
  row.a[1] =   7.6921e-06;
  row.a[2] =    9.625e-06;
  row.a[3] =  -2.9731e-07;
  row.a[4] =   3.1541e-09;
  row.a[5] =  -1.4806e-11;
  row.a[6] =   2.5396e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =     0.034182;
  row.a[1] =    0.0055461;
  row.a[2] =  -0.00018071;
  row.a[3] =   2.2557e-06;
  row.a[4] =   -1.365e-08;
  row.a[5] =   3.8374e-11;
  row.a[6] =  -3.8816e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =     -0.11724;
  row.a[1] =    0.0059063;
  row.a[2] =  -0.00014774;
  row.a[3] =    1.959e-06;
  row.a[4] =  -1.3833e-08;
  row.a[5] =   4.9334e-11;
  row.a[6] =  -6.9928e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =    -0.098393;
  row.a[1] =    0.0075304;
  row.a[2] =  -0.00016596;
  row.a[3] =   1.9534e-06;
  row.a[4] =  -1.3551e-08;
  row.a[5] =   5.0168e-11;
  row.a[6] =  -7.4668e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
