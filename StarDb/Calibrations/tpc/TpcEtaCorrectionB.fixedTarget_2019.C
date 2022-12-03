TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.0;
  row.npar =            3;// fixedTarget_2019
  row.a[0] =     0.020132;
  row.a[1] =     -0.11159;
  row.a[2] =     -0.17212;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.0;
  row.npar =            7;// fixedTarget_2019
  row.a[0] =        0.252;
  row.a[1] =     -0.42396;
  row.a[2] =      -3.4833;
  row.a[3] =      -7.4244;
  row.a[4] =       -7.449;
  row.a[5] =      -3.4958;
  row.a[6] =     -0.61755;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =  -0.9;
  row.npar =            3;// fixedTarget_2019
  row.a[0] =     -0.74551;
  row.a[1] =      -1.0924;
  row.a[2] =     -0.38657;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// fixedTarget_2019
  row.a[0] =     -0.55947;
  row.a[1] =     -0.31531;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
