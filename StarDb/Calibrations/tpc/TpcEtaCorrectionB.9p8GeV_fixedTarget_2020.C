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
  row.npar =            4;// 9p8GeV_fixedTarget_2020
  row.a[0] =     0.022875;
  row.a[1] =      -0.0838;
  row.a[2] =    -0.053802;
  row.a[3] =      0.10539;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            4;// 9p8GeV_fixedTarget_2020
  row.a[0] =       0.2098;
  row.a[1] =    -0.017448;
  row.a[2] =     -0.22518;
  row.a[3] =    -0.057761;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 9p8GeV_fixedTarget_2020
  row.a[0] =     -0.80899;
  row.a[1] =      -1.1412;
  row.a[2] =     -0.38883;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 9p8GeV_fixedTarget_2020
  row.a[0] =      -3.0358;
  row.a[1] =      -3.1335;
  row.a[2] =     -0.79408;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
