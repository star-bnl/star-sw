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
  row.npar =            4;// 7p3GeV_fixedTarget_2020
  row.a[0] =     0.027862;
  row.a[1] =   -0.0016446;
  row.a[2] =      0.11692;
  row.a[3] =      0.19804;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 7p3GeV_fixedTarget_2020
  row.a[0] =      0.20389;
  row.a[1] =     0.047072;
  row.a[2] =    -0.098126;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 7p3GeV_fixedTarget_2020
  row.a[0] =      0.56268;
  row.a[1] =       2.0025;
  row.a[2] =       1.9976;
  row.a[3] =       0.5995;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            2;// 7p3GeV_fixedTarget_2020
  row.a[0] =     -0.65367;
  row.a[1] =     -0.36878;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
