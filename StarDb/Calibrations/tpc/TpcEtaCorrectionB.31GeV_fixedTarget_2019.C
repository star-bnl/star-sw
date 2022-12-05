TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =  -0.1;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =   -0.0067979;
  row.a[1] =     -0.20938;
  row.a[2] =     -0.24911;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            8;// 31GeV_fixedTarget_2019
  row.a[0] =      0.17393;
  row.a[1] =     -0.79682;
  row.a[2] =      -3.4716;
  row.a[3] =      -5.3023;
  row.a[4] =      -3.1398;
  row.a[5] =       0.4105;
  row.a[6] =       1.0981;
  row.a[7] =      0.29381;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =     -0.73554;
  row.a[1] =      -1.1331;
  row.a[2] =     -0.42583;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =      -2.5109;
  row.a[1] =      -2.6547;
  row.a[2] =     -0.68959;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
