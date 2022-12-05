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
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.022246;
  row.a[1] =     -0.19223;
  row.a[2] =      -0.2004;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =      0.16676;
  row.a[1] =     0.048675;
  row.a[2] =    -0.074348;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 7.3GeV_fixedTarget_2019
  row.a[0] =      0.76776;
  row.a[1] =       2.4493;
  row.a[2] =       2.3638;
  row.a[3] =      0.71081;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =      -2.5413;
  row.a[1] =      -2.7489;
  row.a[2] =     -0.73184;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
