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
  row.a[0] =    0.0070038;
  row.a[1] =     -0.15311;
  row.a[2] =     -0.20737;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            2;// 7.3GeV_fixedTarget_2019
  row.a[0] =        0.266;
  row.a[1] =      0.22496;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 7.3GeV_fixedTarget_2019
  row.a[0] =      0.47863;
  row.a[1] =       1.7877;
  row.a[2] =       1.8565;
  row.a[3] =      0.58057;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 7.3GeV_fixedTarget_2019
  row.a[0] =      -2.8262;
  row.a[1] =      -3.0154;
  row.a[2] =      -0.7876;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
