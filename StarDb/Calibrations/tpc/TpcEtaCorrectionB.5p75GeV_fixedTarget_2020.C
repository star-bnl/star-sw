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
  row.npar =            3;// 5p75GeV_fixedTarget_2020
  row.a[0] =     0.014354;
  row.a[1] =       -0.181;
  row.a[2] =     -0.25063;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            6;// 5p75GeV_fixedTarget_2020
  row.a[0] =      0.25428;
  row.a[1] =      0.47606;
  row.a[2] =       1.0984;
  row.a[3] =       1.4563;
  row.a[4] =      0.78264;
  row.a[5] =      0.14961;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 5p75GeV_fixedTarget_2020
  row.a[0] =      -1.2779;
  row.a[1] =      -1.8315;
  row.a[2] =     -0.63966;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// 5p75GeV_fixedTarget_2020
  row.a[0] =     -0.48686;
  row.a[1] =     -0.27335;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
