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
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =     -0.03328;
  row.a[1] =     -0.26054;
  row.a[2] =       -0.266;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.2;
  row.npar =            4;// 100GeV_fixedTarget_2021
  row.a[0] =      0.16517;
  row.a[1] =     -0.14296;
  row.a[2] =     -0.34642;
  row.a[3] =    -0.094922;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =     -0.86239;
  row.a[1] =      -1.2665;
  row.a[2] =     -0.45392;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -1.3;
  row.npar =            2;// 100GeV_fixedTarget_2021
  row.a[0] =     -0.62629;
  row.a[1] =     -0.36339;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
