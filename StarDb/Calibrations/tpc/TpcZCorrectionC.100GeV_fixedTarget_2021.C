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
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =   -0.0087133;
  row.a[1] =   0.00067362;
  row.a[2] =  -4.7101e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =     0.052233;
  row.a[1] =   0.00033015;
  row.a[2] =  -7.0496e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =    -0.054642;
  row.a[1] =   0.00042989;
  row.a[2] =  -1.0157e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =      0.21495;
  row.a[1] =    -0.003043;
  row.a[2] =   9.0431e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
