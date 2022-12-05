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
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =   -0.0039429;
  row.a[1] =     -0.26266;
  row.a[2] =      -0.7959;
  row.a[3] =      -1.1545;
  row.a[4] =     -0.68185;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =      0.22335;
  row.a[1] =     0.078827;
  row.a[2] =    -0.090286;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =      -1.3232;
  row.a[1] =      -1.9586;
  row.a[2] =     -0.71104;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =      -2.8997;
  row.a[1] =       -2.953;
  row.a[2] =     -0.73547;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
