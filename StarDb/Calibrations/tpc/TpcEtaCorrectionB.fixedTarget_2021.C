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
  row.npar =            5;// fixetTarget_2021
  row.a[0] =     0.012547;
  row.a[1] =     -0.18203;
  row.a[2] =     -0.69566;
  row.a[3] =      -1.1659;
  row.a[4] =     -0.74086;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            6;// fixetTarget_2021
  row.a[0] =      0.20908;
  row.a[1] =    -0.084546;
  row.a[2] =     -0.78656;
  row.a[3] =      -1.1602;
  row.a[4] =     -0.81189;
  row.a[5] =     -0.19917;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.6;
  row.npar =            4;// fixetTarget_2021
  row.a[0] =      0.94045;
  row.a[1] =       2.7734;
  row.a[2] =       2.5541;
  row.a[3] =      0.74275;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -0.9;
  row.npar =            5;// fixetTarget_2021
  row.a[0] =      -9.7884;
  row.a[1] =       -27.93;
  row.a[2] =       -29.14;
  row.a[3] =      -13.104;
  row.a[4] =      -2.1428;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
