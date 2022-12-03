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
  row.npar =            5;// 3p85GeV_fixedTarget_2021
  row.a[0] =     0.035749;
  row.a[1] =     -0.22716;
  row.a[2] =      -1.0178;
  row.a[3] =      -1.5177;
  row.a[4] =     -0.83039;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =      0.14182;
  row.a[1] =     -0.98811;
  row.a[2] =      -4.8443;
  row.a[3] =      -9.1943;
  row.a[4] =      -8.6744;
  row.a[5] =       -3.911;
  row.a[6] =     -0.67029;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.7;
  row.npar =            4;// 3p85GeV_fixedTarget_2021
  row.a[0] =       1.0843;
  row.a[1] =       3.0484;
  row.a[2] =       2.7111;
  row.a[3] =      0.76676;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -0.9;
  row.npar =            2;// 3p85GeV_fixedTarget_2021
  row.a[0] =     -0.11071;
  row.a[1] =    -0.058135;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
