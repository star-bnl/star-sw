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
  row.a[0] =    0.0098392;
  row.a[1] =      -0.1744;
  row.a[2] =     -0.64329;
  row.a[3] =      -1.0421;
  row.a[4] =       -0.647;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            7;// 3p85GeV_fixedTarget_2021
  row.a[0] =      0.13294;
  row.a[1] =     -0.93425;
  row.a[2] =      -4.3931;
  row.a[3] =      -8.2306;
  row.a[4] =      -7.6953;
  row.a[5] =      -3.4185;
  row.a[6] =     -0.57426;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.6;
  row.npar =            4;// 3p85GeV_fixedTarget_2021
  row.a[0] =        1.073;
  row.a[1] =        3.016;
  row.a[2] =       2.6952;
  row.a[3] =      0.76843;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -0.9;
  row.npar =            5;// 3p85GeV_fixedTarget_2021
  row.a[0] =      -10.512;
  row.a[1] =      -29.318;
  row.a[2] =      -30.083;
  row.a[3] =       -13.38;
  row.a[4] =      -2.1731;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
