TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.9;
  row.max =   1.6;
  row.npar =            7;// 19GeV_2019
  row.a[0] =   0.00010508;
  row.a[1] =    -0.004218;
  row.a[2] =      0.04755;
  row.a[3] =     0.050005;
  row.a[4] =     -0.12022;
  row.a[5] =    -0.021543;
  row.a[6] =     0.035423;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.5;
  row.max =   2.0;
  row.npar =           10;// 19GeV_2019
  row.a[0] =     0.015231;
  row.a[1] =   -0.0012213;
  row.a[2] =     -0.22025;
  row.a[3] =       0.1141;
  row.a[4] =      0.38916;
  row.a[5] =     -0.17845;
  row.a[6] =     -0.24689;
  row.a[7] =      0.11097;
  row.a[8] =     0.050241;
  row.a[9] =    -0.023251;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.6;
  row.max =   0.9;
  row.npar =            5;// 19GeV_2019
  row.a[0] =   0.00050461;
  row.a[1] =    0.0044103;
  row.a[2] =     0.039817;
  row.a[3] =    -0.044731;
  row.a[4] =    -0.096019;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   1.6;
  row.npar =           10;// 19GeV_2019
  row.a[0] =    0.0063971;
  row.a[1] =  -0.00021674;
  row.a[2] =     -0.20295;
  row.a[3] =     -0.15415;
  row.a[4] =      0.34133;
  row.a[5] =      0.21257;
  row.a[6] =     -0.19812;
  row.a[7] =     -0.11149;
  row.a[8] =     0.036133;
  row.a[9] =     0.019534;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
