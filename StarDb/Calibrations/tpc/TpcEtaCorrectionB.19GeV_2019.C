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
  row.npar =            5;// 19GeV_2019
  row.a[0] =   -0.0091382;
  row.a[1] =     0.020376;
  row.a[2] =     0.026621;
  row.a[3] =       0.0345;
  row.a[4] =    -0.072903;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.5;
  row.max =   2.0;
  row.npar =           10;// 19GeV_2019
  row.a[0] =   -0.0087295;
  row.a[1] =     0.030443;
  row.a[2] =     -0.27781;
  row.a[3] =       0.1047;
  row.a[4] =      0.48797;
  row.a[5] =     -0.13894;
  row.a[6] =     -0.31445;
  row.a[7] =      0.10018;
  row.a[8] =     0.068806;
  row.a[9] =    -0.026201;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.6;
  row.max =   0.9;
  row.npar =            9;// 19GeV_2019
  row.a[0] =   -0.0076649;
  row.a[1] =   -0.0065122;
  row.a[2] =     0.021473;
  row.a[3] =     -0.14111;
  row.a[4] =    -0.067667;
  row.a[5] =        0.234;
  row.a[6] =      0.04174;
  row.a[7] =     -0.16252;
  row.a[8] =     -0.06708;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   1.6;
  row.npar =           10;// 19GeV_2019
  row.a[0] =    -0.008301;
  row.a[1] =    -0.032244;
  row.a[2] =     -0.28205;
  row.a[3] =     -0.15599;
  row.a[4] =      0.47119;
  row.a[5] =      0.19418;
  row.a[6] =     -0.27572;
  row.a[7] =     -0.11126;
  row.a[8] =     0.052955;
  row.a[9] =      0.02265;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
