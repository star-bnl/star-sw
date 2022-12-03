TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.5;
  row.max =   1.4;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =   -0.0036958;
  row.a[1] =   -0.0028136;
  row.a[2] =     0.044952;
  row.a[3] =      0.12347;
  row.a[4] =     -0.18173;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.9;
  row.max =   1.9;
  row.npar =           10;// 7p7GeV_2019
  row.a[0] =      0.01195;
  row.a[1] =     0.029486;
  row.a[2] =     -0.39618;
  row.a[3] =     0.032695;
  row.a[4] =      0.83755;
  row.a[5] =      -0.1309;
  row.a[6] =     -0.64185;
  row.a[7] =      0.15962;
  row.a[8] =      0.17946;
  row.a[9] =     -0.06552;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =   -0.0044376;
  row.a[1] =    0.0025755;
  row.a[2] =     0.043918;
  row.a[3] =     -0.12099;
  row.a[4] =     -0.17393;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.9;
  row.npar =            9;// 7p7GeV_2019
  row.a[0] =    0.0034098;
  row.a[1] =    -0.014659;
  row.a[2] =     -0.45559;
  row.a[3] =      -0.2097;
  row.a[4] =       1.0493;
  row.a[5] =      0.70724;
  row.a[6] =     -0.54137;
  row.a[7] =     -0.58416;
  row.a[8] =     -0.13376;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
