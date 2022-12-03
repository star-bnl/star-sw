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
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =   -0.0014996;
  row.a[1] =    -0.016325;
  row.a[2] =     0.059218;
  row.a[3] =       0.1161;
  row.a[4] =     -0.17503;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.0;
  row.max =   1.9;
  row.npar =            9;// 9p2GeV_2019
  row.a[0] =      0.02277;
  row.a[1] =   -0.0090107;
  row.a[2] =     -0.43909;
  row.a[3] =       0.3203;
  row.a[4] =      0.77901;
  row.a[5] =     -0.71019;
  row.a[6] =     -0.24831;
  row.a[7] =      0.39086;
  row.a[8] =    -0.097035;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =   -0.0041185;
  row.a[1] =    0.0077941;
  row.a[2] =       0.1005;
  row.a[3] =    0.0020369;
  row.a[4] =     -0.10967;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   1.0;
  row.npar =           10;// 9p2GeV_2019
  row.a[0] =    0.0089712;
  row.a[1] =   -0.0083543;
  row.a[2] =     -0.37411;
  row.a[3] =    -0.075755;
  row.a[4] =      0.84419;
  row.a[5] =      0.21486;
  row.a[6] =     -0.65738;
  row.a[7] =     -0.27323;
  row.a[8] =      0.10752;
  row.a[9] =     0.051722;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
