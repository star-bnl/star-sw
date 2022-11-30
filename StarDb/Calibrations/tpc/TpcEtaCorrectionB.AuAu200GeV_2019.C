TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.2;
  row.max =   1.3;
  row.npar =            6;// AuAu200GeV_2019
  row.a[0] =    -0.028208;
  row.a[1] =    0.0076208;
  row.a[2] =      0.42704;
  row.a[3] =      -1.2303;
  row.a[4] =        1.711;
  row.a[5] =     -0.89563;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.3;
  row.max =   1.8;
  row.npar =            7;// AuAu200GeV_2019
  row.a[0] =    -0.022399;
  row.a[1] =     0.045002;
  row.a[2] =     -0.19212;
  row.a[3] =      -1.1774;
  row.a[4] =       3.3595;
  row.a[5] =      -2.6145;
  row.a[6] =      0.63924;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.3;
  row.npar =            6;// AuAu200GeV_2019
  row.a[0] =     -0.01831;
  row.a[1] =    -0.040264;
  row.a[2] =     0.035963;
  row.a[3] =      0.33283;
  row.a[4] =       1.1239;
  row.a[5] =      0.90876;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =   0.5;
  row.npar =            8;// AuAu200GeV_2019
  row.a[0] =    -0.028649;
  row.a[1] =    -0.038728;
  row.a[2] =     -0.47154;
  row.a[3] =     -0.23427;
  row.a[4] =       1.7083;
  row.a[5] =        2.308;
  row.a[6] =       1.0478;
  row.a[7] =      0.15826;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
