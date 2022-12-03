TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.3;
  row.max =   1.2;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =    -0.016811;
  row.a[1] =    -0.019937;
  row.a[2] =      0.17108;
  row.a[3] =      0.11834;
  row.a[4] =     -0.27418;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -0.5;
  row.max =   1.8;
  row.npar =            7;// AuAu200GeV_2019
  row.a[0] =   -0.0054152;
  row.a[1] =     0.026093;
  row.a[2] =     -0.41595;
  row.a[3] =     0.045823;
  row.a[4] =       1.3641;
  row.a[5] =      -1.3563;
  row.a[6] =      0.36634;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.3;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =    -0.015112;
  row.a[1] =   -0.0037211;
  row.a[2] =      0.10169;
  row.a[3] =     -0.21815;
  row.a[4] =     -0.34744;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =   0.6;
  row.npar =            7;// AuAu200GeV_2019
  row.a[0] =    -0.016247;
  row.a[1] =    -0.032716;
  row.a[2] =     -0.43451;
  row.a[3] =     -0.21189;
  row.a[4] =       1.1634;
  row.a[5] =       1.2742;
  row.a[6] =      0.35652;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
