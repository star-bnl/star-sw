TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2020
  row.a[0] =     0.015127;
  row.a[1] =  -2.4087e-05;
  row.a[2] =  -2.8863e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2020
  row.a[0] =      0.18197;
  row.a[1] =   -0.0017336;
  row.a[2] =   7.3805e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2020
  row.a[0] =    0.0079903;
  row.a[1] =   0.00082101;
  row.a[2] =  -5.6606e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2020
  row.a[0] =      0.29294;
  row.a[1] =  -0.00038764;
  row.a[2] =  -5.8851e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
