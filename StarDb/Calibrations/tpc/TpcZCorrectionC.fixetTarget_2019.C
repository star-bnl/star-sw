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
  row.npar =            3;// fixetTarget_2019
  row.a[0] =      -0.0221;
  row.a[1] =   0.00010131;
  row.a[2] =   2.3782e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2019
  row.a[0] =     0.043481;
  row.a[1] =  -0.00093592;
  row.a[2] =   5.0407e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2019
  row.a[0] =    -0.020638;
  row.a[1] =   0.00096351;
  row.a[2] =  -5.4132e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2019
  row.a[0] =      0.32056;
  row.a[1] =   -0.0012706;
  row.a[2] =  -2.1213e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
