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
  row.npar =            3;// fixetTarget_2021
  row.a[0] =    -0.043927;
  row.a[1] =   0.00057975;
  row.a[2] =  -1.8727e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2021
  row.a[0] =    0.0016256;
  row.a[1] =  -0.00037676;
  row.a[2] =   1.8693e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2021
  row.a[0] =    -0.015601;
  row.a[1] =   0.00092245;
  row.a[2] =    -5.47e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// fixetTarget_2021
  row.a[0] =      0.37474;
  row.a[1] =   -0.0023263;
  row.a[2] =   1.9343e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
