TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2020
  row.a[0] =     -0.02186;
  row.a[1] =   0.00041471;
  row.a[2] =  -1.6635e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =    -0.065948;
  row.a[1] =    0.0049326;
  row.a[2] =   -6.629e-05;
  row.a[3] =   3.3333e-07;
  row.a[4] =  -5.8695e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
