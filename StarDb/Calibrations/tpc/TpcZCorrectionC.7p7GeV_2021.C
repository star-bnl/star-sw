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
  row.npar =            5;// 7p7GeV_2021
  row.a[0] =     0.036089;
  row.a[1] =   2.3471e-05;
  row.a[2] =  -5.3644e-06;
  row.a[3] =   3.5128e-08;
  row.a[4] =  -7.7167e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2021
  row.a[0] =     0.063325;
  row.a[1] =    0.0033311;
  row.a[2] =  -5.1243e-05;
  row.a[3] =   1.9514e-07;
  row.a[4] =  -1.9095e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
