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
  row.npar =            2;// 7p7GeV_2019
  row.a[0] =   -0.0066893;
  row.a[1] =   6.5139e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =     -0.14674;
  row.a[1] =    0.0078817;
  row.a[2] =  -0.00012532;
  row.a[3] =    7.396e-07;
  row.a[4] =  -1.4634e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
