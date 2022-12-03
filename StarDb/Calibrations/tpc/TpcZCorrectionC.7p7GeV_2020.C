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
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =     0.019855;
  row.a[1] =   0.00034347;
  row.a[2] =  -8.5019e-06;
  row.a[3] =   5.2628e-08;
  row.a[4] =  -1.1272e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2020
  row.a[0] =     0.021284;
  row.a[1] =    0.0042536;
  row.a[2] =  -6.1751e-05;
  row.a[3] =   2.5678e-07;
  row.a[4] =  -3.2934e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
