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
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =      0.14815;
  row.a[1] =   -0.0042261;
  row.a[2] =   4.4063e-05;
  row.a[3] =  -1.9432e-07;
  row.a[4] =   3.0825e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =     -0.10555;
  row.a[1] =    0.0065846;
  row.a[2] =  -8.8302e-05;
  row.a[3] =   4.2905e-07;
  row.a[4] =  -7.0081e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
