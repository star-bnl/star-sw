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
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =      0.19209;
  row.a[1] =   -0.0062294;
  row.a[2] =    6.854e-05;
  row.a[3] =  -3.1571e-07;
  row.a[4] =   5.2554e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// dAu200GeV_2021
  row.a[0] =    -0.061851;
  row.a[1] =    0.0050418;
  row.a[2] =  -7.5945e-05;
  row.a[3] =   4.1558e-07;
  row.a[4] =   -7.816e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
