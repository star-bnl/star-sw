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
  row.npar =            5;// OO_200GeV_2021
  row.a[0] =     0.035312;
  row.a[1] =   3.8581e-06;
  row.a[2] =  -1.9961e-06;
  row.a[3] =   1.2377e-09;
  row.a[4] =   8.8305e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// OO_200GeV_2021
  row.a[0] =    -0.022406;
  row.a[1] =     0.005119;
  row.a[2] =  -6.2008e-05;
  row.a[3] =   1.7908e-07;
  row.a[4] =   1.2276e-11;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
