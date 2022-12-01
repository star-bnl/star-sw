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
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =     0.042395;
  row.a[1] =  -0.00043238;
  row.a[2] =  -3.8548e-06;
  row.a[3] =   5.3602e-08;
  row.a[4] =  -1.4797e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =     -0.04968;
  row.a[1] =    0.0042401;
  row.a[2] =  -4.7214e-05;
  row.a[3] =   1.4895e-07;
  row.a[4] =  -8.2102e-11;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
