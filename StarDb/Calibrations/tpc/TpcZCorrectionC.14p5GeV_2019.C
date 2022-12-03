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
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =     0.069184;
  row.a[1] =   -0.0011151;
  row.a[2] =   6.5392e-06;
  row.a[3] =   -1.653e-08;
  row.a[4] =   1.4233e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =    -0.092224;
  row.a[1] =    0.0063496;
  row.a[2] =  -7.5784e-05;
  row.a[3] =   2.8863e-07;
  row.a[4] =  -3.0481e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
