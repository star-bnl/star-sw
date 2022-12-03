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
  row.npar =            4;// pp500GeV_2022
  row.a[0] =   -0.0066273;
  row.a[1] =   0.00049743;
  row.a[2] =  -4.5577e-06;
  row.a[3] =   1.0064e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// pp500GeV_2022
  row.a[0] =    0.0049187;
  row.a[1] =    0.0003619;
  row.a[2] =   1.2245e-05;
  row.a[3] =  -1.9374e-07;
  row.a[4] =   5.9845e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
