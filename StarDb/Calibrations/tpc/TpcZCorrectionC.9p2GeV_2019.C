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
  row.npar =            2;// 9p2GeV_2019
  row.a[0] =   -0.0022061;
  row.a[1] =   1.0882e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =     -0.13342;
  row.a[1] =    0.0074237;
  row.a[2] =   -0.0001149;
  row.a[3] =   6.6209e-07;
  row.a[4] =  -1.2892e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
