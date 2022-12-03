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
  row.npar =            4;// 9p2GeV_2019
  row.a[0] =     0.057126;
  row.a[1] =   -0.0011873;
  row.a[2] =   7.7937e-06;
  row.a[3] =  -1.7136e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =    -0.074766;
  row.a[1] =    0.0051083;
  row.a[2] =  -5.8864e-05;
  row.a[3] =   2.1594e-07;
  row.a[4] =  -2.1914e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
