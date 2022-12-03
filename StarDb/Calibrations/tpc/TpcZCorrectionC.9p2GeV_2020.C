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
  row.npar =            5;// 9p2GeV_2020
  row.a[0] =     0.038487;
  row.a[1] =   0.00022143;
  row.a[2] =  -9.0853e-06;
  row.a[3] =   5.5165e-08;
  row.a[4] =  -1.1219e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2020
  row.a[0] =     0.047439;
  row.a[1] =     0.003905;
  row.a[2] =   -6.005e-05;
  row.a[3] =   2.5256e-07;
  row.a[4] =  -3.2792e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
