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
  row.npar =            3;// 11p5GeV_2020
  row.a[0] =     0.010279;
  row.a[1] =   0.00021344;
  row.a[2] =  -1.7961e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  40.0;
  row.max = 208.0;
  row.npar =            3;// 11p5GeV_2020
  row.a[0] =      0.16375;
  row.a[1] =  -0.00091064;
  row.a[2] =   1.1436e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
