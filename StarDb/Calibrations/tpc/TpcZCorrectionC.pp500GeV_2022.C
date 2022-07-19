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
  row.npar =            2;// pp500GeV_2022
  row.a[0] =    -0.040918;
  row.a[1] =   0.00058104;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// pp500GeV_2022
  row.a[0] =     -0.12354;
  row.a[1] =    0.0014864;
  row.a[2] =   1.3324e-05;
  row.a[3] =  -2.2453e-07;
  row.a[4] =   7.2011e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
