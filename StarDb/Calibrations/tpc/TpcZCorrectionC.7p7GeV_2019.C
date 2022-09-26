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
  row.npar =            2;// 7p7GeV_2019
  row.a[0] =    -0.033308;
  row.a[1] =   0.00013217;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 7p7GeV_2019
  row.a[0] =    -0.079879;
  row.a[1] =    0.0037063;
  row.a[2] =  -5.6567e-05;
  row.a[3] =   3.3314e-07;
  row.a[4] =  -6.7025e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
