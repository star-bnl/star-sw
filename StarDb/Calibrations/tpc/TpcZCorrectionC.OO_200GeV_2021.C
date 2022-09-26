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
  row.npar =            2;// OO_200GeV_2021
  row.a[0] =    -0.053578;
  row.a[1] =   0.00036176;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            4;// OO_200GeV_2021
  row.a[0] =      0.15834;
  row.a[1] =   -0.0054502;
  row.a[2] =   4.4321e-05;
  row.a[3] =  -9.7022e-08;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
