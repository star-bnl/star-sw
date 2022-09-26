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
  row.npar =            2;// ps_OO_200GeV_2021
  row.a[0] =    -0.052646;
  row.a[1] =   0.00035939;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// ps_OO_200GeV_2021
  row.a[0] =     -0.15522;
  row.a[1] =    0.0049204;
  row.a[2] =  -7.8609e-05;
  row.a[3] =   5.1994e-07;
  row.a[4] =  -1.1137e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
