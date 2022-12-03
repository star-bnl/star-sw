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
  row.npar =            4;// ps_OO_200GeV_2021
  row.a[0] =     0.014524;
  row.a[1] =   -0.0004036;
  row.a[2] =   4.2611e-06;
  row.a[3] =  -1.2319e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// ps_OO_200GeV_2021
  row.a[0] =    -0.070298;
  row.a[1] =    0.0049246;
  row.a[2] =  -6.4869e-05;
  row.a[3] =   2.9521e-07;
  row.a[4] =  -4.3151e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
