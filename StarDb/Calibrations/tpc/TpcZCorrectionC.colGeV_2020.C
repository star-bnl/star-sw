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
  row.npar =            5;// colGeV_2020
  row.a[0] =     0.069296;
  row.a[1] =  -0.00033506;
  row.a[2] =  -3.5932e-06;
  row.a[3] =   2.2614e-08;
  row.a[4] =  -4.5103e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// colGeV_2020
  row.a[0] =     0.066804;
  row.a[1] =    0.0047311;
  row.a[2] =  -7.4856e-05;
  row.a[3] =    3.308e-07;
  row.a[4] =  -4.5676e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
