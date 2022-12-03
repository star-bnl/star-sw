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
  row.npar =            5;// colGeV_2021
  row.a[0] =     0.037452;
  row.a[1] =   3.4052e-06;
  row.a[2] =  -5.2193e-06;
  row.a[3] =   3.4311e-08;
  row.a[4] =  -7.5123e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// colGeV_2021
  row.a[0] =      0.06186;
  row.a[1] =     0.003446;
  row.a[2] =  -5.3209e-05;
  row.a[3] =   2.0707e-07;
  row.a[4] =  -2.1518e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
