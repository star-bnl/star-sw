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
  row.a[0] =     0.067535;
  row.a[1] =  -0.00029169;
  row.a[2] =  -4.3153e-06;
  row.a[3] =   2.7411e-08;
  row.a[4] =  -5.5164e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// colGeV_2020
  row.a[0] =     0.060889;
  row.a[1] =    0.0047095;
  row.a[2] =  -7.3647e-05;
  row.a[3] =     3.22e-07;
  row.a[4] =  -4.3749e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
