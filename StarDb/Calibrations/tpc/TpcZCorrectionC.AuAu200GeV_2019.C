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
  row.npar =            2;// AuAu200GeV_2019
  row.a[0] =   -0.0099936;
  row.a[1] =   0.00016703;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// AuAu200GeV_2019
  row.a[0] =     -0.18182;
  row.a[1] =    0.0099505;
  row.a[2] =  -0.00015935;
  row.a[3] =   9.4504e-07;
  row.a[4] =  -1.8332e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
