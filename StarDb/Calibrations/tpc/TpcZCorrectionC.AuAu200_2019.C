TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  18.0;
  row.max = 220.0;
  row.npar =            3;// AuAu200_2019
  row.a[0] =    -0.033463;
  row.a[1] =    0.0004485;
  row.a[2] =   -1.609e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  18.0;
  row.max = 220.0;
  row.npar =            6;// AuAu200_2019
  row.a[0] =     0.094603;
  row.a[1] =   -0.0033361;
  row.a[2] =   0.00011937;
  row.a[3] =   -1.691e-06;
  row.a[4] =   9.1139e-09;
  row.a[5] =  -1.6653e-11;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
