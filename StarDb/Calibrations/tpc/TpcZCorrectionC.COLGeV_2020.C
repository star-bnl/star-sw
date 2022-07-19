TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// COLGeV_2020
  row.a[0] =     0.030051;
  row.a[1] =  -0.00017417;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2020
  row.a[0] =    -0.074427;
  row.a[1] =    0.0061335;
  row.a[2] =  -8.5569e-05;
  row.a[3] =   4.2775e-07;
  row.a[4] =  -7.4643e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// COLGeV_2020
  row.a[0] =     0.035954;
  row.a[1] =  -0.00022256;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2020
  row.a[0] =    -0.068169;
  row.a[1] =    0.0067817;
  row.a[2] =  -9.6756e-05;
  row.a[3] =   4.8407e-07;
  row.a[4] =  -8.3811e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
