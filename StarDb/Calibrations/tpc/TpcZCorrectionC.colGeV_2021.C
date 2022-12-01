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
  row.a[0] =     0.038759;
  row.a[1] =  -7.4652e-06;
  row.a[2] =  -4.9999e-06;
  row.a[3] =    3.275e-08;
  row.a[4] =  -7.2051e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// colGeV_2021
  row.a[0] =     0.068165;
  row.a[1] =    0.0033885;
  row.a[2] =  -5.2882e-05;
  row.a[3] =   2.0594e-07;
  row.a[4] =  -2.1395e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
