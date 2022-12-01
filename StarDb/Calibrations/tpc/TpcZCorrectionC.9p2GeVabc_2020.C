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
  row.npar =            5;// 9p2GeVabc_2020
  row.a[0] =     0.044153;
  row.a[1] =   2.4815e-05;
  row.a[2] =  -6.8552e-06;
  row.a[3] =    4.468e-08;
  row.a[4] =  -9.2879e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeVabc_2020
  row.a[0] =     0.066344;
  row.a[1] =    0.0036596;
  row.a[2] =  -5.8568e-05;
  row.a[3] =   2.4634e-07;
  row.a[4] =  -3.0669e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
