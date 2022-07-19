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
  row.npar =            2;// 9p2GeV_2020
  row.a[0] =     0.025868;
  row.a[1] =  -0.00018492;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 9p2GeV_2020
  row.a[0] =     -0.11076;
  row.a[1] =    0.0074663;
  row.a[2] =  -0.00010277;
  row.a[3] =   5.1741e-07;
  row.a[4] =  -9.1343e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
