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
  row.npar =            5;// ps_OO_200GeV_2021
  row.a[0] =     0.029155;
  row.a[1] =  -0.00016255;
  row.a[2] =  -5.7115e-06;
  row.a[3] =    6.732e-08;
  row.a[4] =  -1.8599e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// ps_OO_200GeV_2021
  row.a[0] =    -0.091284;
  row.a[1] =    0.0065941;
  row.a[2] =  -0.00010145;
  row.a[3] =   5.6016e-07;
  row.a[4] =  -1.0308e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
