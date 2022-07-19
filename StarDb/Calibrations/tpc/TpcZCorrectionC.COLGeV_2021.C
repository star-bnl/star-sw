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
  row.npar =            2;// COLGeV_2021
  row.a[0] =     0.021926;
  row.a[1] =  -0.00010694;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2021
  row.a[0] =    -0.066745;
  row.a[1] =    0.0059675;
  row.a[2] =  -8.2296e-05;
  row.a[3] =   3.9986e-07;
  row.a[4] =  -6.7354e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// COLGeV_2021
  row.a[0] =     0.025804;
  row.a[1] =  -0.00013741;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2021
  row.a[0] =    -0.084271;
  row.a[1] =    0.0076011;
  row.a[2] =  -0.00010707;
  row.a[3] =    5.307e-07;
  row.a[4] =  -9.0343e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
