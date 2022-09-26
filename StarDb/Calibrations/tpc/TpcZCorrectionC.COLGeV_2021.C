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
  row.npar =            3;// COLGeV_2021
  row.a[0] =    -0.011677;
  row.a[1] =   0.00032938;
  row.a[2] =  -1.3826e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2021
  row.a[0] =    -0.016645;
  row.a[1] =    0.0040149;
  row.a[2] =  -5.4503e-05;
  row.a[3] =    2.676e-07;
  row.a[4] =  -4.5573e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// COLGeV_2021
  row.a[0] =    -0.015862;
  row.a[1] =   0.00039204;
  row.a[2] =  -1.6591e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2021
  row.a[0] =    -0.015633;
  row.a[1] =    0.0048364;
  row.a[2] =  -6.6551e-05;
  row.a[3] =   3.2975e-07;
  row.a[4] =  -5.6329e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
