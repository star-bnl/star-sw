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
  row.npar =            3;// COLGeV_2020
  row.a[0] =    0.0091553;
  row.a[1] =   0.00021621;
  row.a[2] =  -1.7333e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2020
  row.a[0] =     0.020945;
  row.a[1] =    0.0038911;
  row.a[2] =  -5.6596e-05;
  row.a[3] =    2.945e-07;
  row.a[4] =  -5.3818e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// COLGeV_2020
  row.a[0] =    0.0092927;
  row.a[1] =   0.00022018;
  row.a[2] =  -1.8702e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// COLGeV_2020
  row.a[0] =     0.028965;
  row.a[1] =    0.0041052;
  row.a[2] =  -6.0736e-05;
  row.a[3] =   3.1097e-07;
  row.a[4] =  -5.5077e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
