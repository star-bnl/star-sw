TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2020
  row.a[0] =     0.015392;
  row.a[1] =  -7.7004e-06;
  row.a[2] =  -7.9889e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2020
  row.a[0] =    -0.092363;
  row.a[1] =    0.0064046;
  row.a[2] =  -0.00010163;
  row.a[3] =   7.0944e-07;
  row.a[4] =  -2.4687e-09;
  row.a[5] =   3.4336e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 7p7GeV_2020
  row.a[0] =     0.016358;
  row.a[1] =   1.4935e-05;
  row.a[2] =  -9.9941e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 7p7GeV_2020
  row.a[0] =    -0.090176;
  row.a[1] =    0.0066291;
  row.a[2] =   -0.0001015;
  row.a[3] =   6.5218e-07;
  row.a[4] =  -2.0428e-09;
  row.a[5] =   2.5487e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
