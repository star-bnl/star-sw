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
  row.npar =            5;// fixedTarget_2020
  row.a[0] =   -0.0078348;
  row.a[1] =  -9.8435e-05;
  row.a[2] =   2.8662e-06;
  row.a[3] =  -2.4185e-08;
  row.a[4] =   8.2747e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2020
  row.a[0] =   -0.0011594;
  row.a[1] =  -0.00050815;
  row.a[2] =   2.1372e-05;
  row.a[3] =  -1.9813e-07;
  row.a[4] =   5.4383e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2020
  row.a[0] =    -0.016068;
  row.a[1] =    0.0010143;
  row.a[2] =  -1.4621e-05;
  row.a[3] =   7.3958e-08;
  row.a[4] =   -1.077e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2020
  row.a[0] =     -0.14114;
  row.a[1] =    0.0054347;
  row.a[2] =  -6.4287e-05;
  row.a[3] =   2.8035e-07;
  row.a[4] =  -3.6212e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
