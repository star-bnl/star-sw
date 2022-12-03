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
  row.npar =            2;// fixedTarget_2019
  row.a[0] =    -0.042229;
  row.a[1] =   0.00025263;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2019
  row.a[0] =      -0.1056;
  row.a[1] =    0.0017197;
  row.a[2] =   3.0724e-06;
  row.a[3] =  -1.2724e-07;
  row.a[4] =   4.3024e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2019
  row.a[0] =    -0.017736;
  row.a[1] =   0.00050403;
  row.a[2] =  -7.8341e-06;
  row.a[3] =   4.3407e-08;
  row.a[4] =  -7.0537e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2019
  row.a[0] =     -0.42877;
  row.a[1] =     0.014517;
  row.a[2] =  -0.00017312;
  row.a[3] =   8.3964e-07;
  row.a[4] =  -1.3988e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
