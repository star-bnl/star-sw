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
  row.npar =            3;// dEdx
  row.a[0] =    0.0086348;
  row.a[1] =   1.2462e-05;
  row.a[2] =  -5.6235e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// dEdx
  row.a[0] =    -0.015869;
  row.a[1] =    0.0028543;
  row.a[2] =  -5.3274e-05;
  row.a[3] =   3.8561e-07;
  row.a[4] =  -1.2714e-09;
  row.a[5] =    1.547e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// dEdx
  row.a[0] =    0.0096932;
  row.a[1] =   2.4962e-05;
  row.a[2] =   -7.122e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// dEdx
  row.a[0] =    -0.027804;
  row.a[1] =    0.0036667;
  row.a[2] =  -6.3593e-05;
  row.a[3] =   4.2162e-07;
  row.a[4] =  -1.2366e-09;
  row.a[5] =   1.2629e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
