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
  row.npar =            3;// 9p2GeV_2019
  row.a[0] =    0.0070491;
  row.a[1] =   -1.965e-05;
  row.a[2] =  -2.1272e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeV_2019
  row.a[0] =    -0.043788;
  row.a[1] =    0.0038179;
  row.a[2] =   -7.465e-05;
  row.a[3] =   5.9398e-07;
  row.a[4] =  -2.0914e-09;
  row.a[5] =    2.635e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeV_2019
  row.a[0] =     0.010759;
  row.a[1] =  -5.0278e-05;
  row.a[2] =  -2.1376e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeV_2019
  row.a[0] =    -0.055796;
  row.a[1] =    0.0050506;
  row.a[2] =  -9.7602e-05;
  row.a[3] =   7.7268e-07;
  row.a[4] =  -2.7542e-09;
  row.a[5] =   3.5882e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
