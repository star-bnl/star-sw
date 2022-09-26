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
  row.npar =            7;// 19GeV_2019
  row.a[0] =    -0.090581;
  row.a[1] =    0.0038753;
  row.a[2] =  -8.9053e-05;
  row.a[3] =   1.1201e-06;
  row.a[4] =  -7.3958e-09;
  row.a[5] =   2.4336e-11;
  row.a[6] =  -3.1748e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19GeV_2019
  row.a[0] =     -0.11012;
  row.a[1] =    0.0071467;
  row.a[2] =  -0.00013439;
  row.a[3] =   1.2726e-06;
  row.a[4] =  -6.8504e-09;
  row.a[5] =   2.0808e-11;
  row.a[6] =  -2.8078e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 19GeV_2019
  row.a[0] =    -0.092413;
  row.a[1] =    0.0041892;
  row.a[2] =  -9.4781e-05;
  row.a[3] =   1.1459e-06;
  row.a[4] =   -7.329e-09;
  row.a[5] =   2.3735e-11;
  row.a[6] =  -3.1068e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 19GeV_2019
  row.a[0] =     -0.06684;
  row.a[1] =    0.0049301;
  row.a[2] =   -7.101e-05;
  row.a[3] =   3.9868e-07;
  row.a[4] =   -7.784e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
