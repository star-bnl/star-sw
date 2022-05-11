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
  row.npar =            3;// 9p2GeVb_2020
  row.a[0] =     0.025322;
  row.a[1] =  -9.1565e-05;
  row.a[2] =   -8.072e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeVb_2020
  row.a[0] =    -0.074666;
  row.a[1] =    0.0058264;
  row.a[2] =  -9.4612e-05;
  row.a[3] =   6.6975e-07;
  row.a[4] =  -2.3769e-09;
  row.a[5] =   3.3807e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 9p2GeVb_2020
  row.a[0] =     0.028957;
  row.a[1] =  -0.00013632;
  row.a[2] =  -7.0412e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 9p2GeVb_2020
  row.a[0] =    -0.070412;
  row.a[1] =    0.0059098;
  row.a[2] =  -9.2438e-05;
  row.a[3] =   6.0014e-07;
  row.a[4] =  -1.9151e-09;
  row.a[5] =    2.459e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
