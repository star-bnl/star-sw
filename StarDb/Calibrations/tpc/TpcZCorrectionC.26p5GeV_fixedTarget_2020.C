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
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.28852;
  row.a[1] =    0.0084892;
  row.a[2] =  -9.4422e-05;
  row.a[3] =   4.4732e-07;
  row.a[4] =  -7.3544e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.05336;
  row.a[1] =    0.0010204;
  row.a[2] =   3.4219e-06;
  row.a[3] =  -1.0178e-07;
  row.a[4] =   3.5648e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =    -0.028904;
  row.a[1] =    0.0007464;
  row.a[2] =  -7.8502e-06;
  row.a[3] =   2.1316e-08;
  row.a[4] =   3.0448e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 26p5GeV_fixedTarget_2020
  row.a[0] =     -0.22419;
  row.a[1] =    0.0068897;
  row.a[2] =  -8.3584e-05;
  row.a[3] =   4.0961e-07;
  row.a[4] =  -6.5749e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
