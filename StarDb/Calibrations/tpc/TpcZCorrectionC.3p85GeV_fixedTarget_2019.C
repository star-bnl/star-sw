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
  row.npar =            3;// 3p85GeV_fixedTarget_2019
  row.a[0] =     0.016048;
  row.a[1] =   0.00011935;
  row.a[2] =  -1.9851e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 3p85GeV_fixedTarget_2019
  row.a[0] =      0.21778;
  row.a[1] =   -0.0029874;
  row.a[2] =   4.4265e-05;
  row.a[3] =  -5.4672e-07;
  row.a[4] =   2.7438e-09;
  row.a[5] =  -4.7259e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 3p85GeV_fixedTarget_2019
  row.a[0] =    -0.014622;
  row.a[1] =   0.00032373;
  row.a[2] =  -1.4899e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 3p85GeV_fixedTarget_2019
  row.a[0] =       4.6453;
  row.a[1] =     -0.17431;
  row.a[2] =    0.0026146;
  row.a[3] =  -1.9391e-05;
  row.a[4] =   7.0558e-08;
  row.a[5] =  -1.0052e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
