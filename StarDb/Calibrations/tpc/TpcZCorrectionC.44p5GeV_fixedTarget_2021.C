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
  row.npar =            4;// 44p5GeV_fixedTarget_2021
  row.a[0] =     0.010299;
  row.a[1] =  -4.4161e-05;
  row.a[2] =   1.9231e-06;
  row.a[3] =  -1.7579e-08;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =     0.026945;
  row.a[1] =    0.0051248;
  row.a[2] =  -0.00020255;
  row.a[3] =   3.2598e-06;
  row.a[4] =  -2.5711e-08;
  row.a[5] =   9.5534e-11;
  row.a[6] =  -1.3435e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =     -0.16568;
  row.a[1] =    0.0087835;
  row.a[2] =  -0.00024734;
  row.a[3] =   3.4758e-06;
  row.a[4] =  -2.5122e-08;
  row.a[5] =   9.0205e-11;
  row.a[6] =  -1.2783e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =     -0.38464;
  row.a[1] =     0.028935;
  row.a[2] =  -0.00073081;
  row.a[3] =   9.1775e-06;
  row.a[4] =  -6.2632e-08;
  row.a[5] =   2.2026e-10;
  row.a[6] =  -3.1075e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
