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
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.073457;
  row.a[1] =    0.0062726;
  row.a[2] =  -0.00016874;
  row.a[3] =   2.1362e-06;
  row.a[4] =  -1.4023e-08;
  row.a[5] =   4.5809e-11;
  row.a[6] =  -5.9189e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =     -0.02499;
  row.a[1] =     0.011989;
  row.a[2] =  -0.00042038;
  row.a[3] =   6.3312e-06;
  row.a[4] =  -4.7621e-08;
  row.a[5] =   1.7389e-10;
  row.a[6] =  -2.4622e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.089495;
  row.a[1] =    0.0040292;
  row.a[2] =  -0.00011642;
  row.a[3] =   1.7334e-06;
  row.a[4] =  -1.3141e-08;
  row.a[5] =   4.9019e-11;
  row.a[6] =  -7.1572e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7.3GeV_fixedTarget_2019
  row.a[0] =    -0.082343;
  row.a[1] =    0.0076232;
  row.a[2] =  -0.00016859;
  row.a[3] =   1.8825e-06;
  row.a[4] =   -1.245e-08;
  row.a[5] =    4.501e-11;
  row.a[6] =  -6.6577e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
