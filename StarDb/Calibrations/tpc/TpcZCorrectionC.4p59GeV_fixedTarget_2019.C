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
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =    -0.036395;
  row.a[1] =    0.0024607;
  row.a[2] =  -5.8326e-05;
  row.a[3] =   6.4259e-07;
  row.a[4] =  -3.5481e-09;
  row.a[5] =   9.5849e-12;
  row.a[6] =  -1.0469e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// 4p59GeV_fixedTarget_2019
  row.a[0] =      0.12929;
  row.a[1] =   -0.0027545;
  row.a[2] =   3.0806e-05;
  row.a[3] =  -2.0347e-07;
  row.a[4] =   4.3765e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =    -0.033096;
  row.a[1] =    0.0020911;
  row.a[2] =  -4.2276e-05;
  row.a[3] =   5.0368e-07;
  row.a[4] =  -3.6771e-09;
  row.a[5] =   1.4464e-11;
  row.a[6] =  -2.2861e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.20653;
  row.a[1] =     0.018774;
  row.a[2] =  -0.00046882;
  row.a[3] =   6.0209e-06;
  row.a[4] =    -4.32e-08;
  row.a[5] =   1.6046e-10;
  row.a[6] =  -2.3767e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
