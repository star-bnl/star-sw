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
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =   0.00053227;
  row.a[1] =  -0.00057768;
  row.a[2] =   1.2642e-05;
  row.a[3] =  -8.8635e-08;
  row.a[4] =   4.8263e-10;
  row.a[5] =  -2.5247e-12;
  row.a[6] =   5.0428e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =     0.096241;
  row.a[1] =   -0.0010957;
  row.a[2] =    3.462e-05;
  row.a[3] =  -3.4935e-07;
  row.a[4] =   1.9819e-10;
  row.a[5] =   6.7191e-12;
  row.a[6] =  -1.6691e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =    -0.015284;
  row.a[1] =   0.00069842;
  row.a[2] =   6.5964e-07;
  row.a[3] =  -9.0279e-08;
  row.a[4] =   5.4895e-10;
  row.a[5] =  -8.3374e-13;
  row.a[6] =   -7.519e-16;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 44p5GeV_fixedTarget_2021
  row.a[0] =     -0.56045;
  row.a[1] =     0.049415;
  row.a[2] =   -0.0013495;
  row.a[3] =   1.8294e-05;
  row.a[4] =  -1.3246e-07;
  row.a[5] =   4.8506e-10;
  row.a[6] =  -7.0347e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
