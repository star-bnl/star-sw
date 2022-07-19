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
  row.npar =            7;// 14p5GeV_2019
  row.a[0] =    -0.031461;
  row.a[1] =    0.0024194;
  row.a[2] =  -6.1424e-05;
  row.a[3] =   7.6202e-07;
  row.a[4] =    -4.87e-09;
  row.a[5] =   1.5657e-11;
  row.a[6] =   -2.031e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 14p5GeV_2019
  row.a[0] =     -0.14368;
  row.a[1] =    0.0086619;
  row.a[2] =  -0.00014729;
  row.a[3] =   1.0732e-06;
  row.a[4] =  -3.8944e-09;
  row.a[5] =    7.895e-12;
  row.a[6] =  -8.9507e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 14p5GeV_2019
  row.a[0] =    -0.023569;
  row.a[1] =    0.0021368;
  row.a[2] =  -5.6363e-05;
  row.a[3] =   7.2929e-07;
  row.a[4] =  -4.9531e-09;
  row.a[5] =   1.7241e-11;
  row.a[6] =  -2.4504e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 14p5GeV_2019
  row.a[0] =     -0.18195;
  row.a[1] =     0.012907;
  row.a[2] =  -0.00025971;
  row.a[3] =   2.5214e-06;
  row.a[4] =  -1.4183e-08;
  row.a[5] =   4.5873e-11;
  row.a[6] =  -6.5229e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
