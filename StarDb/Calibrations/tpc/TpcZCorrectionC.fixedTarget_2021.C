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
  row.npar =            5;// fixedTarget_2021
  row.a[0] =     -0.14284;
  row.a[1] =    0.0040967;
  row.a[2] =  -4.5323e-05;
  row.a[3] =   2.1683e-07;
  row.a[4] =  -3.6293e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2021
  row.a[0] =   -0.0083529;
  row.a[1] =  -0.00019138;
  row.a[2] =   1.4298e-05;
  row.a[3] =  -1.3844e-07;
  row.a[4] =   3.7494e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            4;// fixedTarget_2021
  row.a[0] =     -0.01102;
  row.a[1] =   0.00026904;
  row.a[2] =  -2.3025e-06;
  row.a[3] =   6.9681e-09;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixedTarget_2021
  row.a[0] =     -0.20687;
  row.a[1] =    0.0088456;
  row.a[2] =  -0.00011374;
  row.a[3] =   5.7222e-07;
  row.a[4] =  -9.9077e-10;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
