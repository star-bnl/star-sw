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
  row.a[0] =    -0.039083;
  row.a[1] =    0.0028504;
  row.a[2] =  -7.1745e-05;
  row.a[3] =   9.0123e-07;
  row.a[4] =  -5.9088e-09;
  row.a[5] =   1.9554e-11;
  row.a[6] =   -2.603e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19GeV_2019
  row.a[0] =     -0.14219;
  row.a[1] =    0.0090763;
  row.a[2] =  -0.00015499;
  row.a[3] =   1.1895e-06;
  row.a[4] =  -4.9857e-09;
  row.a[5] =   1.2375e-11;
  row.a[6] =  -1.5359e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 19GeV_2019
  row.a[0] =    -0.042744;
  row.a[1] =    0.0031509;
  row.a[2] =  -7.6498e-05;
  row.a[3] =   9.2659e-07;
  row.a[4] =  -5.9261e-09;
  row.a[5] =   1.9383e-11;
  row.a[6] =  -2.5882e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19GeV_2019
  row.a[0] =     -0.22179;
  row.a[1] =     0.015936;
  row.a[2] =  -0.00033777;
  row.a[3] =   3.5594e-06;
  row.a[4] =  -2.1431e-08;
  row.a[5] =   7.0685e-11;
  row.a[6] =  -9.8169e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
