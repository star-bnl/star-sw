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
  row.npar =            2;// fixetTarget_2019
  row.a[0] =    -0.037324;
  row.a[1] =   0.00024896;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2019
  row.a[0] =     -0.13863;
  row.a[1] =    0.0032954;
  row.a[2] =  -1.8629e-05;
  row.a[3] =  -4.1563e-09;
  row.a[4] =   1.8402e-10;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2019
  row.a[0] =    -0.015907;
  row.a[1] =   0.00057238;
  row.a[2] =  -9.1545e-06;
  row.a[3] =   5.2051e-08;
  row.a[4] =  -9.0427e-11;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            5;// fixetTarget_2019
  row.a[0] =     -0.26444;
  row.a[1] =     0.010137;
  row.a[2] =   -0.0001311;
  row.a[3] =   6.6558e-07;
  row.a[4] =  -1.1345e-09;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
