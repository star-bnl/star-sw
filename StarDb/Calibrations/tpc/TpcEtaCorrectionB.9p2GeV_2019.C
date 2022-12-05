TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -0.5;
  row.max =   1.4;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =   -0.0081129;
  row.a[1] =    -0.009679;
  row.a[2] =     0.024901;
  row.a[3] =      0.22591;
  row.a[4] =     -0.24043;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.0;
  row.max =   1.9;
  row.npar =            9;// 9p2GeV_2019
  row.a[0] =   0.00060923;
  row.a[1] =     0.022421;
  row.a[2] =     -0.46662;
  row.a[3] =      0.21038;
  row.a[4] =      0.93762;
  row.a[5] =     -0.56046;
  row.a[6] =     -0.48659;
  row.a[7] =      0.46669;
  row.a[8] =    -0.099583;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            5;// 9p2GeV_2019
  row.a[0] =    -0.008962;
  row.a[1] =  -0.00060138;
  row.a[2] =     0.034033;
  row.a[3] =     -0.17035;
  row.a[4] =     -0.20637;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   1.0;
  row.npar =            9;// 9p2GeV_2019
  row.a[0] =   -0.0077048;
  row.a[1] =    -0.039017;
  row.a[2] =     -0.47407;
  row.a[3] =     -0.16048;
  row.a[4] =       1.1392;
  row.a[5] =      0.64272;
  row.a[6] =     -0.67426;
  row.a[7] =     -0.64446;
  row.a[8] =     -0.14269;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
