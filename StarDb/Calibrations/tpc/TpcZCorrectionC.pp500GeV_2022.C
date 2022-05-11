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
  row.npar =            3;// pp500GeV_2022
  row.a[0] =     0.014328;
  row.a[1] =  -0.00031223;
  row.a[2] =   1.4227e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// pp500GeV_2022
  row.a[0] =    -0.020003;
  row.a[1] =   -0.0014235;
  row.a[2] =   6.0929e-05;
  row.a[3] =  -7.4962e-07;
  row.a[4] =   3.6799e-09;
  row.a[5] =  -6.2375e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// pp500GeV_2022
  row.a[0] =     0.013805;
  row.a[1] =  -0.00028039;
  row.a[2] =   1.1364e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// pp500GeV_2022
  row.a[0] =    -0.002625;
  row.a[1] =   -0.0030006;
  row.a[2] =   0.00010097;
  row.a[3] =  -1.1646e-06;
  row.a[4] =   5.5606e-09;
  row.a[5] =  -9.3677e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
