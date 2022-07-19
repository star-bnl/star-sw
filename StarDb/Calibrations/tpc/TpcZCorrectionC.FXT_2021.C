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
  row.npar =            7;// FXT_2021
  row.a[0] =   -0.0066751;
  row.a[1] =    0.0005605;
  row.a[2] =  -9.9895e-06;
  row.a[3] =   4.7187e-08;
  row.a[4] =   3.5289e-10;
  row.a[5] =  -3.6056e-12;
  row.a[6] =   7.6253e-15;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =     -0.32836;
  row.a[1] =     0.028728;
  row.a[2] =  -0.00078316;
  row.a[3] =   1.0215e-05;
  row.a[4] =  -6.9809e-08;
  row.a[5] =   2.3897e-10;
  row.a[6] =  -3.2328e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =    -0.021305;
  row.a[1] =    0.0023198;
  row.a[2] =  -5.1513e-05;
  row.a[3] =   6.0193e-07;
  row.a[4] =  -4.0721e-09;
  row.a[5] =    1.462e-11;
  row.a[6] =  -2.1229e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// FXT_2021
  row.a[0] =     -0.10906;
  row.a[1] =     0.013145;
  row.a[2] =  -0.00033162;
  row.a[3] =   4.3839e-06;
  row.a[4] =  -3.2796e-08;
  row.a[5] =   1.2612e-10;
  row.a[6] =  -1.9122e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
