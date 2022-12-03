TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =   0.0;
  row.npar =            5;// fixedTarget_2021
  row.a[0] =     0.037611;
  row.a[1] =      -0.2487;
  row.a[2] =      -1.1153;
  row.a[3] =      -1.6957;
  row.a[4] =     -0.94442;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   0.0;
  row.npar =            7;// fixedTarget_2021
  row.a[0] =      0.14423;
  row.a[1] =      -1.0864;
  row.a[2] =      -5.3726;
  row.a[3] =      -10.336;
  row.a[4] =      -9.8477;
  row.a[5] =       -4.477;
  row.a[6] =     -0.77353;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.6;
  row.npar =            5;// fixedTarget_2021
  row.a[0] =    -0.079737;
  row.a[1] =     -0.94719;
  row.a[2] =      -2.3028;
  row.a[3] =      -1.9648;
  row.a[4] =     -0.54585;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -0.9;
  row.npar =            3;// fixedTarget_2021
  row.a[0] =      0.47675;
  row.a[1] =      0.70694;
  row.a[2] =      0.23724;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
