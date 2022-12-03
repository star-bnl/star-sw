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
  row.npar =            5;// 14p5GeV_2019
  row.a[0] =   -0.0030409;
  row.a[1] =   0.00044263;
  row.a[2] =     0.041575;
  row.a[3] =      0.10192;
  row.a[4] =      -0.1656;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.0;
  row.max =   2.0;
  row.npar =           10;// 14p5GeV_2019
  row.a[0] =    0.0098921;
  row.a[1] =     0.014679;
  row.a[2] =     -0.38121;
  row.a[3] =      0.15483;
  row.a[4] =      0.76202;
  row.a[5] =     -0.37408;
  row.a[6] =     -0.47782;
  row.a[7] =      0.32722;
  row.a[8] =   0.00093627;
  row.a[9] =    -0.022626;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.4;
  row.max =   0.5;
  row.npar =            6;// 14p5GeV_2019
  row.a[0] =   -0.0020048;
  row.a[1] =   -0.0075981;
  row.a[2] =     0.018325;
  row.a[3] =    -0.066149;
  row.a[4] =    -0.019912;
  row.a[5] =     0.099316;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =   1.0;
  row.npar =           10;// 14p5GeV_2019
  row.a[0] =   -0.0018338;
  row.a[1] =    -0.011255;
  row.a[2] =     -0.35872;
  row.a[3] =     -0.13009;
  row.a[4] =      0.74684;
  row.a[5] =      0.25562;
  row.a[6] =     -0.54041;
  row.a[7] =     -0.25943;
  row.a[8] =     0.065612;
  row.a[9] =      0.03685;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
