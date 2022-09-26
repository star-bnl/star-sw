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
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =   -0.0092921;
  row.a[1] =    0.0030469;
  row.a[2] =  -9.6447e-05;
  row.a[3] =     1.23e-06;
  row.a[4] =  -7.4727e-09;
  row.a[5] =   2.0688e-11;
  row.a[6] =  -2.0589e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =   -0.0078835;
  row.a[1] =     0.012085;
  row.a[2] =   -0.0004454;
  row.a[3] =   6.9903e-06;
  row.a[4] =  -5.4394e-08;
  row.a[5] =   2.0397e-10;
  row.a[6] =  -2.9504e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =     -0.12125;
  row.a[1] =    0.0060057;
  row.a[2] =  -0.00016007;
  row.a[3] =   2.1982e-06;
  row.a[4] =  -1.5723e-08;
  row.a[5] =   5.5962e-11;
  row.a[6] =  -7.8494e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 19p5GeV_fixedTarget_2020
  row.a[0] =    -0.077713;
  row.a[1] =    0.0081072;
  row.a[2] =  -0.00018283;
  row.a[3] =   2.1493e-06;
  row.a[4] =  -1.5253e-08;
  row.a[5] =   5.8737e-11;
  row.a[6] =  -9.1161e-14;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
