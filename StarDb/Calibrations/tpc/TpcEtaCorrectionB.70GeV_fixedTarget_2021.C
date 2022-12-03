TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =  -0.1;
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =    0.0066264;
  row.a[1] =     -0.21489;
  row.a[2] =     -0.26852;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            4;// 70GeV_fixedTarget_2021
  row.a[0] =      0.18993;
  row.a[1] =    -0.080005;
  row.a[2] =     -0.29706;
  row.a[3] =    -0.082515;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 70GeV_fixedTarget_2021
  row.a[0] =      0.93736;
  row.a[1] =       2.9949;
  row.a[2] =       2.8296;
  row.a[3] =      0.82419;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =       -2.926;
  row.a[1] =      -3.1764;
  row.a[2] =     -0.84775;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
