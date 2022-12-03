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
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =   0.00015417;
  row.a[1] =     -0.18658;
  row.a[2] =      -0.2351;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =      0.26945;
  row.a[1] =      0.18503;
  row.a[2] =    -0.040023;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =      -1.0194;
  row.a[1] =      -1.5114;
  row.a[2] =     -0.54789;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            4;// 31GeV_fixedTarget_2019
  row.a[0] =       6.5978;
  row.a[1] =       13.576;
  row.a[2] =       8.8453;
  row.a[3] =       1.8467;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
