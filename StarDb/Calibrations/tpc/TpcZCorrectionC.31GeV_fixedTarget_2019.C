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
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =    -0.013417;
  row.a[1] =   0.00069228;
  row.a[2] =  -4.7153e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =     0.069239;
  row.a[1] =   0.00010339;
  row.a[2] =  -6.0293e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =    -0.047818;
  row.a[1] =   0.00030317;
  row.a[2] =   -3.831e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =      0.22224;
  row.a[1] =    -0.003104;
  row.a[2] =   9.4217e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
