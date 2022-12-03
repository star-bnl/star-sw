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
  row.npar =            4;// 31p2GeV_fixedTarget_2020
  row.a[0] =     0.056613;
  row.a[1] =     0.050706;
  row.a[2] =      0.18484;
  row.a[3] =      0.25559;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 31p2GeV_fixedTarget_2020
  row.a[0] =      0.22715;
  row.a[1] =       0.1201;
  row.a[2] =     0.032179;
  row.a[3] =      0.11364;
  row.a[4] =     0.035887;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =  -0.9;
  row.npar =            4;// 31p2GeV_fixedTarget_2020
  row.a[0] =      0.82936;
  row.a[1] =       2.8863;
  row.a[2] =       2.8437;
  row.a[3] =      0.84743;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// 31p2GeV_fixedTarget_2020
  row.a[0] =     -0.65735;
  row.a[1] =     -0.36592;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
