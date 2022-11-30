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
  row.npar =            4;// 5p75GeV_fixedTarget_2020
  row.a[0] =     0.041818;
  row.a[1] =      0.10395;
  row.a[2] =      0.36079;
  row.a[3] =      0.36351;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 5p75GeV_fixedTarget_2020
  row.a[0] =      0.23516;
  row.a[1] =      0.31015;
  row.a[2] =      0.52841;
  row.a[3] =       0.5463;
  row.a[4] =      0.15397;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 5p75GeV_fixedTarget_2020
  row.a[0] =       1.0139;
  row.a[1] =       3.1101;
  row.a[2] =       2.8893;
  row.a[3] =      0.83598;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// 5p75GeV_fixedTarget_2020
  row.a[0] =     -0.54773;
  row.a[1] =     -0.30733;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
