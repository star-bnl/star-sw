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
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =   -0.0038968;
  row.a[1] =     -0.17207;
  row.a[2] =     -0.20196;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =      0.21968;
  row.a[1] =      0.11207;
  row.a[2] =    -0.062009;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =  -0.9;
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.62158;
  row.a[1] =     -0.93549;
  row.a[2] =     -0.34087;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            2;// 4p59GeV_fixedTarget_2019
  row.a[0] =     -0.65563;
  row.a[1] =      -0.3791;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
