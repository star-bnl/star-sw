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
  row.npar =            3;// 100GeV_fixedTarget_2021
  row.a[0] =     0.013811;
  row.a[1] =      -0.1618;
  row.a[2] =     -0.22477;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.2;
  row.npar =            4;// 100GeV_fixedTarget_2021
  row.a[0] =      0.17856;
  row.a[1] =    -0.062751;
  row.a[2] =     -0.25711;
  row.a[3] =     -0.06639;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 100GeV_fixedTarget_2021
  row.a[0] =       1.2982;
  row.a[1] =        3.769;
  row.a[2] =       3.4133;
  row.a[3] =      0.97954;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -1.3;
  row.npar =            2;// 100GeV_fixedTarget_2021
  row.a[0] =     -0.61654;
  row.a[1] =     -0.35958;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
