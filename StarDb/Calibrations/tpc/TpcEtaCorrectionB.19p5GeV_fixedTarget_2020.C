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
  row.npar =            4;// 19p5GeV_fixedTarget_2020
  row.a[0] =     0.049179;
  row.a[1] =     0.079932;
  row.a[2] =      0.31726;
  row.a[3] =      0.36587;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =  -0.1;
  row.npar =            5;// 19p5GeV_fixedTarget_2020
  row.a[0] =       0.2377;
  row.a[1] =       0.3211;
  row.a[2] =       0.5294;
  row.a[3] =      0.55298;
  row.a[4] =        0.164;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 19p5GeV_fixedTarget_2020
  row.a[0] =      -1.0897;
  row.a[1] =      -1.5576;
  row.a[2] =     -0.53969;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.3;
  row.npar =            2;// 19p5GeV_fixedTarget_2020
  row.a[0] =     -0.61776;
  row.a[1] =     -0.34536;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
