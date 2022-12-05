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
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =     0.003556;
  row.a[1] =     -0.16661;
  row.a[2] =     -0.20844;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 13p5GeV_fixedTarget_2020
  row.a[0] =      0.24826;
  row.a[1] =      0.43337;
  row.a[2] =      0.81537;
  row.a[3] =      0.79542;
  row.a[4] =      0.22797;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =      -1.0219;
  row.a[1] =      -1.4593;
  row.a[2] =     -0.50633;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =      -3.2393;
  row.a[1] =      -3.3885;
  row.a[2] =     -0.87309;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
