TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcEtaCorrectionB",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  -1.2;
  row.max =  -0.5;
  row.npar =            1;// 26p5GeV_fixedTarget_2021
  row.a[0] =   -0.0056796;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.6;
  row.npar =            2;// 26p5GeV_fixedTarget_2021
  row.a[0] =      0.26654;
  row.a[1] =      0.23435;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =  -0.9;
  row.npar =            2;// 26p5GeV_fixedTarget_2021
  row.a[0] =    -0.091661;
  row.a[1] =    -0.068619;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -1.6;
  row.max =  -1.3;
  row.npar =            1;// 26p5GeV_fixedTarget_2021
  row.a[0] =    -0.038321;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
