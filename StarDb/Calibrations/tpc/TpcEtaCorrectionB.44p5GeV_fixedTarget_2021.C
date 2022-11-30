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
  row.npar =            3;// 44p5GeV_fixedTarget_2021
  row.a[0] =    -0.026601;
  row.a[1] =     -0.25492;
  row.a[2] =     -0.26788;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 44p5GeV_fixedTarget_2021
  row.a[0] =      0.23614;
  row.a[1] =      0.31735;
  row.a[2] =      0.53235;
  row.a[3] =      0.54303;
  row.a[4] =      0.15296;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 44p5GeV_fixedTarget_2021
  row.a[0] =      0.82356;
  row.a[1] =       2.5933;
  row.a[2] =       2.4726;
  row.a[3] =      0.73435;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 44p5GeV_fixedTarget_2021
  row.a[0] =      -2.1634;
  row.a[1] =      -2.2086;
  row.a[2] =     -0.55193;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
