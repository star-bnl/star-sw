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
  row.a[0] =     0.027323;
  row.a[1] =     -0.13008;
  row.a[2] =     -0.20212;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 44p5GeV_fixedTarget_2021
  row.a[0] =      0.23793;
  row.a[1] =      0.27932;
  row.a[2] =      0.38829;
  row.a[3] =      0.42721;
  row.a[4] =       0.1324;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 44p5GeV_fixedTarget_2021
  row.a[0] =       1.2417;
  row.a[1] =       3.6069;
  row.a[2] =       3.2578;
  row.a[3] =      0.92964;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            2;// 44p5GeV_fixedTarget_2021
  row.a[0] =     -0.54113;
  row.a[1] =     -0.29896;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
