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
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =      0.01504;
  row.a[1] =     -0.12355;
  row.a[2] =      -0.1806;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            5;// 4p59GeV_fixedTarget_2019
  row.a[0] =      0.33639;
  row.a[1] =       0.6037;
  row.a[2] =      0.68474;
  row.a[3] =      0.49978;
  row.a[4] =      0.12378;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 4p59GeV_fixedTarget_2019
  row.a[0] =      0.87439;
  row.a[1] =       2.6966;
  row.a[2] =       2.5418;
  row.a[3] =       0.7493;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 4p59GeV_fixedTarget_2019
  row.a[0] =      -1.7216;
  row.a[1] =      -1.7459;
  row.a[2] =     -0.42901;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
