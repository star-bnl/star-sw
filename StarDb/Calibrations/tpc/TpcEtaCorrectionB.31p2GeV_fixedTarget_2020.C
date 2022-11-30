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
  row.npar =            5;// 31p2GeV_fixedTarget_2020
  row.a[0] =     0.014483;
  row.a[1] =     -0.23372;
  row.a[2] =     -0.70963;
  row.a[3] =        -0.97;
  row.a[4] =     -0.58421;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 31p2GeV_fixedTarget_2020
  row.a[0] =      0.21273;
  row.a[1] =     0.045642;
  row.a[2] =     -0.10388;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 31p2GeV_fixedTarget_2020
  row.a[0] =     -0.92504;
  row.a[1] =      -1.3087;
  row.a[2] =     -0.44626;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            3;// 31p2GeV_fixedTarget_2020
  row.a[0] =       -2.996;
  row.a[1] =      -3.0741;
  row.a[2] =     -0.77227;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
