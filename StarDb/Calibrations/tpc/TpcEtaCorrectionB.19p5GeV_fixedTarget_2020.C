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
  row.npar =            4;// 19p5GeV_fixedTarget_2020
  row.a[0] =     0.030264;
  row.a[1] =      0.03392;
  row.a[2] =      0.25465;
  row.a[3] =       0.3214;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            3;// 19p5GeV_fixedTarget_2020
  row.a[0] =      0.19259;
  row.a[1] =     0.019234;
  row.a[2] =     -0.11376;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 19p5GeV_fixedTarget_2020
  row.a[0] =      0.73794;
  row.a[1] =       2.4656;
  row.a[2] =       2.3851;
  row.a[3] =      0.70328;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            2;// 19p5GeV_fixedTarget_2020
  row.a[0] =     -0.57585;
  row.a[1] =     -0.31911;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
