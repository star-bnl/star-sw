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
  row.npar =            3;// 70GeV_fixedTarget_2021
  row.a[0] =   -0.0084959;
  row.a[1] =      -0.2202;
  row.a[2] =     -0.24622;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -2.0;
  row.max =  -0.1;
  row.npar =            4;// 70GeV_fixedTarget_2021
  row.a[0] =      0.16295;
  row.a[1] =       -0.176;
  row.a[2] =     -0.38714;
  row.a[3] =     -0.10796;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            4;// 70GeV_fixedTarget_2021
  row.a[0] =      0.91403;
  row.a[1] =        2.927;
  row.a[2] =       2.7693;
  row.a[3] =      0.80722;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.1;
  row.max =  -1.2;
  row.npar =            4;// 70GeV_fixedTarget_2021
  row.a[0] =       9.5782;
  row.a[1] =         18.9;
  row.a[2] =       12.012;
  row.a[3] =       2.4721;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
