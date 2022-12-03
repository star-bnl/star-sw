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
  row.npar =            6;// 26p5GeV_fixedTarget_2020
  row.a[0] =      0.16328;
  row.a[1] =      0.13892;
  row.a[2] =      -2.0676;
  row.a[3] =      -6.7749;
  row.a[4] =      -7.4981;
  row.a[5] =      -2.6735;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.9;
  row.max =   0.0;
  row.npar =            7;// 26p5GeV_fixedTarget_2020
  row.a[0] =      0.20345;
  row.a[1] =     -0.90193;
  row.a[2] =      -4.9091;
  row.a[3] =      -9.7297;
  row.a[4] =      -9.4833;
  row.a[5] =      -4.3953;
  row.a[6] =     -0.77366;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.9;
  row.npar =            3;// 26p5GeV_fixedTarget_2020
  row.a[0] =        -1.39;
  row.a[1] =      -2.0071;
  row.a[2] =     -0.70733;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.2;
  row.max =  -1.3;
  row.npar =            2;// 26p5GeV_fixedTarget_2020
  row.a[0] =      -0.6224;
  row.a[1] =     -0.35673;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
