TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =    -0.043921;
  row.a[1] =   0.00098335;
  row.a[2] =  -4.6195e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =     0.095049;
  row.a[1] =  -0.00024208;
  row.a[2] =  -4.6849e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =   -0.0015486;
  row.a[1] =   0.00012551;
  row.a[2] =  -9.5268e-07;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 31GeV_fixedTarget_2019
  row.a[0] =      0.27086;
  row.a[1] =   -0.0026887;
  row.a[2] =   5.3913e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
