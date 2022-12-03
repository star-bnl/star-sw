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
  row.npar =            2;// 26p5GeV_fixedTarget_2021
  row.a[0] =       0.1281;
  row.a[1] =      0.16383;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  -1.8;
  row.max =  -0.6;
  row.npar =            2;// 26p5GeV_fixedTarget_2021
  row.a[0] =      0.24994;
  row.a[1] =      0.22137;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  -1.7;
  row.max =  -0.9;
  row.npar =            3;// 26p5GeV_fixedTarget_2021
  row.a[0] =     -0.29445;
  row.a[1] =     -0.37365;
  row.a[2] =     -0.11411;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  -2.5;
  row.max =   2.5;
  row.npar =            0;// 26p5GeV_fixedTarget_2021
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
