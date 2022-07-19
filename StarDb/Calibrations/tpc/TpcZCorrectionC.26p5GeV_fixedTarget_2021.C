TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 2;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            2;// 26p5GeV_fixedTarget_2021
  row.a[0] =   0.00085625;
  row.a[1] =  -4.8879e-05;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 26p5GeV_fixedTarget_2021
  row.a[0] =     0.076943;
  row.a[1] =  -0.00065841;
  row.a[2] =  -3.2007e-07;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
