TDataSet *CreateTable() {
  if (!gROOT->GetClass("St_tpcCorrection")) return 0;
  Int_t nrows = 4;
  St_tpcCorrection *tableSet = new St_tpcCorrection("TpcZCorrectionC",nrows);
  tpcCorrection_st row;
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 1;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 44p5GeV_fixedTarget_2021
  row.a[0] =      0.04258;
  row.a[1] =  -1.1838e-06;
  row.a[2] =  -2.1497e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 44p5GeV_fixedTarget_2021
  row.a[0] =      0.21302;
  row.a[1] =   -0.0012326;
  row.a[2] =   8.5586e-06;
  row.a[3] =  -2.3759e-07;
  row.a[4] =    1.491e-09;
  row.a[5] =  -2.8244e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 44p5GeV_fixedTarget_2021
  row.a[0] =     0.010866;
  row.a[1] =   0.00019253;
  row.a[2] =  -1.9229e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 44p5GeV_fixedTarget_2021
  row.a[0] =    -0.055663;
  row.a[1] =    0.0065122;
  row.a[2] =  -9.5052e-05;
  row.a[3] =   4.3608e-07;
  row.a[4] =  -3.8774e-10;
  row.a[5] =  -1.1896e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
