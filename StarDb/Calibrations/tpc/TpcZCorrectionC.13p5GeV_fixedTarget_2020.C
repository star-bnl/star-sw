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
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =     0.014949;
  row.a[1] =   0.00020411;
  row.a[2] =  -3.0419e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =      0.10246;
  row.a[1] =  -0.00018711;
  row.a[2] =  -4.9006e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =    -0.051591;
  row.a[1] =   0.00056842;
  row.a[2] =  -1.7977e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 13p5GeV_fixedTarget_2020
  row.a[0] =      0.10139;
  row.a[1] =   -0.0011008;
  row.a[2] =   2.0169e-06;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
