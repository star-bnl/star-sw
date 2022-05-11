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
  row.npar =            3;// 5p75GeV_fixedTarget_2020
  row.a[0] =     0.070747;
  row.a[1] =  -0.00028543;
  row.a[2] =  -1.8151e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 5p75GeV_fixedTarget_2020
  row.a[0] =       0.2629;
  row.a[1] =   -0.0036736;
  row.a[2] =   5.0544e-05;
  row.a[3] =  -6.0164e-07;
  row.a[4] =   2.9738e-09;
  row.a[5] =  -5.0922e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            3;// 5p75GeV_fixedTarget_2020
  row.a[0] =     0.033978;
  row.a[1] =  -7.6112e-07;
  row.a[2] =  -1.8213e-06;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            6;// 5p75GeV_fixedTarget_2020
  row.a[0] =    -0.029945;
  row.a[1] =    0.0051487;
  row.a[2] =  -5.8398e-05;
  row.a[3] =   3.8189e-08;
  row.a[4] =   1.4637e-09;
  row.a[5] =  -4.3209e-12;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
