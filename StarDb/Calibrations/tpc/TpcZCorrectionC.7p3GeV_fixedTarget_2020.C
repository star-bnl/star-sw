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
  row.npar =            6;// 7p3GeV_fixedTarget_2020
  row.a[0] =   -0.0099103;
  row.a[1] =    0.0017169;
  row.a[2] =  -4.2136e-05;
  row.a[3] =   4.1244e-07;
  row.a[4] =  -1.7861e-09;
  row.a[5] =    2.726e-12;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 2;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =    -0.056191;
  row.a[1] =     0.012428;
  row.a[2] =  -0.00040578;
  row.a[3] =   5.9436e-06;
  row.a[4] =  -4.4607e-08;
  row.a[5] =   1.6389e-10;
  row.a[6] =  -2.3369e-13;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 3;
  row.nrows = nrows;
  row.min =  22.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =    -0.024152;
  row.a[1] =    0.0033106;
  row.a[2] =  -8.7796e-05;
  row.a[3] =   1.1795e-06;
  row.a[4] =  -8.7065e-09;
  row.a[5] =    3.262e-11;
  row.a[6] =  -4.8206e-14;
  tableSet->AddAt(&row);
  memset(&row,0,tableSet->GetRowSize());
  row.idx   = 4;
  row.nrows = nrows;
  row.min =  16.5;
  row.max = 208.0;
  row.npar =            7;// 7p3GeV_fixedTarget_2020
  row.a[0] =     -0.12076;
  row.a[1] =     0.015802;
  row.a[2] =  -0.00041191;
  row.a[3] =    5.499e-06;
  row.a[4] =  -4.1182e-08;
  row.a[5] =   1.5888e-10;
  row.a[6] =  -2.4287e-13;
  tableSet->AddAt(&row);
  return (TDataSet *)tableSet;
}
